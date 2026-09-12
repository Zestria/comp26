[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** QuickCheck properties for the top-down parser.

    The generator and the canonical printer are hand-written: [Common.expr] has
    no [@@deriving qcheck], and [Common.pp_expr] prints operator forms as
    [((+ a) b)], which is no canonical source. *)

open Frontend.Common
module P = Top_down.Parser
module Gen = QCheck.Gen
open QCheck.Iter

let binop_ops = [ "+"; "-"; "*"; "/"; "<"; "<="; ">"; ">="; "=" ]
let is_binop op = List.mem op binop_ops
let names = [ "x"; "y"; "z"; "f"; "g"; "h"; "n"; "k"; "r"; "fact"; "fib"; "foo"; "bar"; "accum" ]

(* --- Canonical source --- *)

let rec surface_operand e =
  match e with
  | Let _ | If _ | Abstr _ -> "(" ^ surface_pp e ^ ")"
  | _ -> surface_pp e

and surface_pp e =
  match e with
  | Ident s -> s
  | Const (LitInt n) -> string_of_int n
  | Abstr (x, body) -> "fun " ^ x ^ " -> " ^ surface_pp body
  | Let (Recursive, x, v, body) -> "let rec " ^ x ^ " = " ^ surface_pp v ^ " in " ^ surface_pp body
  | Let (NotRecursive, x, v, body) ->
    "let " ^ x ^ " = " ^ surface_pp v ^ " in " ^ surface_pp body
  | If (c, t, e) -> "if " ^ surface_pp c ^ " then " ^ surface_pp t ^ " else " ^ surface_pp e
  | App (App (Ident op, a), b) when is_binop op ->
    "(" ^ surface_operand a ^ " " ^ op ^ " " ^ surface_operand b ^ ")"
  | App (Ident "-", a) -> "(- " ^ surface_operand a ^ ")"
  | App (l, r) -> "(" ^ surface_operand l ^ " " ^ surface_operand r ^ ")"
;;

(* --- Glue-safe source ---

   Spaces are dropped only where the parser can restore them: keyword +
   identifier. Two identifiers would merge into one name, so application
   arguments are always parenthesised. *)

let glue_pp e =
  let b = Buffer.create 128 in
  let out s = Buffer.add_string b s in
  let rec pp e =
    match e with
    | Ident s -> out s
    | Const (LitInt n) -> out (string_of_int n)
    | Abstr (x, body) -> out "fun"; out x; out "->"; pp body
    | Let (flag, x, v, body) ->
      out (match flag with Recursive -> "letrec" | NotRecursive -> "let");
      out x;
      out "=";
      pp v;
      out "in";
      pp body
    | If (c, t, e) -> out "if"; pp c; out "then"; pp t; out "else"; pp e
    | App (App (Ident op, a), b) when is_binop op ->
      out "(";
      operand a;
      out op;
      operand b;
      out ")"
    | App (Ident "-", a) -> out "(-"; operand a; out ")"
    | App (l, r) -> out "("; operand l; out "("; pp r; out "))"
  and operand e =
    match e with
    | Let _ | If _ | Abstr _ -> out "("; pp e; out ")"
    | _ -> pp e
  in
  pp e;
  Buffer.contents b
;;

(* --- Generator --- *)

let gen_name = Gen.oneof (List.map Gen.return names)
let gen_op = Gen.oneof (List.map Gen.return binop_ops)
let gen_int = Gen.int_range 0 20

let rec gen_expr n =
  if n <= 0
  then Gen.oneof [ Gen.map (fun i -> Const (LitInt i)) gen_int; Gen.map (fun s -> Ident s) gen_name ]
  else (
    let half = n / 2 in
    let gen_sub = gen_expr half in
    Gen.oneof_weighted
      [ 1, Gen.map (fun i -> Const (LitInt i)) gen_int
      ; 2, Gen.map (fun s -> Ident s) gen_name
      ; 3, Gen.map3 (fun op a b -> eapp (eapp (Ident op) a) b) gen_op gen_sub gen_sub
      ; 3, Gen.map2 (fun a b -> eapp a b) gen_sub gen_sub
      ; 2, Gen.map3 (fun c t e -> eif c t e) gen_sub gen_sub gen_sub
      ; 2, Gen.map2 (fun x v -> elet x v (Ident x)) gen_name gen_sub
      ; 1, Gen.map2 (fun x v -> eletrec x v (Ident x)) gen_name gen_sub
      ; 2, Gen.map2 (fun x body -> elambda x body) gen_name gen_sub
      ])
;;

let gen_expr_sized = Gen.sized gen_expr

(* --- Shrinker --- *)

let rec shrink_expr e =
  match e with
  | Const _ | Ident _ -> empty
  | App (l, r) ->
    of_list [ l; r ]
    <+> (shrink_expr l >|= fun l -> App (l, r))
    <+> (shrink_expr r >|= fun r -> App (l, r))
  | Let (flag, x, v, body) ->
    of_list [ v; body ]
    <+> (shrink_expr v >|= fun v -> Let (flag, x, v, body))
    <+> (shrink_expr body >|= fun body -> Let (flag, x, v, body))
  | If (c, t, e) ->
    of_list [ c; t; e ]
    <+> (shrink_expr c >|= fun c -> If (c, t, e))
    <+> (shrink_expr t >|= fun t -> If (c, t, e))
    <+> (shrink_expr e >|= fun e -> If (c, t, e))
  | Abstr (x, body) ->
    of_list [ body ] <+> (shrink_expr body >|= fun body -> Abstr (x, body))
;;

let arb_expr = QCheck.make ~print:surface_pp ~shrink:shrink_expr gen_expr_sized

let arb_triple =
  QCheck.make
    ~print:(fun (a, b, c) ->
      Printf.sprintf "(%s) (%s) (%s)" (surface_pp a) (surface_pp b) (surface_pp c))
    (Gen.triple gen_expr_sized gen_expr_sized gen_expr_sized)
;;

(* --- Properties --- *)

let tests =
  [ QCheck.Test.make ~name:"round-trip: parse ∘ pp = id" arb_expr (fun e ->
      P.parse_expr (surface_pp e) = Ok e)
  ; QCheck.Test.make ~name:"glued: склеенный исходник" arb_expr (fun e ->
      P.parse_expr ~glued:true (glue_pp e) = Ok e)
  ; QCheck.Test.make ~name:"glued: обычный исходник читается так же" arb_expr (fun e ->
      P.parse_expr ~glued:true (surface_pp e) = Ok e)
  ; QCheck.Test.make ~name:"идемпотентность pp/parse" arb_expr (fun e ->
      match P.parse_expr (surface_pp e) with
      | Error _ -> false
      | Ok e' -> P.parse_expr (surface_pp e') = Ok e')
  ; QCheck.Test.make ~name:"тотальность и границы позиции ошибки" QCheck.string (fun s ->
      match P.parse_expr s with
      | Ok _ -> true
      | Error err -> err.pos.pos_cnum >= 0 && err.pos.pos_cnum <= String.length s)
  ; QCheck.Test.make ~name:"* связывает крепче +" arb_triple (fun (a, b, c) ->
      P.parse_expr
        (Printf.sprintf
           "%s + %s * %s"
           (surface_operand a)
           (surface_operand b)
           (surface_operand c))
      = Ok (eadd a (emul b c)))
  ; QCheck.Test.make ~name:"- левоассоциативен" arb_triple (fun (a, b, c) ->
      P.parse_expr
        (Printf.sprintf
           "%s - %s - %s"
           (surface_operand a)
           (surface_operand b)
           (surface_operand c))
      = Ok (esub (esub a b) c))
  ; QCheck.Test.make ~name:"< связывает крепче +" arb_triple (fun (a, b, c) ->
      P.parse_expr
        (Printf.sprintf
           "%s < %s + %s"
           (surface_operand a)
           (surface_operand b)
           (surface_operand c))
      = Ok (elt a (eadd b c)))
  ]
;;

let () = exit (QCheck_runner.run_tests ~verbose:true tests)
