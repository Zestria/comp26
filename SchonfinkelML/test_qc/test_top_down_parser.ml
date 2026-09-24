[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** QuickCheck properties for the top-down parser and for the operator table it
    is driven by.

    The generator and the canonical printer are hand-written: [Common.expr] has
    no [@@deriving qcheck], and [Common.pp_expr] prints operator forms as
    [((+ a) b)], which is no canonical source. *)

open Frontend.Common
module P = Top_down.Parser
module Operators = Top_down.Operators
module Gen = QCheck.Gen
open QCheck.Iter

let binop_ops = [ "+"; "-"; "*"; "/"; "<"; ">"; "=" ]
let is_binop op = List.mem op binop_ops

let names =
  [ "x"; "y"; "z"; "f"; "g"; "h"; "n"; "k"; "r"; "fact"; "fib"; "foo"; "bar"; "accum" ]
;;

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
  | Let (Recursive, x, v, body) ->
    "let rec " ^ x ^ " = " ^ surface_pp v ^ " in " ^ surface_pp body
  | Let (NotRecursive, x, v, body) ->
    "let " ^ x ^ " = " ^ surface_pp v ^ " in " ^ surface_pp body
  | If (c, t, e) ->
    "if " ^ surface_pp c ^ " then " ^ surface_pp t ^ " else " ^ surface_pp e
  | App (App (Ident op, a), b) when is_binop op ->
    "(" ^ surface_operand a ^ " " ^ op ^ " " ^ surface_operand b ^ ")"
  | App (Ident "-", a) -> "(- " ^ surface_operand a ^ ")"
  | App (l, r) -> "(" ^ surface_operand l ^ " " ^ surface_operand r ^ ")"
;;

(* --- Tables used by the properties --- *)

let table src =
  match Operators.of_string src with
  | Ok t -> t
  | Error msg -> failwith ("the tests use an invalid operator table: " ^ msg)
;;

(* The tables below keep every operator the generator may produce and change one
   thing only, so that the top-level form is the only thing under test. *)
let ops_right_plus = table "left 4 = < >; right 6 + -; left 7 * /; prefix 9 -"
let ops_plus_tighter = table "left 4 = < >; left 9 + -; left 7 * /; prefix 9 -"
let ops_nonassoc_eq = table "nonassoc 4 = < >; left 6 + -; left 7 * /; prefix 9 -"

(* --- Generator --- *)

let gen_name = Gen.oneof (List.map Gen.return names)
let gen_op = Gen.oneof (List.map Gen.return binop_ops)
let gen_int = Gen.int_range 0 20

let rec gen_expr n =
  if n <= 0
  then
    Gen.oneof
      [ Gen.map (fun i -> Const (LitInt i)) gen_int; Gen.map (fun s -> Ident s) gen_name ]
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

(* A table is a sequence of words, which is what [--ops] takes.  Every word
   carries the space that separates it from the next one, so that the
   concatenation is a well formed table whenever it parses at all. *)
let table_words =
  [ "left "
  ; "right "
  ; "nonassoc "
  ; "prefix "
  ; "+ "
  ; "- "
  ; "* "
  ; "/ "
  ; "= "
  ; "< "
  ; "> "
  ; "0 "
  ; "6 "
  ; "7 "
  ; "; "
  ]
;;

let gen_table_src =
  Gen.map
    (String.concat "")
    (Gen.list_size (Gen.int_range 0 12) (Gen.oneof (List.map Gen.return table_words)))
;;

let arb_table_src = QCheck.make ~print:String.escaped gen_table_src

(* Sources for the totality property.  [Frontend.Lexer] still leaves the process
   on a character it does not know and on a comment that is not closed, so the
   property cannot be run over [QCheck.string].  These are words of the accepted
   language, always separated by a space -- which keeps the lexer happy, since
   no two of them can glue into a comment opener -- while still giving the
   parser plenty of nonsense to reject. *)
let lexer_words =
  [ "x"
  ; "y"
  ; "f"
  ; "1"
  ; "0"
  ; "17"
  ; "+"
  ; "-"
  ; "*"
  ; "/"
  ; "="
  ; "<"
  ; ">"
  ; "("
  ; ")"
  ; "let"
  ; "rec"
  ; "in"
  ; "if"
  ; "then"
  ; "else"
  ; "fun"
  ; "->"
  ]
;;

let gen_source =
  Gen.map
    (String.concat " ")
    (Gen.list_size (Gen.int_range 0 20) (Gen.oneof (List.map Gen.return lexer_words)))
;;

let arb_source = QCheck.make ~print:String.escaped gen_source

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
  ; QCheck.Test.make ~name:"idempotence pp/parse" arb_expr (fun e ->
      match P.parse_expr (surface_pp e) with
      | Error _ -> false
      | Ok e' -> P.parse_expr (surface_pp e') = Ok e')
  ; QCheck.Test.make
      ~name:"totality and error position bounds, lexable sources"
      arb_source
      (fun s ->
         match P.parse_expr s with
         | Ok _ -> true
         | Error err -> err.pos.pos_cnum >= 0 && err.pos.pos_cnum <= String.length s)
  ; QCheck.Test.make ~name:"* binds tighter than +" arb_triple (fun (a, b, c) ->
      P.parse_expr
        (Printf.sprintf
           "%s + %s * %s"
           (surface_operand a)
           (surface_operand b)
           (surface_operand c))
      = Ok (eadd a (emul b c)))
  ; QCheck.Test.make ~name:"- is left-associative" arb_triple (fun (a, b, c) ->
      P.parse_expr
        (Printf.sprintf
           "%s - %s - %s"
           (surface_operand a)
           (surface_operand b)
           (surface_operand c))
      = Ok (esub (esub a b) c))
  ; QCheck.Test.make ~name:"< binds tighter than +" arb_triple (fun (a, b, c) ->
      P.parse_expr
        (Printf.sprintf
           "%s < %s + %s"
           (surface_operand a)
           (surface_operand b)
           (surface_operand c))
      = Ok (elt a (eadd b c)))
    (* The same source reads differently under another table. *)
  ; QCheck.Test.make
      ~name:"table: right 6 + is right-associative"
      arb_triple
      (fun (a, b, c) ->
         P.parse_expr
           ~ops:ops_right_plus
           (Printf.sprintf
              "%s + %s + %s"
              (surface_operand a)
              (surface_operand b)
              (surface_operand c))
         = Ok (eadd a (eadd b c)))
  ; QCheck.Test.make
      ~name:"table: + can be made tighter than *"
      arb_triple
      (fun (a, b, c) ->
         P.parse_expr
           ~ops:ops_plus_tighter
           (Printf.sprintf
              "%s + %s * %s"
              (surface_operand a)
              (surface_operand b)
              (surface_operand c))
         = Ok (emul (eadd a b) c))
  ; QCheck.Test.make
      ~name:"table: nonassoc 4 = rejects chaining"
      arb_triple
      (fun (a, b, c) ->
         match
           P.parse_expr
             ~ops:ops_nonassoc_eq
             (Printf.sprintf
                "%s = %s = %s"
                (surface_operand a)
                (surface_operand b)
                (surface_operand c))
         with
         | Error _ -> true
         | Ok _ -> false)
  ; QCheck.Test.make
      ~name:"table: of_string is total, pp round-trips"
      arb_table_src
      (fun src ->
         match Operators.of_string src with
         | Error _ -> true
         | Ok t -> Operators.of_string (Operators.string_of t) = Ok t)
  ]
;;

let () = exit (QCheck_runner.run_tests ~verbose:true tests)
