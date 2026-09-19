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
open Ast_gen

(* --- Canonical source --- *)

let rec surface_operand e =
  match e with
  | Let _ | If _ | Abstr _ -> "(" ^ surface_pp e ^ ")"
  | _ -> surface_pp e

and surface_pp e =
  match e with
  | Ident s -> s
  | Const (LitInt n) -> string_of_int n
  | Const LitUnit -> "()"
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
let arb_table_src = QCheck.make ~print:String.escaped Ast_gen.gen_table_src
let arb_source = QCheck.make ~print:String.escaped Ast_gen.gen_source

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

let arb_expr = QCheck.make ~print:surface_pp ~shrink:shrink_expr Ast_gen.gen_expr_sized

let arb_triple =
  QCheck.make
    ~print:(fun (a, b, c) ->
      Printf.sprintf "(%s) (%s) (%s)" (surface_pp a) (surface_pp b) (surface_pp c))
    (Gen.triple Ast_gen.gen_expr_sized Ast_gen.gen_expr_sized Ast_gen.gen_expr_sized)
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
