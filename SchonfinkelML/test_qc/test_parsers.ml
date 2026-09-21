[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Unified QuickCheck properties for both parsers.

    The round-trip and idempotence properties are parameterised by a parser
    adapter and run against both the bottom-up (Menhir) and the top-down
    (recursive descent) engines. The remaining properties exercise features
    exclusive to the top-down parser: the operator table, its syntax, and
    position-based error reporting. *)

open Frontend.Common
module P = Top_down.Parser
module Operators = Top_down.Operators
module Gen = QCheck.Gen
open QCheck.Iter
open Ast_gen

(* --- Parser adapters --- *)

type parser =
  { name : string
  ; parse_expr : string -> (expr, unit) result
  }

let bottom_up : parser =
  { name = "bottom-up"
  ; parse_expr =
      (fun s ->
        let lexbuf = Lexing.from_string ("let x = " ^ s) in
        try
          match Frontend.Bottom_up_parser.program Frontend.Lexer.token lexbuf with
          | [ StrLet (NotRecursive, "x", e) ] -> Ok e
          | _ -> Error ()
        with
        | Frontend.Bottom_up_parser.Error -> Error ()
        | Frontend.Lexer.Lexing_error _ -> Error ())
  }
;;

let top_down : parser =
  { name = "top-down"
  ; parse_expr =
      (fun s ->
        match P.parse_expr s with
        | Ok e -> Ok e
        | Error _ -> Error ())
  }
;;

(* --- Canonical source printer --- *)

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

(* --- Arbitraries --- *)

(* One shared generator for both parsers. *)
let arb_expr = QCheck.make ~print:surface_pp ~shrink:shrink_expr gen_expr_sized

let arb_triple =
  QCheck.make
    ~print:(fun (a, b, c) ->
      Printf.sprintf "(%s) (%s) (%s)" (surface_pp a) (surface_pp b) (surface_pp c))
    (Gen.triple gen_expr_sized gen_expr_sized gen_expr_sized)
;;

let arb_source = QCheck.make ~print:String.escaped gen_source
let arb_table_src = QCheck.make ~print:String.escaped gen_table_src

(* --- Shared properties: round-trip and idempotence, both parsers --- *)

let test_roundtrip (p : parser) =
  QCheck.Test.make
    ~name:(p.name ^ " round-trip: parse ∘ pp = id")
    ~count:1000
    arb_expr
    (fun e ->
       match p.parse_expr (surface_pp e) with
       | Ok e' -> e = e'
       | Error () -> false)
;;

(* [surface_pp] is canonical: one pass through parse/pp stabilises the AST. *)
let test_idempotence (p : parser) =
  QCheck.Test.make ~name:(p.name ^ " idempotence pp/parse") arb_expr (fun e ->
    match p.parse_expr (surface_pp e) with
    | Error () -> false
    | Ok e' ->
      (match p.parse_expr (surface_pp e') with
       | Error () -> false
       | Ok e'' -> e' = e''))
;;

(* --- Top-down-specific properties: operator table --- *)

let table src =
  match Operators.of_string src with
  | Ok t -> t
  | Error msg -> failwith ("the tests use an invalid operator table: " ^ msg)
;;

let ops_right_plus = table "left 4 = < >; right 6 + -; left 7 * /; prefix 9 -"
let ops_plus_tighter = table "left 4 = < >; left 9 + -; left 7 * /; prefix 9 -"
let ops_nonassoc_eq = table "nonassoc 4 = < >; left 6 + -; left 7 * /; prefix 9 -"

(* Totality on lexable sources: the parser must not raise, and any error
   position must sit inside the input. *)
let test_totality_lexable =
  QCheck.Test.make
    ~name:"top-down totality and error position bounds, lexable sources"
    arb_source
    (fun s ->
       match P.parse_expr s with
       | Ok _ -> true
       | Error err -> err.pos.pos_cnum >= 0 && err.pos.pos_cnum <= String.length s)
;;

let test_right_plus_assoc =
  QCheck.Test.make
    ~name:"top-down table: right 6 + is right-associative"
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
;;

let test_plus_tighter_than_mul =
  QCheck.Test.make
    ~name:"top-down table: + can be made tighter than *"
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
;;

let test_nonassoc_eq =
  QCheck.Test.make
    ~name:"top-down table: nonassoc 4 = rejects chaining"
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
;;

(* [Operators.of_string] is total, and printing then reading back a table is
   the identity.  This is the property that uses [Ast_gen.gen_table_src]. *)
let test_table_roundtrip =
  QCheck.Test.make
    ~name:"top-down table: of_string is total, pp round-trips"
    arb_table_src
    (fun src ->
       match Operators.of_string src with
       | Error _ -> true
       | Ok t -> Operators.of_string (Operators.string_of t) = Ok t)
;;

(* --- Run all tests --- *)

let () =
  let tests =
    [ test_roundtrip bottom_up
    ; test_roundtrip top_down
    ; test_idempotence bottom_up
    ; test_idempotence top_down
    ; test_totality_lexable
    ; test_right_plus_assoc
    ; test_plus_tighter_than_mul
    ; test_nonassoc_eq
    ; test_table_roundtrip
    ]
  in
  exit (QCheck_runner.run_tests ~verbose:true tests)
;;
