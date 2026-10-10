[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Inline tests for the top-down parser: source -> AST, plus the operator
    table it is driven by.

    Every check is a [let%test], so the runner is what reports a failure and
    the predicates below only have to answer yes or no. *)

open SchonfinkelML_frontend.Common
module P = Top_down.Parser
module Operators = Top_down.Operators

(* --- The predicates the tests are written with --- *)

(* The expression a source parses to, or [false] when it does not parse. *)
let check_expr ops src expected =
  match Operators.of_string ops with
  | Ok ops ->
    (match P.parse_expr ~ops src with
     | Ok e -> e = expected
     | Error _ -> false)
  | Error _ -> false
;;

let check_error ops src =
  match Operators.of_string ops with
  | Ok ops ->
    (match P.parse_expr ~ops src with
     | Error _ -> true
     | Ok _ -> false)
  | Error msg -> false
;;

let check_table src expected =
  match Operators.of_string src with
  | Ok t -> String.equal (Operators.string_of t) expected
  | Error _ -> false
;;

let check_bad_table src =
  match Operators.of_string src with
  | Error _ -> true
  | Ok _ -> false
;;

(* printing a table and reading it back gives the same table *)
let check_round_trip src =
  match Operators.of_string src with
  | Error _ -> false
  | Ok t ->
    (match Operators.of_string (Operators.string_of t) with
     | Ok t' -> t' = t
     | Error _ -> false)
;;

(* Stress tests bounded by [?max_budget]: the parser must not raise even on
   pathological inputs; it must return an [Error] instead. *)
let does_not_raise src =
  match P.parse_expr src with
  | exception _ -> false
  | _ -> true
;;

(* --- The operator table: custom tables via [~ops] --- 
  Associativity is data: the same source reads differently. *)

let%test "left +" =
  check_expr "left 6 +" "1 + 2 + 3" (eadd (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "right +" =
  check_expr "right 6 +" "1 + 2 + 3" (eadd (econsti 1) (eadd (econsti 2) (econsti 3)))
;;

(* So is precedence: here [+] is the tighter of the two. *)
let%test "+ tighter than *" =
  check_expr
    "left 9 +; left 7 *"
    "1 + 2 * 3"
    (emul (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

(* The precedence of a prefix operator decides how much it takes. *)
let%test "a tight prefix -" =
  check_expr "prefix 9 -; left 6 +" "- a + b" (eadd (euminus (Ident "a")) (Ident "b"))
;;

let%test "a loose prefix -" =
  check_expr "prefix 3 -; left 6 +" "- a + b" (euminus (eadd (Ident "a") (Ident "b")))
;;

(* Arity is data as well: without a prefix [-] there is no unary minus. *)
let%test "no prefix minus" = check_error "left 6 -" "- a"
let%test "no infix minus" = check_error "prefix 9 -" "a - b"

(* nonassoc forbids chaining an operator with itself, and with an operator of
   the same precedence. *)
let%test "= chained as left" =
  check_expr "left 4 =" "a = b = c" (eeq (eeq (Ident "a") (Ident "b")) (Ident "c"))
;;

let%test "= chained as nonassoc" = check_error "nonassoc 4 =" "a = b = c"

let%test "position of a chained =" =
  match Operators.of_string "nonassoc 4 =" with
  | Ok ops ->
    (match P.parse_expr ~ops "a = b = c" with
     | Error e -> e.pos.pos_cnum = 6
     | Ok _ -> false)
  | Error msg -> false
;;

let%test "= and < chained" = check_error "nonassoc 4 = <" "a = b < c"

(* A table without an operator is a table without that operator. *)
let%test "no multiplication" = check_error "left 6 +" "a * b"

(* The table only describes operators: [=] of a binding is still a token. *)
let%test "the table does not touch let" =
  check_expr
    "right 6 +"
    "let x = 1 in x + x"
    (elet "x" (econsti 1) (eadd (Ident "x") (Ident "x")))
;;

(* --- The syntax of a table, as given to [--ops] --- *)

let%test "an empty table" = check_table "" ""
let%test "one group" = check_table "left 6 + -" "left 6 + -"

let%test "the default table" =
  check_table
    "left 4 = < > <= >=; left 6 + -; left 7 * /; prefix 9 -"
    (Operators.string_of Operators.default)
;;

let%test "spaces and tabs" = check_table "  left \t 6\t+  " "left 6 +"
let%test "an empty group" = check_table "left 6 +;" "left 6 +"

let%test "one spelling, two fixities" =
  check_table "prefix 9 -; left 6 -" "prefix 9 -; left 6 -"
;;

let%test "unknown fixity" = check_bad_table "fat 6 +"
let%test "no precedence" = check_bad_table "left"
let%test "no operators" = check_bad_table "left 6"
let%test "precedence is not a number" = check_bad_table "left + 6"
let%test "unknown operator" = check_bad_table "left 6 ^"
let%test "the same spelling twice" = check_bad_table "left 6 +; right 6 +"
let%test "the same spelling twice as a prefix" = check_bad_table "prefix 9 -; prefix 8 -"

(* printing a table and reading it back gives the same table *)
let%test "pp round-trip of an empty table" = check_round_trip ""
let%test "pp round-trip of left 6 +" = check_round_trip "left 6 +"
let%test "pp round-trip of prefix 9 -" = check_round_trip "prefix 9 -"

let%test "pp round-trip of the default table" =
  check_round_trip "left 4 = < >; left 6 + -; left 7 * /; prefix 9 -"
;;

let%test "pp round-trip of nonassoc 4 =" = check_round_trip "nonassoc 4 ="
let%test "pp round-trip of prefix 9 -; left 6 -" = check_round_trip "prefix 9 -; left 6 -"

(* a spelling can be looked up as an infix and as a prefix operator *)
let%test "find_infix/find_prefix on -" =
  let ops = "prefix 9 -; left 6 -" in
  match Operators.of_string ops with
  | Ok t ->
    (match Operators.find_infix t MINUS, Operators.find_prefix t MINUS with
     | Some i, Some p -> i.Operators.prec = 6 && p.Operators.prec = 9
     | _ -> false)
  | Error _ -> false
;;

let%test "lookups of an absent operator" =
  let ( let* ) = Result.bind in
  let aux () =
    let* t1 = Operators.of_string "prefix 9 -" in
    let* t2 = Operators.of_string "left 6 +" in
    match Operators.find_infix t1 PLUS, Operators.find_prefix t2 PLUS with
    | None, None -> Ok ()
    | _ -> Error ""
  in
  match aux () with
  | Ok _ -> true
  | Error _ -> false
;;
