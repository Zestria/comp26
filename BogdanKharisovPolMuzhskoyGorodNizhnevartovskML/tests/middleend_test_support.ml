[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Middleend
module Lambda_eval = Lambda_eval
module Eval_value = Eval_value

let fail source message = QCheck.Test.fail_reportf "Input: %s\n%s" source message

let allocated source = function
  | Ok value -> value
  | Error error -> fail source (Id.error_to_string error)
;;

let fresh supply name =
  allocated (Printf.sprintf "allocate %S" name) (Id.fresh supply name)
;;

let rename supply id =
  allocated (Printf.sprintf "rename %S" (Id.name id)) (Id.rename supply id)
;;

let parsed parse source =
  match parse source with
  | Ok ast -> ast
  | Error error -> fail source ("Unexpected parse error: " ^ error)
;;

let invariant_error = function
  | Lambda.Unbound_variable id -> "Unbound Id: " ^ Id.name id
  | Duplicate_binding id -> "Repeated binding Id: " ^ Id.name id
  | Negative_field n -> "Negative field: " ^ string_of_int n
;;

let checked source term =
  (match Lambda.validate term with
   | Ok () -> ()
   | Error error -> fail source (invariant_error error));
  term
;;

let lower_with_supply supply source =
  match Lower.expression supply (parsed Parser.parse_expression source) with
  | Ok (term, supply) -> checked source term, supply
  | Error error -> fail source (Lower.error_to_string error)
;;

let lower source = fst (lower_with_supply Id.initial source)

let equal source expected actual =
  if expected <> actual
  then fail source (Printf.sprintf "Expected: %s\nActual: %s" expected actual);
  true
;;

(* Unexpected host exceptions remain test failures, with their original backtrace. *)
let observe source evaluate input =
  let outcome =
    try evaluate input with
    | error ->
      let backtrace = Printexc.get_raw_backtrace () in
      fail
        source
        ("Unexpected evaluation exception: "
         ^ Printexc.to_string error
         ^ "\n"
         ^ Printexc.raw_backtrace_to_string backtrace)
  in
  match outcome with
  | Ok value -> value
  | Error (Eval_value.Runtime error) -> Eval_value.runtime_error_to_string error
  | Error error -> fail source (Eval_value.error_to_string error)
;;

let run_tests tests =
  Printexc.record_backtrace true;
  QCheck_base_runner.run_tests_main tests
;;

let cps_factorial n =
  Printf.sprintf
    "let rec fact n k = if n = 0 then k 1 else fact (n - 1) (fun r -> k (n * r)) in fact \
     %d (fun x -> x)"
    n
;;

let cps_fibonacci n =
  Printf.sprintf
    "let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
     -> k (a + b))) in fib %d (fun x -> x)"
    n
;;
