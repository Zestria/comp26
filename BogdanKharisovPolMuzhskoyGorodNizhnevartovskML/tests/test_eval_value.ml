[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Middleend
open Middleend_test_support

let error_case name source evaluate expected accepts =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    match evaluate () with
    | Error error when accepts error -> true
    | Error error ->
      fail source ("Expected " ^ expected ^ ", got " ^ Eval_value.error_to_string error)
    | Ok value -> fail source ("Expected " ^ expected ^ ", got value " ^ value))
;;

let expression_error name source expected accepts =
  error_case name source (fun () -> Lambda_eval.observe (lower source)) expected accepts
;;

let division_error = function
  | Eval_value.Runtime Division_by_zero -> true
  | _ -> false
;;

let integer_error = function
  | Eval_value.Invalid_ir (Unexpected_value (Integer, Boolean)) -> true
  | _ -> false
;;

let field_error name index =
  error_case
    name
    (Printf.sprintf "field[%d] ((), ())" index)
    (fun () ->
       Lambda_eval.observe
         (Lambda.Field (Tuple (Constant Unit, Constant Unit, []), index)))
    "a field-bounds error"
    (function
      | Eval_value.Invalid_ir (Field_out_of_bounds (actual, 2)) -> actual = index
      | _ -> false)
;;

let cases =
  [ expression_error
      "division is a program error"
      "1 / 0"
      "a division-by-zero runtime error"
      division_error
  ; expression_error
      "arithmetic rejects a non-integer operand as invalid IR"
      "true + 1"
      "an integer operand"
      integer_error
  ; expression_error
      "a conditional rejects a non-boolean condition"
      "if 1 then 2 else 3"
      "a boolean condition"
      (function
      | Eval_value.Invalid_ir (Unexpected_value (Boolean, Integer)) -> true
      | _ -> false)
  ; expression_error
      "application rejects a non-function callee"
      "1 2"
      "a function callee"
      (function
      | Eval_value.Invalid_ir (Unexpected_value (Function_value, Integer)) -> true
      | _ -> false)
  ; error_case
      "a field requires a tuple"
      "field[0] ()"
      (fun () -> Lambda_eval.observe (Lambda.Field (Constant Unit, 0)))
      "a tuple operand"
      (function
        | Eval_value.Invalid_ir (Unexpected_value (Tuple_value, Unit_value)) -> true
        | _ -> false)
  ; field_error "a negative field returns a typed error" (-1)
  ; field_error "a field just beyond the tuple returns a typed error" 2
  ; expression_error
      "incompatible comparison operands indicate invalid IR"
      "1 = true"
      "compatible comparison operands"
      (function
      | Eval_value.Invalid_ir (Incompatible_comparison (Integer, Boolean)) -> true
      | _ -> false)
  ; expression_error
      "reading a recursive value before initialization is invalid IR"
      "let rec x = x in x"
      "an uninitialized-value error"
      (function
      | Eval_value.Invalid_ir (Uninitialized_value _) -> true
      | _ -> false)
  ; expression_error
      "a program error stops before a later IR error"
      "let _ = 1 / 0 in true + 1"
      "a division-by-zero runtime error"
      division_error
  ; expression_error
      "an IR error is not replaced by a later program error"
      "let _ = true + 1 in 1 / 0"
      "an integer operand"
      integer_error
  ]
;;

let missing_variable =
  let name = "a missing interpreter binding reports the actual identifier" in
  error_case
    name
    "missing"
    (fun () ->
       let id, _ = fresh Id.initial "missing" in
       Lambda_eval.observe (Lambda.Var id))
    "an unbound-variable error"
    (function
      | Eval_value.Invalid_ir (Unbound_variable id) -> Id.name id = "missing"
      | _ -> false)
;;

let unsupported_integer =
  let source = "1" ^ String.make 100 '0' in
  expression_error
    "host integer limits are reported as an interpreter limitation"
    source
    "an unsupported-integer error"
    (function
    | Eval_value.Unsupported_integer literal ->
      Frontend.Integer_literal.to_string literal = source
    | _ -> false)
;;

let invalid_ir_fails_test =
  let source = "true + 1" in
  QCheck.Test.make
    ~name:"the observation adapter never treats invalid IR as a program result"
    ~count:1
    QCheck.unit
    (fun () ->
       let probe =
         QCheck.Test.make_cell ~count:1 QCheck.unit (fun () ->
           let _ = observe source Lambda_eval.observe (lower source) in
           true)
       in
       match QCheck.TestResult.get_state (QCheck.Test.check_cell probe) with
       | Failed { instances = [ { msg_l = [ message ]; _ } ] } ->
         equal
           source
           "Input: true + 1\nInvalid interpreted IR: Expected integer, found boolean"
           message
       | _ ->
         fail
           source
           "Expected invalid IR to fail the test, with its source and diagnostic")
;;

let () =
  run_tests (cases @ [ missing_variable; unsupported_integer; invalid_ir_fails_test ])
;;
