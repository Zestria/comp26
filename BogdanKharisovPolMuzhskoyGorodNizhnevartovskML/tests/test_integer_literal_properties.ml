[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Test_support

let print_error (Integer_literal.Invalid_integer_literal text) =
  "Invalid_integer_literal " ^ print_string text
;;

let check accepted text =
  let expected =
    if accepted then Ok text else Error (Integer_literal.Invalid_integer_literal text)
  in
  equal
    ~print:(QCheck.Print.result print_string print_error)
    ~expected
    (Result.map Integer_literal.to_string (Integer_literal.of_string text))
;;

let properties =
  [ property
      "integer preserves decimal digits and separators"
      (strings "0123456789_")
      (fun suffix -> check true ("1" ^ suffix))
  ; property
      "negative integer preserves decimal digits and separators"
      (strings "0123456789_")
      (fun suffix -> check true ("-1" ^ suffix))
  ; property
      "integer rejects letters after decimal digits and separators"
      (strings "0123456789_")
      (fun suffix -> check false ("1" ^ suffix ^ "a"))
  ; property
      "double negation preserves literal spelling"
      (strings "0123456789_")
      (fun suffix ->
         let literal = Result.get_ok (Integer_literal.of_string ("-1" ^ suffix)) in
         equal
           ~print:Integer_literal.to_string
           ~expected:literal
           (Integer_literal.negate (Integer_literal.negate literal)))
  ]
;;

let () = QCheck_runner.run_tests_main properties
