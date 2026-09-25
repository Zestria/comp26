(* SPDX-License-Identifier: LGPL-3.0-or-later *)

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
  ]
;;

let () = QCheck_runner.run_tests_main properties
