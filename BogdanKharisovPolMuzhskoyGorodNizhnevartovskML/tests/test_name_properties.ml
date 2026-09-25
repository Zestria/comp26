(* SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend
open Test_support

let print_error = function
  | Name.Invalid_identifier text -> "Invalid_identifier " ^ print_string text
  | Name.Invalid_operator text -> "Invalid_operator " ^ print_string text
;;

let check create error accepted text =
  let expected = if accepted then Ok text else Error (error text) in
  equal
    ~print:(QCheck.Print.result print_string print_error)
    ~expected
    (Result.map Name.to_string (create text))
;;

let identifier = check Name.of_identifier (fun text -> Name.Invalid_identifier text)
let operator = check Name.of_operator (fun text -> Name.Invalid_operator text)

let properties =
  [ property
      "identifier preserves generated suffix after x"
      (strings "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'")
      (fun suffix -> identifier true ("x" ^ suffix))
  ; property
      "identifier rejects embedded whitespace"
      (strings "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'")
      (fun suffix -> identifier false ("x " ^ suffix))
  ; property
      "custom infix preserves generated symbolic suffix"
      (strings "!$%&*+-./:<=>?@^|~")
      (fun suffix -> operator true ("+" ^ suffix))
  ; property
      "infix rejects hash in generated symbolic suffix"
      (strings "!$%&*+-./:<=>?@^|~")
      (fun suffix -> operator false ("+#" ^ suffix))
  ]
;;

let () = QCheck_runner.run_tests_main properties
