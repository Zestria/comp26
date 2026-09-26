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

let print_kind =
  QCheck.Print.option (function
    | Name.Prefix -> "Prefix"
    | Hash -> "Hash"
    | Comparison -> "Comparison"
    | Concatenation -> "Concatenation"
    | Additive -> "Additive"
    | Multiplicative -> "Multiplicative"
    | Power -> "Power"
    | Conjunction -> "Conjunction"
    | Disjunction -> "Disjunction"
    | Assignment -> "Assignment")
;;

let custom_operator_kind =
  let generator =
    let open QCheck.Gen in
    let ( let* ) = ( >>= ) in
    let* prefix, expected =
      oneof_list
        [ "!+", Name.Prefix
        ; "~!", Prefix
        ; "?!", Prefix
        ; "#!", Hash
        ; "=", Comparison
        ; "<>", Comparison
        ; ">", Comparison
        ; "|+", Comparison
        ; "&+", Comparison
        ; "$", Comparison
        ; "@", Concatenation
        ; "^", Concatenation
        ; "+", Additive
        ; "-+", Additive
        ; "*+", Multiplicative
        ; "/", Multiplicative
        ; "%", Multiplicative
        ; "**", Power
        ]
    in
    let* suffix = strings "!$%&*+-./:<=>?@^|~" in
    return (prefix ^ suffix, expected)
  in
  QCheck.Test.make
    ~name:"custom operator suffix preserves its lexical class"
    ~count:2000
    (QCheck.make ~print:(fun (text, _) -> print_string text) generator)
    (fun (text, expected) ->
       let name = Result.get_ok (Name.of_operator text) in
       equal ~print:print_kind ~expected:(Some expected) (Name.operator_kind name))
;;

let properties =
  [ custom_operator_kind
  ; property
      "identifier preserves generated suffix after x"
      (strings "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'")
      (fun suffix -> identifier true ("x" ^ suffix))
  ; property
      "ordinary identifiers have no operator class"
      (strings "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'")
      (fun suffix ->
         let name = Result.get_ok (Name.of_identifier ("x" ^ suffix)) in
         equal ~print:print_kind ~expected:None (Name.operator_kind name))
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
