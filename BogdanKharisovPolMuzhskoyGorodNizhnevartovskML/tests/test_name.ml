[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend

let print_result = function
  | Ok name -> Printf.printf "Ok %S\n" (Name.to_string name)
  | Error (Name.Invalid_identifier text) ->
    Printf.printf "Error (Invalid_identifier %S)\n" text
  | Error (Name.Invalid_operator text) ->
    Printf.printf "Error (Invalid_operator %S)\n" text
;;

module Identifier = struct
  let%expect_test "identifier accepts: first lowercase letter" =
    Name.of_identifier "a" |> print_result;
    [%expect {| Ok "a" |}]
  ;;

  let%expect_test "identifier accepts: last lowercase letter" =
    Name.of_identifier "z" |> print_result;
    [%expect {| Ok "z" |}]
  ;;

  let%expect_test "identifier accepts: underscore prefix" =
    Name.of_identifier "_x" |> print_result;
    [%expect {| Ok "_x" |}]
  ;;

  let%expect_test "identifier accepts: two underscores" =
    Name.of_identifier "__" |> print_result;
    [%expect {| Ok "__" |}]
  ;;

  let%expect_test "identifier accepts: uppercase, digits, apostrophe in tail" =
    Name.of_identifier "xAZ09_'" |> print_result;
    [%expect {| Ok "xAZ09_'" |}]
  ;;

  let%expect_test "identifier accepts: keyword prefix" =
    Name.of_identifier "letx" |> print_result;
    [%expect {| Ok "letx" |}]
  ;;

  let%expect_test "identifier accepts: word operator prefix" =
    Name.of_identifier "modx" |> print_result;
    [%expect {| Ok "modx" |}]
  ;;

  let%expect_test "identifier accepts: ordinary function not" =
    Name.of_identifier "not" |> print_result;
    [%expect {| Ok "not" |}]
  ;;

  let%expect_test "identifier accepts: class is not reserved" =
    Name.of_identifier "class" |> print_result;
    [%expect {| Ok "class" |}]
  ;;

  let%expect_test "identifier accepts: and is not reserved" =
    Name.of_identifier "and" |> print_result;
    [%expect {| Ok "and" |}]
  ;;

  let%expect_test "identifier accepts: match is not reserved" =
    Name.of_identifier "match" |> print_result;
    [%expect {| Ok "match" |}]
  ;;

  let%expect_test "identifier accepts: land is not reserved" =
    Name.of_identifier "land" |> print_result;
    [%expect {| Ok "land" |}]
  ;;

  let%expect_test "identifier rejects: empty" =
    Name.of_identifier "" |> print_result;
    [%expect {| Error (Invalid_identifier "") |}]
  ;;

  let%expect_test "identifier rejects: wildcard" =
    Name.of_identifier "_" |> print_result;
    [%expect {| Error (Invalid_identifier "_") |}]
  ;;

  let%expect_test "identifier rejects: uppercase initial" =
    Name.of_identifier "X" |> print_result;
    [%expect {| Error (Invalid_identifier "X") |}]
  ;;

  let%expect_test "identifier rejects: digit initial" =
    Name.of_identifier "0x" |> print_result;
    [%expect {| Error (Invalid_identifier "0x") |}]
  ;;

  let%expect_test "identifier rejects: apostrophe initial" =
    Name.of_identifier "'x" |> print_result;
    [%expect {| Error (Invalid_identifier "'x") |}]
  ;;

  let%expect_test "identifier rejects: qualified name" =
    Name.of_identifier "x.y" |> print_result;
    [%expect {| Error (Invalid_identifier "x.y") |}]
  ;;

  let%expect_test "identifier rejects: operator character" =
    Name.of_identifier "x-y" |> print_result;
    [%expect {| Error (Invalid_identifier "x-y") |}]
  ;;

  let%expect_test "identifier rejects: operator name" =
    Name.of_identifier "+" |> print_result;
    [%expect {| Error (Invalid_identifier "+") |}]
  ;;

  let%expect_test "identifier rejects: leading space" =
    Name.of_identifier " x" |> print_result;
    [%expect {| Error (Invalid_identifier " x") |}]
  ;;

  let%expect_test "identifier rejects: trailing space" =
    Name.of_identifier "x " |> print_result;
    [%expect {| Error (Invalid_identifier "x ") |}]
  ;;

  let%expect_test "identifier rejects: embedded space" =
    Name.of_identifier "x y" |> print_result;
    [%expect {| Error (Invalid_identifier "x y") |}]
  ;;

  let%expect_test "identifier rejects: newline" =
    Name.of_identifier "x\n" |> print_result;
    [%expect {| Error (Invalid_identifier "x\n") |}]
  ;;

  let%expect_test "identifier rejects: NUL" =
    Name.of_identifier "x\000" |> print_result;
    [%expect {| Error (Invalid_identifier "x\000") |}]
  ;;

  let%expect_test "identifier rejects: non-ASCII" =
    Name.of_identifier "имя" |> print_result;
    [%expect {| Error (Invalid_identifier "\208\184\208\188\209\143") |}]
  ;;

  let%expect_test "identifier rejects: escaped identifier" =
    Name.of_identifier "\\#let" |> print_result;
    [%expect {| Error (Invalid_identifier "\\#let") |}]
  ;;

  let%expect_test "reserved word: else" =
    Name.of_identifier "else" |> print_result;
    [%expect {| Error (Invalid_identifier "else") |}]
  ;;

  let%expect_test "reserved word: false" =
    Name.of_identifier "false" |> print_result;
    [%expect {| Error (Invalid_identifier "false") |}]
  ;;

  let%expect_test "reserved word: fun" =
    Name.of_identifier "fun" |> print_result;
    [%expect {| Error (Invalid_identifier "fun") |}]
  ;;

  let%expect_test "reserved word: if" =
    Name.of_identifier "if" |> print_result;
    [%expect {| Error (Invalid_identifier "if") |}]
  ;;

  let%expect_test "reserved word: in" =
    Name.of_identifier "in" |> print_result;
    [%expect {| Error (Invalid_identifier "in") |}]
  ;;

  let%expect_test "reserved word: let" =
    Name.of_identifier "let" |> print_result;
    [%expect {| Error (Invalid_identifier "let") |}]
  ;;

  let%expect_test "reserved word: rec" =
    Name.of_identifier "rec" |> print_result;
    [%expect {| Error (Invalid_identifier "rec") |}]
  ;;

  let%expect_test "reserved word: then" =
    Name.of_identifier "then" |> print_result;
    [%expect {| Error (Invalid_identifier "then") |}]
  ;;

  let%expect_test "reserved word: true" =
    Name.of_identifier "true" |> print_result;
    [%expect {| Error (Invalid_identifier "true") |}]
  ;;

  let%expect_test "reserved word: mod" =
    Name.of_identifier "mod" |> print_result;
    [%expect {| Error (Invalid_identifier "mod") |}]
  ;;
end

module Operator = struct
  let%expect_test "operator accepts: word operator" =
    Name.of_operator "mod" |> print_result;
    [%expect {| Ok "mod" |}]
  ;;

  let%expect_test "operator accepts: single prefix" =
    Name.of_operator "!" |> print_result;
    [%expect {| Ok "!" |}]
  ;;

  let%expect_test "operator accepts: tilde prefix" =
    Name.of_operator "~+" |> print_result;
    [%expect {| Ok "~+" |}]
  ;;

  let%expect_test "operator accepts: question prefix" =
    Name.of_operator "?+" |> print_result;
    [%expect {| Ok "?+" |}]
  ;;

  let%expect_test "operator accepts: hash operator" =
    Name.of_operator "#+" |> print_result;
    [%expect {| Ok "#+" |}]
  ;;

  let%expect_test "operator accepts: hash after bang" =
    Name.of_operator "!#" |> print_result;
    [%expect {| Ok "!#" |}]
  ;;

  let%expect_test "operator accepts: hash after tilde" =
    Name.of_operator "~#" |> print_result;
    [%expect {| Ok "~#" |}]
  ;;

  let%expect_test "operator accepts: hash after question" =
    Name.of_operator "?#" |> print_result;
    [%expect {| Ok "?#" |}]
  ;;

  let%expect_test "operator accepts: double hash" =
    Name.of_operator "##" |> print_result;
    [%expect {| Ok "##" |}]
  ;;

  let%expect_test "operator accepts: assignment exception" =
    Name.of_operator ":=" |> print_result;
    [%expect {| Ok ":=" |}]
  ;;

  let%expect_test "operator accepts: equality" =
    Name.of_operator "=" |> print_result;
    [%expect {| Ok "=" |}]
  ;;

  let%expect_test "operator accepts: less than" =
    Name.of_operator "<" |> print_result;
    [%expect {| Ok "<" |}]
  ;;

  let%expect_test "operator accepts: greater than" =
    Name.of_operator ">" |> print_result;
    [%expect {| Ok ">" |}]
  ;;

  let%expect_test "operator accepts: disjunction" =
    Name.of_operator "||" |> print_result;
    [%expect {| Ok "||" |}]
  ;;

  let%expect_test "operator accepts: ampersand" =
    Name.of_operator "&" |> print_result;
    [%expect {| Ok "&" |}]
  ;;

  let%expect_test "operator accepts: dollar" =
    Name.of_operator "$" |> print_result;
    [%expect {| Ok "$" |}]
  ;;

  let%expect_test "operator accepts: at" =
    Name.of_operator "@" |> print_result;
    [%expect {| Ok "@" |}]
  ;;

  let%expect_test "operator accepts: caret" =
    Name.of_operator "^" |> print_result;
    [%expect {| Ok "^" |}]
  ;;

  let%expect_test "operator accepts: plus" =
    Name.of_operator "+" |> print_result;
    [%expect {| Ok "+" |}]
  ;;

  let%expect_test "operator accepts: minus" =
    Name.of_operator "-" |> print_result;
    [%expect {| Ok "-" |}]
  ;;

  let%expect_test "operator accepts: star" =
    Name.of_operator "*" |> print_result;
    [%expect {| Ok "*" |}]
  ;;

  let%expect_test "operator accepts: slash" =
    Name.of_operator "/" |> print_result;
    [%expect {| Ok "/" |}]
  ;;

  let%expect_test "operator accepts: percent" =
    Name.of_operator "%" |> print_result;
    [%expect {| Ok "%" |}]
  ;;

  let%expect_test "operator accepts: custom infix" =
    Name.of_operator "+++--+++" |> print_result;
    [%expect {| Ok "+++--+++" |}]
  ;;

  let%expect_test "operator accepts: dot in tail" =
    Name.of_operator "+." |> print_result;
    [%expect {| Ok "+." |}]
  ;;

  let%expect_test "operator accepts: colon in tail" =
    Name.of_operator "+:" |> print_result;
    [%expect {| Ok "+:" |}]
  ;;

  let%expect_test "operator rejects: empty" =
    Name.of_operator "" |> print_result;
    [%expect {| Error (Invalid_operator "") |}]
  ;;

  let%expect_test "operator rejects: bare tilde" =
    Name.of_operator "~" |> print_result;
    [%expect {| Error (Invalid_operator "~") |}]
  ;;

  let%expect_test "operator rejects: bare question" =
    Name.of_operator "?" |> print_result;
    [%expect {| Error (Invalid_operator "?") |}]
  ;;

  let%expect_test "operator rejects: bare hash" =
    Name.of_operator "#" |> print_result;
    [%expect {| Error (Invalid_operator "#") |}]
  ;;

  let%expect_test "operator rejects: arrow" =
    Name.of_operator "->" |> print_result;
    [%expect {| Error (Invalid_operator "->") |}]
  ;;

  let%expect_test "operator rejects: left arrow" =
    Name.of_operator "<-" |> print_result;
    [%expect {| Error (Invalid_operator "<-") |}]
  ;;

  let%expect_test "operator rejects: single bar" =
    Name.of_operator "|" |> print_result;
    [%expect {| Error (Invalid_operator "|") |}]
  ;;

  let%expect_test "operator rejects: colon" =
    Name.of_operator ":" |> print_result;
    [%expect {| Error (Invalid_operator ":") |}]
  ;;

  let%expect_test "operator rejects: cons" =
    Name.of_operator "::" |> print_result;
    [%expect {| Error (Invalid_operator "::") |}]
  ;;

  let%expect_test "operator rejects: dot initial" =
    Name.of_operator ".+" |> print_result;
    [%expect {| Error (Invalid_operator ".+") |}]
  ;;

  let%expect_test "operator rejects: hash in infix tail" =
    Name.of_operator "+#" |> print_result;
    [%expect {| Error (Invalid_operator "+#") |}]
  ;;

  let%expect_test "operator rejects: ordinary identifier" =
    Name.of_operator "foo" |> print_result;
    [%expect {| Error (Invalid_operator "foo") |}]
  ;;

  let%expect_test "operator rejects: function not" =
    Name.of_operator "not" |> print_result;
    [%expect {| Error (Invalid_operator "not") |}]
  ;;

  let%expect_test "operator rejects: excluded word operator" =
    Name.of_operator "land" |> print_result;
    [%expect {| Error (Invalid_operator "land") |}]
  ;;

  let%expect_test "operator rejects: word operator prefix" =
    Name.of_operator "modx" |> print_result;
    [%expect {| Error (Invalid_operator "modx") |}]
  ;;

  let%expect_test "operator rejects: letter in tail" =
    Name.of_operator "+a" |> print_result;
    [%expect {| Error (Invalid_operator "+a") |}]
  ;;

  let%expect_test "operator rejects: parentheses" =
    Name.of_operator "( + )" |> print_result;
    [%expect {| Error (Invalid_operator "( + )") |}]
  ;;

  let%expect_test "operator rejects: leading space" =
    Name.of_operator " +" |> print_result;
    [%expect {| Error (Invalid_operator " +") |}]
  ;;

  let%expect_test "operator rejects: trailing space" =
    Name.of_operator "+ " |> print_result;
    [%expect {| Error (Invalid_operator "+ ") |}]
  ;;

  let%expect_test "operator rejects: embedded space" =
    Name.of_operator "+ -" |> print_result;
    [%expect {| Error (Invalid_operator "+ -") |}]
  ;;

  let%expect_test "operator rejects: newline" =
    Name.of_operator "+\n" |> print_result;
    [%expect {| Error (Invalid_operator "+\n") |}]
  ;;

  let%expect_test "operator rejects: NUL" =
    Name.of_operator "+\000" |> print_result;
    [%expect {| Error (Invalid_operator "+\000") |}]
  ;;
end

module Error = struct
  let%expect_test "name error: empty identifier" =
    Name.error_to_string (Invalid_identifier "") |> print_endline;
    [%expect {| Invalid identifier: "" |}]
  ;;

  let%expect_test "name error: ordinary identifier" =
    Name.error_to_string (Invalid_identifier "X") |> print_endline;
    [%expect {| Invalid identifier: "X" |}]
  ;;

  let%expect_test "name error: escaped identifier" =
    Name.error_to_string (Invalid_identifier "\"\\\n\t\r\000") |> print_endline;
    [%expect {| Invalid identifier: "\"\\\n\t\r\000" |}]
  ;;

  let%expect_test "name error: empty operator" =
    Name.error_to_string (Invalid_operator "") |> print_endline;
    [%expect {| Invalid operator name: "" |}]
  ;;

  let%expect_test "name error: ordinary operator" =
    Name.error_to_string (Invalid_operator "->") |> print_endline;
    [%expect {| Invalid operator name: "->" |}]
  ;;

  let%expect_test "name error: escaped operator" =
    Name.error_to_string (Invalid_operator "\"\\\n\t\r\000") |> print_endline;
    [%expect {| Invalid operator name: "\"\\\n\t\r\000" |}]
  ;;
end
