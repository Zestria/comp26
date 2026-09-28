[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend

let print_result = function
  | Ok literal -> Printf.printf "Ok %S\n" (Integer_literal.to_string literal)
  | Error (Integer_literal.Invalid_integer_literal text) ->
    Printf.printf "Error (Invalid_integer_literal %S)\n" text
;;

let negate text =
  Integer_literal.of_string text |> Result.map Integer_literal.negate |> print_result
;;

let%expect_test "negation preserves leading zeroes and separators" =
  negate "001__2_";
  [%expect {| Ok "-001__2_" |}]
;;

let%expect_test "negation removes the sign from a negative literal" =
  negate "-001__2_";
  [%expect {| Ok "001__2_" |}]
;;

let%expect_test "negation preserves signed zero spelling" =
  negate "0";
  [%expect {| Ok "-0" |}]
;;

let%expect_test "integer accepts: zero" =
  Integer_literal.of_string "0" |> print_result;
  [%expect {| Ok "0" |}]
;;

let%expect_test "integer accepts: last decimal digit" =
  Integer_literal.of_string "9" |> print_result;
  [%expect {| Ok "9" |}]
;;

let%expect_test "integer accepts: negative zero" =
  Integer_literal.of_string "-0" |> print_result;
  [%expect {| Ok "-0" |}]
;;

let%expect_test "integer accepts: negative integer" =
  Integer_literal.of_string "-42" |> print_result;
  [%expect {| Ok "-42" |}]
;;

let%expect_test "integer accepts: all decimal digits" =
  Integer_literal.of_string "1234567890" |> print_result;
  [%expect {| Ok "1234567890" |}]
;;

let%expect_test "integer accepts: leading zeroes" =
  Integer_literal.of_string "0001" |> print_result;
  [%expect {| Ok "0001" |}]
;;

let%expect_test "integer accepts: separator" =
  Integer_literal.of_string "1_000" |> print_result;
  [%expect {| Ok "1_000" |}]
;;

let%expect_test "integer accepts: consecutive separators" =
  Integer_literal.of_string "1__2" |> print_result;
  [%expect {| Ok "1__2" |}]
;;

let%expect_test "integer accepts: trailing separator" =
  Integer_literal.of_string "1_" |> print_result;
  [%expect {| Ok "1_" |}]
;;

let%expect_test "integer accepts: negative with separators" =
  Integer_literal.of_string "-0__1_" |> print_result;
  [%expect {| Ok "-0__1_" |}]
;;

let%expect_test "integer accepts: no machine integer range limit" =
  let text = String.make 1000 '9' in
  (match Integer_literal.of_string text with
   | Ok literal ->
     Printf.printf
       "accepted: length=%d, spelling preserved=%b\n"
       (String.length (Integer_literal.to_string literal))
       (String.equal text (Integer_literal.to_string literal))
   | Error _ as result -> print_result result);
  [%expect {| accepted: length=1000, spelling preserved=true |}]
;;

let%expect_test "integer rejects: empty" =
  Integer_literal.of_string "" |> print_result;
  [%expect {| Error (Invalid_integer_literal "") |}]
;;

let%expect_test "integer rejects: bare minus" =
  Integer_literal.of_string "-" |> print_result;
  [%expect {| Error (Invalid_integer_literal "-") |}]
;;

let%expect_test "integer rejects: explicit plus" =
  Integer_literal.of_string "+1" |> print_result;
  [%expect {| Error (Invalid_integer_literal "+1") |}]
;;

let%expect_test "integer rejects: repeated sign" =
  Integer_literal.of_string "--1" |> print_result;
  [%expect {| Error (Invalid_integer_literal "--1") |}]
;;

let%expect_test "integer rejects: leading separator" =
  Integer_literal.of_string "_1" |> print_result;
  [%expect {| Error (Invalid_integer_literal "_1") |}]
;;

let%expect_test "integer rejects: separator after sign" =
  Integer_literal.of_string "-_1" |> print_result;
  [%expect {| Error (Invalid_integer_literal "-_1") |}]
;;

let%expect_test "integer rejects: embedded sign" =
  Integer_literal.of_string "1-2" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1-2") |}]
;;

let%expect_test "integer rejects: letter" =
  Integer_literal.of_string "12a" |> print_result;
  [%expect {| Error (Invalid_integer_literal "12a") |}]
;;

let%expect_test "integer rejects: float" =
  Integer_literal.of_string "1.0" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1.0") |}]
;;

let%expect_test "integer rejects: exponent" =
  Integer_literal.of_string "1e2" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1e2") |}]
;;

let%expect_test "integer rejects: hexadecimal" =
  Integer_literal.of_string "0xff" |> print_result;
  [%expect {| Error (Invalid_integer_literal "0xff") |}]
;;

let%expect_test "integer rejects: octal" =
  Integer_literal.of_string "0o77" |> print_result;
  [%expect {| Error (Invalid_integer_literal "0o77") |}]
;;

let%expect_test "integer rejects: binary" =
  Integer_literal.of_string "0b10" |> print_result;
  [%expect {| Error (Invalid_integer_literal "0b10") |}]
;;

let%expect_test "integer rejects: int32 suffix" =
  Integer_literal.of_string "1l" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1l") |}]
;;

let%expect_test "integer rejects: int64 suffix" =
  Integer_literal.of_string "1L" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1L") |}]
;;

let%expect_test "integer rejects: nativeint suffix" =
  Integer_literal.of_string "1n" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1n") |}]
;;

let%expect_test "integer rejects: leading space" =
  Integer_literal.of_string " 1" |> print_result;
  [%expect {| Error (Invalid_integer_literal " 1") |}]
;;

let%expect_test "integer rejects: trailing space" =
  Integer_literal.of_string "1 " |> print_result;
  [%expect {| Error (Invalid_integer_literal "1 ") |}]
;;

let%expect_test "integer rejects: embedded space" =
  Integer_literal.of_string "1 2" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1 2") |}]
;;

let%expect_test "integer rejects: space after sign" =
  Integer_literal.of_string "- 1" |> print_result;
  [%expect {| Error (Invalid_integer_literal "- 1") |}]
;;

let%expect_test "integer rejects: newline" =
  Integer_literal.of_string "1\n" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1\n") |}]
;;

let%expect_test "integer rejects: NUL" =
  Integer_literal.of_string "1\000" |> print_result;
  [%expect {| Error (Invalid_integer_literal "1\000") |}]
;;

let%expect_test "integer rejects: non-ASCII digit" =
  Integer_literal.of_string "١" |> print_result;
  [%expect {| Error (Invalid_integer_literal "\217\161") |}]
;;

let%expect_test "integer error: empty" =
  Integer_literal.error_to_string (Invalid_integer_literal "") |> print_endline;
  [%expect {| Invalid integer literal: "" |}]
;;

let%expect_test "integer error: ordinary input" =
  Integer_literal.error_to_string (Invalid_integer_literal "+1") |> print_endline;
  [%expect {| Invalid integer literal: "+1" |}]
;;

let%expect_test "integer error: escaped characters" =
  Integer_literal.error_to_string (Invalid_integer_literal "\"\\\n\t\r\000")
  |> print_endline;
  [%expect {| Invalid integer literal: "\"\\\n\t\r\000" |}]
;;
