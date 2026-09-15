[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Lexer

let show_token = function
  | TEOF -> "TEOF"
  | TInt i -> Printf.sprintf "TInt(%d)" i
  | TIdent s -> Printf.sprintf "TIdent(%S)" s
  | TUnderscore -> "TUnderscore"
  | TTrue -> "TTrue"
  | TFalse -> "TFalse"
  | TIF -> "TIF"
  | TThen -> "TThen"
  | TElse -> "TElse"
  | TLet -> "TLet"
  | TRec -> "TRec"
  | TIn -> "TIn"
  | TFun -> "TFun"
  | TNot -> "TNot"
  | TUnit -> "TUnit"
  | TLParen -> "TLParen"
  | TRParen -> "TRParen"
  | TPlus -> "TPlus"
  | TMinus -> "TMinus"
  | TArrow -> "TArrow"
  | TStar -> "TStar"
  | TSlash -> "TSlash"
  | TEqual -> "TEqual"
  | TNotEqual -> "TNotEqual"
  | TLess -> "TLess"
  | TELess -> "TELess"
  | TMore -> "TMore"
  | TEMore -> "TEMore"
  | TAnd -> "TAnd"
  | TOr -> "TOr"
  | TSemisemi -> "TSemisemi"
;;

let test src =
  let tokens = tokenize src in
  if Lexer_LR.tokenize src <> tokens
  then failwith (Printf.sprintf "Lexers disagree on %S" src);
  match tokens with
  | Ok toks ->
    let tok_strs = List.map show_token toks in
    Printf.printf "Ok [%s]\n" (String.concat "; " tok_strs)
  | Error (`lexer_error msg) -> Printf.printf "Error: %s\n" msg
;;

let%expect_test "empty & whitespace" =
  test "";
  [%expect {| Ok [TEOF] |}];
  test "   \t\n  \n";
  [%expect {| Ok [TEOF] |}]
;;

(* --- Числа --- *)

let%expect_test "numbers" =
  test "0";
  [%expect {| Ok [TInt(0); TEOF] |}];
  test "42";
  [%expect {| Ok [TInt(42); TEOF] |}];
  test "123";
  [%expect {| Ok [TInt(123); TEOF] |}];
  test "999999999999999999999999999";
  [%expect {| Error: 1:1: integer literal '999999999999999999999999999' out of range |}]
;;

let%expect_test "identifiers" =
  test "x";
  [%expect {| Ok [TIdent("x"); TEOF] |}];
  test "x1";
  [%expect {| Ok [TIdent("x1"); TEOF] |}];
  test "_";
  [%expect {| Ok [TUnderscore; TEOF] |}];
  test "_foo";
  [%expect {| Ok [TIdent("_foo"); TEOF] |}];
  test "fooBar";
  [%expect {| Ok [TIdent("fooBar"); TEOF] |}]
;;

let%expect_test "keywords" =
  test "true false if then else let rec in fun not";
  [%expect {| Ok [TTrue; TFalse; TIF; TThen; TElse; TLet; TRec; TIn; TFun; TNot; TEOF] |}]
;;

let%expect_test "maximal munch for keywords" =
  test "iffy";
  [%expect {| Ok [TIdent("iffy"); TEOF] |}];
  test "inlet";
  [%expect {| Ok [TIdent("inlet"); TEOF] |}];
  test "lettuce";
  [%expect {| Ok [TIdent("lettuce"); TEOF] |}];
  test "trueish";
  [%expect {| Ok [TIdent("trueish"); TEOF] |}];
  test "funny";
  [%expect {| Ok [TIdent("funny"); TEOF] |}];
  test "recover";
  [%expect {| Ok [TIdent("recover"); TEOF] |}];
  test "thenable";
  [%expect {| Ok [TIdent("thenable"); TEOF] |}];
  test "nothing";
  [%expect {| Ok [TIdent("nothing"); TEOF] |}]
;;

let%expect_test "parens and unit" =
  test "()";
  [%expect {| Ok [TUnit; TEOF] |}];
  test "(x)";
  [%expect {| Ok [TLParen; TIdent("x"); TRParen; TEOF] |}];
  test "( )";
  [%expect {| Ok [TLParen; TRParen; TEOF] |}]
;;

let%expect_test "operators" =
  test "+";
  [%expect {| Ok [TPlus; TEOF] |}];
  test "-";
  [%expect {| Ok [TMinus; TEOF] |}];
  test "->";
  [%expect {| Ok [TArrow; TEOF] |}];
  test "*";
  [%expect {| Ok [TStar; TEOF] |}];
  test "/";
  [%expect {| Ok [TSlash; TEOF] |}];
  test "=";
  [%expect {| Ok [TEqual; TEOF] |}];
  test "<>";
  [%expect {| Ok [TNotEqual; TEOF] |}];
  test "<";
  [%expect {| Ok [TLess; TEOF] |}];
  test "<=";
  [%expect {| Ok [TELess; TEOF] |}];
  test ">";
  [%expect {| Ok [TMore; TEOF] |}];
  test ">=";
  [%expect {| Ok [TEMore; TEOF] |}];
  test "&&";
  [%expect {| Ok [TAnd; TEOF] |}];
  test "||";
  [%expect {| Ok [TOr; TEOF] |}];
  test ";;";
  [%expect {| Ok [TSemisemi; TEOF] |}]
;;

let%expect_test "expressions" =
  test "-5";
  [%expect {| Ok [TMinus; TInt(5); TEOF] |}];
  test "1+2*3";
  [%expect {| Ok [TInt(1); TPlus; TInt(2); TStar; TInt(3); TEOF] |}]
;;

let%expect_test "lexer errors" =
  test "&";
  [%expect {| Error: 1:1: unexpected '&' (expected '&&') |}];
  test "|";
  [%expect {| Error: 1:1: unexpected '|' (expected '||') |}];
  test ";";
  [%expect {| Error: 1:1: unexpected character ';' |}];
  test "@";
  [%expect {| Error: 1:1: unexpected character '@' |}];
  test "$";
  [%expect {| Error: 1:1: unexpected character '$' |}]
;;

let%expect_test "error positions" =
  test "\n\n@";
  [%expect {| Error: 3:1: unexpected character '@' |}];
  test "ab @";
  [%expect {| Error: 1:4: unexpected character '@' |}]
;;
