[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open LL_parser.Lexer
open Tokens

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

let render src =
  let tokens = tokenize src in
  if Lexer_LR.tokenize src <> tokens
  then failwith (Printf.sprintf "Lexers disagree on %S" src);
  match tokens with
  | Ok toks ->
    let tok_strs = List.map show_token toks in
    Printf.sprintf "Ok [%s]" (String.concat "; " tok_strs)
  | Error (`lexer_error msg) -> Printf.sprintf "Error: %s" msg
;;

let case ~name src expected =
  Alcotest.test_case name `Quick (fun () ->
    let actual = render src in
    Alcotest.(check string) src expected actual)
;;

let empty_and_whitespace_tests =
  [ case ~name:"empty" "" "Ok [TEOF]"; case ~name:"whitespace" "   \t\n  \n" "Ok [TEOF]" ]
;;

let number_tests =
  [ case ~name:"zero" "0" "Ok [TInt(0); TEOF]"
  ; case ~name:"42" "42" "Ok [TInt(42); TEOF]"
  ; case ~name:"123" "123" "Ok [TInt(123); TEOF]"
  ; case
      ~name:"overflow"
      "999999999999999999999999999"
      "Error: 1:1: integer literal '999999999999999999999999999' out of range"
  ]
;;

let identifier_tests =
  [ case ~name:"x" "x" "Ok [TIdent(\"x\"); TEOF]"
  ; case ~name:"x1" "x1" "Ok [TIdent(\"x1\"); TEOF]"
  ; case ~name:"underscore" "_" "Ok [TUnderscore; TEOF]"
  ; case ~name:"_foo" "_foo" "Ok [TIdent(\"_foo\"); TEOF]"
  ; case ~name:"fooBar" "fooBar" "Ok [TIdent(\"fooBar\"); TEOF]"
  ]
;;

let keyword_tests =
  [ case
      ~name:"all keywords"
      "true false if then else let rec in fun not"
      "Ok [TTrue; TFalse; TIF; TThen; TElse; TLet; TRec; TIn; TFun; TNot; TEOF]"
  ]
;;

let maximal_munch_tests =
  [ case ~name:"iffy" "iffy" "Ok [TIdent(\"iffy\"); TEOF]"
  ; case ~name:"inlet" "inlet" "Ok [TIdent(\"inlet\"); TEOF]"
  ; case ~name:"lettuce" "lettuce" "Ok [TIdent(\"lettuce\"); TEOF]"
  ; case ~name:"trueish" "trueish" "Ok [TIdent(\"trueish\"); TEOF]"
  ; case ~name:"funny" "funny" "Ok [TIdent(\"funny\"); TEOF]"
  ; case ~name:"recover" "recover" "Ok [TIdent(\"recover\"); TEOF]"
  ; case ~name:"thenable" "thenable" "Ok [TIdent(\"thenable\"); TEOF]"
  ; case ~name:"nothing" "nothing" "Ok [TIdent(\"nothing\"); TEOF]"
  ]
;;

let parens_and_unit_tests =
  [ case ~name:"unit" "()" "Ok [TUnit; TEOF]"
  ; case ~name:"paren ident" "(x)" "Ok [TLParen; TIdent(\"x\"); TRParen; TEOF]"
  ; case ~name:"paren space" "( )" "Ok [TLParen; TRParen; TEOF]"
  ]
;;

let operator_tests =
  [ case ~name:"plus" "+" "Ok [TPlus; TEOF]"
  ; case ~name:"minus" "-" "Ok [TMinus; TEOF]"
  ; case ~name:"arrow" "->" "Ok [TArrow; TEOF]"
  ; case ~name:"star" "*" "Ok [TStar; TEOF]"
  ; case ~name:"slash" "/" "Ok [TSlash; TEOF]"
  ; case ~name:"equal" "=" "Ok [TEqual; TEOF]"
  ; case ~name:"not equal" "<>" "Ok [TNotEqual; TEOF]"
  ; case ~name:"less" "<" "Ok [TLess; TEOF]"
  ; case ~name:"less-equal" "<=" "Ok [TELess; TEOF]"
  ; case ~name:"more" ">" "Ok [TMore; TEOF]"
  ; case ~name:"more-equal" ">=" "Ok [TEMore; TEOF]"
  ; case ~name:"and" "&&" "Ok [TAnd; TEOF]"
  ; case ~name:"or" "||" "Ok [TOr; TEOF]"
  ; case ~name:"semisemi" ";;" "Ok [TSemisemi; TEOF]"
  ]
;;

let expression_tests =
  [ case ~name:"unary minus" "-5" "Ok [TMinus; TInt(5); TEOF]"
  ; case ~name:"arithmetic" "1+2*3" "Ok [TInt(1); TPlus; TInt(2); TStar; TInt(3); TEOF]"
  ]
;;

let lexer_error_tests =
  [ case ~name:"single &" "&" "Error: 1:1: unexpected '&' (expected '&&')"
  ; case ~name:"single |" "|" "Error: 1:1: unexpected '|' (expected '||')"
  ; case ~name:"semicolon" ";" "Error: 1:1: unexpected character ';'"
  ; case ~name:"at sign" "@" "Error: 1:1: unexpected character '@'"
  ; case ~name:"dollar sign" "$" "Error: 1:1: unexpected character '$'"
  ]
;;

let error_position_tests =
  [ case ~name:"newlines before error" "\n\n@" "Error: 3:1: unexpected character '@'"
  ; case ~name:"error after tokens" "ab @" "Error: 1:4: unexpected character '@'"
  ]
;;

let () =
  Alcotest.run
    "lexer"
    [ "empty & whitespace", empty_and_whitespace_tests
    ; "numbers", number_tests
    ; "identifiers", identifier_tests
    ; "keywords", keyword_tests
    ; "maximal munch for keywords", maximal_munch_tests
    ; "parens and unit", parens_and_unit_tests
    ; "operators", operator_tests
    ; "expressions", expression_tests
    ; "lexer errors", lexer_error_tests
    ; "error positions", error_position_tests
    ]
;;
