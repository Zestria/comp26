[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend

let print source =
  match Parser.parse_program_with_operators source with
  | Ok program -> Format.printf "%a@." Ast_print.program program
  | Error error -> Printf.printf "Unexpected parse error for %S: %s\n" source error
;;

let reject source =
  match Parser.parse_program_with_operators source with
  | Error _ -> print_endline "Rejected"
  | Ok program ->
    Format.printf "Unexpectedly accepted %S: %a@." source Ast_print.program program
;;

let%expect_test "right associative addition" =
  print "infixr 6 (+);; a + b + c";
  [%expect
    {| [Evaluation (Apply (Var "+", [Var "a"; Apply (Var "+", [Var "b"; Var "c"])]))] |}]
;;

let%expect_test "left associative power" =
  print "infixl 8 ( ** );; a ** b ** c";
  [%expect
    {| [Evaluation (Apply (Var "**", [Apply (Var "**", [Var "a"; Var "b"]); Var "c"]))] |}]
;;

let%expect_test "custom precedence above ordinary multiplication" =
  print "infixl 9 (+);; a + b * c";
  [%expect
    {| [Evaluation (Apply (Var "*", [Apply (Var "+", [Var "a"; Var "b"]); Var "c"]))] |}]
;;

let%expect_test "custom precedence below ordinary addition" =
  print "infixl 1 ( * );; a * b + c";
  [%expect
    {| [Evaluation (Apply (Var "*", [Var "a"; Apply (Var "+", [Var "b"; Var "c"])]))] |}]
;;

let%expect_test "prefix operator with three operands" =
  print "prefix 3 (%%);; %% a b c";
  [%expect {| [Evaluation (Apply (Var "%%", [Var "a"; Var "b"; Var "c"]))] |}]
;;

let%expect_test "binary operator becomes unary prefix" =
  print "prefix 1 (+);; + a";
  [%expect {| [Evaluation (Apply (Var "+", [Var "a"]))] |}]
;;

let%expect_test "ordinary prefix operator becomes binary infix" =
  print "infixl 6 (!);; a ! b";
  [%expect {| [Evaluation (Apply (Var "!", [Var "a"; Var "b"]))] |}]
;;

let%expect_test "nested prefix applications consume their own operands" =
  print "prefix 2 (%%);; %% a %% b c";
  [%expect
    {| [Evaluation (Apply (Var "%%", [Var "a"; Apply (Var "%%", [Var "b"; Var "c"])]))] |}]
;;

let%expect_test "prefix operands include ordinary prefixes and parenthesized expressions" =
  print "prefix 3 (%%);; %% !a (b + c) (f x)";
  [%expect
    {| [Evaluation (Apply (Var "%%", [Apply (Var "!", [Var "a"]); Apply (Var "+", [Var "b"; Var "c"]); Apply (Var "f", [Var "x"])]))] |}]
;;

let%expect_test "a constructor argument does not consume the next prefix operand" =
  print "prefix 2 (%%);; %% true x";
  [%expect {| [Evaluation (Apply (Var "%%", [true; Var "x"]))] |}]
;;

let%expect_test "a prefix result can be applied" =
  print "prefix 2 (%%);; %% a b c";
  [%expect {| [Evaluation (Apply (Apply (Var "%%", [Var "a"; Var "b"]), [Var "c"]))] |}]
;;

let%expect_test "parenthesized operator values keep ordinary partial application" =
  print "prefix 3 (%%);; (%%) a";
  [%expect {| [Evaluation (Apply (Var "%%", [Var "a"]))] |}]
;;

let%expect_test "declarations do not alter earlier definitions" =
  print "let before = a + b + c;; infixr 6 (+);; let after = a + b + c";
  [%expect
    {| [Definition (PVar "before" = Apply (Var "+", [Apply (Var "+", [Var "a"; Var "b"]); Var "c"])); Definition (PVar "after" = Apply (Var "+", [Var "a"; Apply (Var "+", [Var "b"; Var "c"])]))] |}]
;;

let%expect_test "a later declaration replaces the earlier one" =
  print "infixr 6 (+);; infixl 6 (+);; a + b + c";
  [%expect
    {| [Evaluation (Apply (Var "+", [Apply (Var "+", [Var "a"; Var "b"]); Var "c"]))] |}]
;;

let%expect_test "a declaration alone has no runtime expression" =
  print "prefix 3 (%%);;";
  [%expect {| [] |}]
;;

let%expect_test "an infix declaration replaces a previous prefix declaration" =
  print "prefix 3 (%%);; infixl 6 (%%);; a %% b";
  [%expect {| [Evaluation (Apply (Var "%%", [Var "a"; Var "b"]))] |}]
;;

let%expect_test "a prefix declaration replaces a previous infix declaration" =
  print "infixr 6 (%%);; prefix 3 (%%);; %% a b c";
  [%expect {| [Evaluation (Apply (Var "%%", [Var "a"; Var "b"; Var "c"]))] |}]
;;

let%expect_test "operator declaration and function definition are independent" =
  print "prefix 3 (%%);; let (%%) a b c = a;; %% 1 2 3";
  [%expect
    {| [Definition (PVar "%%" = Fun ([PVar "a"; PVar "b"; PVar "c"], Var "a")); Evaluation (Apply (Var "%%", [Int "1"; Int "2"; Int "3"]))] |}]
;;

let%expect_test "function bodies use the current operator rules" =
  print "infixr 6 (+);; let f x = x + a + b";
  [%expect
    {| [Definition (PVar "f" = Fun ([PVar "x"], Apply (Var "+", [Var "x"; Apply (Var "+", [Var "a"; Var "b"])])))] |}]
;;

let%expect_test "parentheses resolve mixed associativity" =
  print "infixl 6 (+);; infixr 6 ( ** );; a + (b ** c)";
  [%expect
    {| [Evaluation (Apply (Var "+", [Var "a"; Apply (Var "**", [Var "b"; Var "c"])]))] |}]
;;

let%expect_test "a word operator can become prefix" =
  print "prefix 2 (mod);; mod a b";
  [%expect {| [Evaluation (Apply (Var "mod", [Var "a"; Var "b"]))] |}]
;;

let%expect_test "redeclared hash operator uses the requested precedence" =
  print "infixl 1 (##);; f a ## b + c";
  [%expect
    {| [Evaluation (Apply (Var "##", [Apply (Var "f", [Var "a"]); Apply (Var "+", [Var "b"; Var "c"])]))] |}]
;;

let%expect_test "prefix minus uses the declared function rather than literal negation" =
  print "prefix 1 (-);; -1";
  [%expect {| [Evaluation (Apply (Var "-", [Int "1"]))] |}]
;;

let%expect_test "declaration words are contextual" =
  print "let prefix x = x;; (prefix) 1";
  [%expect
    {| [Definition (PVar "prefix" = Fun ([PVar "x"], Var "x")); Evaluation (Apply (Var "prefix", [Int "1"]))] |}]
;;

let%expect_test "declaration words require a word boundary" =
  print "let prefixfoo = 1;; prefixfoo";
  [%expect {| [Definition (PVar "prefixfoo" = Int "1"); Evaluation (Var "prefixfoo")] |}]
;;

let%expect_test "comments and decimal underscores in a declaration" =
  print "prefix (* arity *) 0_2_ ( (* name *) %% );; %% a b";
  [%expect {| [Evaluation (Apply (Var "%%", [Var "a"; Var "b"]))] |}]
;;

let%expect_test "precedence zero binds below tuples" =
  print "infixr 0 (+);; a + b, c";
  [%expect {| [Evaluation (Apply (Var "+", [Var "a"; Tuple [Var "b"; Var "c"]]))] |}]
;;

let%expect_test "missing prefix operand" =
  reject "prefix 3 (%%);; %% a b";
  [%expect {| Rejected |}]
;;

let%expect_test "mixed associativity with an ordinary operator on the left" =
  reject "infixr 6 ( ** );; a + b ** c";
  [%expect {| Rejected |}]
;;

let%expect_test "mixed associativity with an ordinary operator on the right" =
  reject "infixr 6 ( ** );; a ** b + c";
  [%expect {| Rejected |}]
;;

let%expect_test "zero prefix arity" =
  reject "prefix 0 (%%);;";
  [%expect {| Rejected |}]
;;

let%expect_test "negative prefix arity" =
  reject "prefix -1 (%%);;";
  [%expect {| Rejected |}]
;;

let%expect_test "precedence above nine" =
  reject "infixl 10 (+);;";
  [%expect {| Rejected |}]
;;

let%expect_test "negative precedence" =
  reject "infixr -1 (+);;";
  [%expect {| Rejected |}]
;;

let%expect_test "metadata overflow" =
  reject "prefix 9999999999999999999999999999999999999999 (%%);;";
  [%expect {| Rejected |}]
;;

let%expect_test "missing declaration terminator" =
  reject "infixr 6 (+)";
  [%expect {| Rejected |}]
;;

let%expect_test "ordinary name instead of an operator" =
  reject "prefix 3 (f);;";
  [%expect {| Rejected |}]
;;

let%expect_test "missing operator parentheses" =
  reject "prefix 3 %%;;";
  [%expect {| Rejected |}]
;;

let%expect_test "infix declaration preserves negative integer literals" =
  print "infixl 6 (-);; -1";
  [%expect {| [Evaluation (Int "-1")] |}]
;;

let%expect_test "infix declaration preserves unary negation in an operator chain" =
  print "infixr 6 (-);; -x - y - z";
  [%expect
    {| [Evaluation (Apply (Var "-", [Apply (Var "~-", [Var "x"]); Apply (Var "-", [Var "y"; Var "z"])]))] |}]
;;

let%expect_test "large declared arity with insufficient operands" =
  reject (Printf.sprintf "prefix %d (%%%%);; %%%% a" max_int);
  [%expect {| Rejected |}]
;;

let%expect_test "operator declarations do not leak between successful parses" =
  print "prefix 3 (%%);; %% a b c";
  reject "%% a b c";
  [%expect
    {|
    [Evaluation (Apply (Var "%%", [Var "a"; Var "b"; Var "c"]))]
    Rejected |}]
;;

let%expect_test "operator declarations do not leak from failed parses" =
  reject "prefix 3 (%%);; %% a b";
  reject "%% a b c";
  [%expect
    {|
    Rejected
    Rejected |}]
;;
