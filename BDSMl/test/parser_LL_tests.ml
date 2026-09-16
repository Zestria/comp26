[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Parser_LL
open Printer

let render src =
  let parsed = parse_string_LL src in
  (match parsed, Parse.parse src with
   | Ok exp_ast, Ok act_ast when exp_ast = act_ast -> ()
   | Error _, _ -> ()
   | _ -> failwith (Printf.sprintf "Parsers disagree on %S" src));
  match parsed with
  | Ok prog -> Printf.sprintf "Ok [%s]" (print_ast prog)
  | Error (`lexer_error msg | `parse_error msg) -> Printf.sprintf "Error: %s" msg
;;

let case ~name src expected =
  Alcotest.test_case name `Quick (fun () ->
    let actual = render src in
    Alcotest.(check string) src expected actual)
;;

let empty_and_whitespace_tests =
  [ case ~name:"empty" "" "Error: unexpected token '<eof>'"
  ; case ~name:"whitespace" "   \t\n  \n" "Error: unexpected token '<eof>'"
  ]
;;

let number_tests =
  [ case ~name:"zero" "0" "Ok [Int 0]"
  ; case ~name:"42" "42" "Ok [Int 42]"
  ; case ~name:"123" "123" "Ok [Int 123]"
  ]
;;

let identifier_tests =
  [ case ~name:"x" "x" "Ok [Var x]"
  ; case ~name:"foo" "foo" "Ok [Var foo]"
  ; case ~name:"bar123" "bar123" "Ok [Var bar123]"
  ]
;;

let simple_expression_tests =
  [ case ~name:"addition" "1 + 2" "Ok [BinOp ('+', Int 1, Int 2)]"
  ; case
      ~name:"mul and sub"
      "x * y - z"
      "Ok [BinOp ('-', BinOp ('*', Var x, Var y), Var z)]"
  ; case
      ~name:"not and or"
      "not true || false"
      "Ok [BinOp ('||', UnOp (not, Bool true), Bool false)]"
  ]
;;

let binop_tests =
  [ case
      ~name:"nested parens"
      "1 * ((4 - 3) / 2)"
      "Ok [BinOp ('*', Int 1, BinOp ('/', BinOp ('-', Int 4, Int 3), Int 2))]"
  ]
;;

let if_tests =
  [ case ~name:"simple if" "if x then y else z" "Ok [If (Var x, Var y, Var z)]"
  ; case
      ~name:"if with condition expr"
      "if a + b < c then d else e"
      "Ok [If (BinOp ('<', BinOp ('+', Var a, Var b), Var c), Var d, Var e)]"
  ]
;;

let let_tests =
  [ case
      ~name:"simple let"
      "let x = 5 in x + 1"
      "Ok [Let (NonRec, Var x, Int 5, BinOp ('+', Var x, Int 1))]"
  ; case
      ~name:"let rec factorial"
      "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5"
      "Ok [Let (Rec, Var fact, Abs (Var n, If (BinOp ('<=', Var n, Int 1), Int 1, BinOp \
       ('*', Var n, App (Var fact, BinOp ('-', Var n, Int 1))))), App (Var fact, Int \
       5))]"
  ]
;;

let application_tests =
  [ case ~name:"two args" "f x y" "Ok [App (App (Var f, Var x), Var y)]"
  ; case ~name:"parenthesised" "(g a) b" "Ok [App (App (Var g, Var a), Var b)]"
  ]
;;

let abs_tests =
  [ case ~name:"single arg" "fun x -> x + 1" "Ok [Abs (Var x, BinOp ('+', Var x, Int 1))]"
  ; case
      ~name:"curried"
      "fun x -> fun y -> x * y"
      "Ok [Abs (Var x, Abs (Var y, BinOp ('*', Var x, Var y)))]"
  ]
;;

let cps_tests =
  [ case
      ~name:"factorial"
      "let rec fact_cps n k = if n <= 1 then k 1 else fact_cps (n - 1) (fun m -> k (n * \
       m)) in fact_cps 5 (fun x -> x)"
      "Ok [Let (Rec, Var fact_cps, Abs (Var n, Abs (Var k, If (BinOp ('<=', Var n, Int \
       1), App (Var k, Int 1), App (App (Var fact_cps, BinOp ('-', Var n, Int 1)), Abs \
       (Var m, App (Var k, BinOp ('*', Var n, Var m))))))), App (App (Var fact_cps, Int \
       5), Abs (Var x, Var x)))]"
  ; case
      ~name:"fibonacci"
      "let rec fib_cps n k = if n <= 1 then k n else fib_cps (n - 1) (fun a -> fib_cps \
       (n - 2) (fun b -> k (a + b))) in fib_cps 10 (fun x -> x)"
      "Ok [Let (Rec, Var fib_cps, Abs (Var n, Abs (Var k, If (BinOp ('<=', Var n, Int \
       1), App (Var k, Var n), App (App (Var fib_cps, BinOp ('-', Var n, Int 1)), Abs \
       (Var a, App (App (Var fib_cps, BinOp ('-', Var n, Int 2)), Abs (Var b, App (Var \
       k, BinOp ('+', Var a, Var b))))))))), App (App (Var fib_cps, Int 10), Abs (Var x, \
       Var x)))]"
  ]
;;

let () =
  Alcotest.run
    "parser"
    [ "empty & whitespace", empty_and_whitespace_tests
    ; "numbers", number_tests
    ; "identifiers", identifier_tests
    ; "simple expressions", simple_expression_tests
    ; "binOp expressions", binop_tests
    ; "if expressions", if_tests
    ; "let expressions", let_tests
    ; "function application", application_tests
    ; "abs expressions", abs_tests
    ; "cps", cps_tests
    ]
;;
