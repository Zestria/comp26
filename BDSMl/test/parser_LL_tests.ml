[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Parser_LL
open Printer

let render src =
  match parse_string_LL src with
  | Ok prog -> Printf.sprintf "Ok [%s]" (print_ast prog)
  | Error (`lexer_error msg | `parse_error msg) -> Printf.sprintf "Error: %s" msg
;;

let check ~name src expected =
  let parsed = parse_string_LL src in
  (match parsed, Parse.parse src with
   | Ok exp_ast, Ok act_ast when exp_ast = act_ast -> ()
   | Error _, _ -> ()
   | _ -> failwith (Printf.sprintf "[%s] Parsers disagree on %S" name src));
  let actual =
    match parsed with
    | Ok prog -> Printf.sprintf "Ok [%s]" (print_ast prog)
    | Error (`lexer_error msg | `parse_error msg) -> Printf.sprintf "Error: %s" msg
  in
  if String.trim actual <> String.trim expected
  then
    failwith
      (Printf.sprintf
         "[%s] mismatch for %S:\nexpected: %s\nactual:   %s"
         name
         src
         expected
         actual)
;;

let test_empty_and_whitespace () =
  check ~name:"empty & whitespace" "" "Error: unexpected token '<eof>'";
  check ~name:"empty & whitespace" "   \t\n  \n" "Error: unexpected token '<eof>'"
;;

let test_numbers () =
  check ~name:"numbers" "0" "Ok [Int 0]";
  check ~name:"numbers" "42" "Ok [Int 42]";
  check ~name:"numbers" "123" "Ok [Int 123]"
;;

let test_identifiers () =
  check ~name:"identifiers" "x" "Ok [Var x]";
  check ~name:"identifiers" "foo" "Ok [Var foo]";
  check ~name:"identifiers" "bar123" "Ok [Var bar123]"
;;

let test_simple_expressions () =
  check ~name:"simple expressions" "1 + 2" "Ok [BinOp ('+', Int 1, Int 2)]";
  check
    ~name:"simple expressions"
    "x * y - z"
    "Ok [BinOp ('-', BinOp ('*', Var x, Var y), Var z)]";
  check
    ~name:"simple expressions"
    "not true || false"
    "Ok [BinOp ('||', UnOp (not, Bool true), Bool false)]"
;;

let test_binop_expressions () =
  check
    ~name:"binOp expressions"
    "1 * ((4 - 3) / 2)"
    "Ok [BinOp ('*', Int 1, BinOp ('/', BinOp ('-', Int 4, Int 3), Int 2))]"
;;

let test_if_expressions () =
  check ~name:"if expressions" "if x then y else z" "Ok [If (Var x, Var y, Var z)]";
  check
    ~name:"if expressions"
    "if a + b < c then d else e"
    "Ok [If (BinOp ('<', BinOp ('+', Var a, Var b), Var c), Var d, Var e)]"
;;

let test_let_expressions () =
  check
    ~name:"let expressions"
    "let x = 5 in x + 1"
    "Ok [Let (NonRec, Var x, Int 5, BinOp ('+', Var x, Int 1))]";
  check
    ~name:"let expressions"
    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5"
    "Ok [Let (Rec, Var fact, Abs (Var n, If (BinOp ('<=', Var n, Int 1), Int 1, BinOp \
     ('*', Var n, App (Var fact, BinOp ('-', Var n, Int 1))))), App (Var fact, Int 5))]"
;;

let test_function_application () =
  check ~name:"function application" "f x y" "Ok [App (App (Var f, Var x), Var y)]";
  check ~name:"function application" "(g a) b" "Ok [App (App (Var g, Var a), Var b)]"
;;

let test_abs_expressions () =
  check
    ~name:"abs expressions"
    "fun x -> x + 1"
    "Ok [Abs (Var x, BinOp ('+', Var x, Int 1))]";
  check
    ~name:"abs expressions"
    "fun x -> fun y -> x * y"
    "Ok [Abs (Var x, Abs (Var y, BinOp ('*', Var x, Var y)))]"
;;

let test_cps_factorial () =
  check
    ~name:"CPS factorial"
    "let rec fact_cps n k = if n <= 1 then k 1 else fact_cps (n - 1) (fun m -> k (n * \
     m)) in fact_cps 5 (fun x -> x)"
    "Ok [Let (Rec, Var fact_cps, Abs (Var n, Abs (Var k, If (BinOp ('<=', Var n, Int 1), \
     App (Var k, Int 1), App (App (Var fact_cps, BinOp ('-', Var n, Int 1)), Abs (Var m, \
     App (Var k, BinOp ('*', Var n, Var m))))))), App (App (Var fact_cps, Int 5), Abs \
     (Var x, Var x)))]"
;;

let test_cps_fibonacci () =
  check
    ~name:"CPS fibonacci"
    "let rec fib_cps n k = if n <= 1 then k n else fib_cps (n - 1) (fun a -> fib_cps (n \
     - 2) (fun b -> k (a + b))) in fib_cps 10 (fun x -> x)"
    "Ok [Let (Rec, Var fib_cps, Abs (Var n, Abs (Var k, If (BinOp ('<=', Var n, Int 1), \
     App (Var k, Var n), App (App (Var fib_cps, BinOp ('-', Var n, Int 1)), Abs (Var a, \
     App (App (Var fib_cps, BinOp ('-', Var n, Int 2)), Abs (Var b, App (Var k, BinOp \
     ('+', Var a, Var b))))))))), App (App (Var fib_cps, Int 10), Abs (Var x, Var x)))]"
;;

let () =
  test_empty_and_whitespace ();
  test_numbers ();
  test_identifiers ();
  test_simple_expressions ();
  test_binop_expressions ();
  test_if_expressions ();
  test_let_expressions ();
  test_function_application ();
  test_abs_expressions ();
  test_cps_factorial ();
  test_cps_fibonacci ();
  print_endline "All parser tests passed."
;;
