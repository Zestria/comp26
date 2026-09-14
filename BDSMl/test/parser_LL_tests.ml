open Frontend
open Parser_LL
open Printer_LL

let test src =
  match parse_string_LL src with
  | Ok prog -> Printf.printf "Ok [%s]\n" (print_ast prog)
  | Error (`lexer_error msg | `parse_error msg) -> Printf.printf "Error: %s\n" msg
;;

let%expect_test "empty & whitespace" =
  test "";
  [%expect {| Error: unexpected token '<eof>' |}];
  test "   \t\n  \n";
  [%expect {| Error: unexpected token '<eof>' |}]
;;

let%expect_test "numbers" =
  test "0";
  [%expect {| Ok [Int 0] |}];
  test "42";
  [%expect {| Ok [Int 42] |}];
  test "123";
  [%expect {| Ok [Int 123] |}]
;;

let%expect_test "identifiers" =
  test "x";
  [%expect {| Ok [Var x] |}];
  test "foo";
  [%expect {| Ok [Var foo] |}];
  test "bar123";
  [%expect {| Ok [Var bar123] |}]
;;

let%expect_test "simple expressions" =
  test "1 + 2";
  [%expect {| Ok [BinOp ('+', Int 1, Int 2)] |}];
  test "x * y - z";
  [%expect {| Ok [BinOp ('-', BinOp ('*', Var x, Var y), Var z)] |}];
  test "not true || false";
  [%expect {| Ok [BinOp ('||', UnOp (not, Bool true), Bool false)] |}]
;;

let%expect_test "if expressions" =
  test "if x then y else z";
  [%expect {| Ok [If (Var x, Var y, Var z)] |}];
  test "if a + b < c then d else e";
  [%expect {| Ok [If (BinOp ('<', BinOp ('+', Var a, Var b), Var c), Var d, Var e)] |}]
;;

let%expect_test "Let expressions" =
  test "let x = 5 in x + 1";
  [%expect {| Ok [Let (NonRec, Var x, Int 5, BinOp ('+', Var x, Int 1))] |}];
  test "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5";
  [%expect
    {|
Ok [Let (Rec, Var fact, Abs (Var n, If (BinOp ('<=', Var n, Int 1), Int 1, BinOp ('*', Var n, App (Var fact, BinOp ('-', Var n, Int 1))))), App (Var fact, Int 5))] |}]
;;

let%expect_test "function application" =
  test "f x y";
  [%expect {| Ok [App (App (Var f, Var x), Var y)] |}];
  test "(g a) b";
  [%expect {| Ok [App (App (Var g, Var a), Var b)] |}]
;;

let%expect_test "Abs expressions" =
  test "fun x -> x + 1";
  [%expect {| Ok [Abs (Var x, BinOp ('+', Var x, Int 1))] |}];
  test "fun x -> fun y -> x * y";
  [%expect {| Ok [Abs (Var x, Abs (Var y, BinOp ('*', Var x, Var y)))] |}]
;;

let%expect_test "CPS factorial" =
  test
    "let rec fact_cps n k = if n <= 1 then k 1 else fact_cps (n - 1) (fun m -> k (n * \
     m)) in fact_cps 5 (fun x -> x)";
  [%expect {| Ok [Let (Rec, Var fact_cps, Abs (Var n, Abs (Var k, If (BinOp ('<=', Var n, Int 1), App (Var k, Int 1), App (App (Var fact_cps, BinOp ('-', Var n, Int 1)), Abs (Var m, App (Var k, BinOp ('*', Var n, Var m))))))), App (App (Var fact_cps, Int 5), Abs (Var x, Var x)))] |}]
;;

let%expect_test "CPS fibonacci" =
  test
    "let rec fib_cps n k = if n <= 1 then k n else fib_cps (n - 1) (fun a -> fib_cps (n \
     - 2) (fun b -> k (a + b))) in fib_cps 10 (fun x -> x)";
  [%expect {| Ok [Let (Rec, Var fib_cps, Abs (Var n, Abs (Var k, If (BinOp ('<=', Var n, Int 1), App (Var k, Var n), App (App (Var fib_cps, BinOp ('-', Var n, Int 1)), Abs (Var a, App (App (Var fib_cps, BinOp ('-', Var n, Int 2)), Abs (Var b, App (Var k, BinOp ('+', Var a, Var b))))))))), App (App (Var fib_cps, Int 10), Abs (Var x, Var x)))] |}]
;;
