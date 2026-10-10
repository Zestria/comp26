[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

exception Parse_error of Error_monad.error

let parse source =
  match Topdown_parser.parse_program source with
  | Ok program -> program
  | Error error -> raise (Parse_error error)
;;

let show source =
  source
  |> parse
  |> Middleend.Lambda_lifting.lift_program
  |> Pretty_printer.program_to_string
  |> print_endline
;;

let%expect_test "closed definition stays in place" =
  show "let f x = x + 1";
  [%expect
    {|
    let f x =
      x + 1
    |}]
;;

let%expect_test "lambda capturing a variable is lifted" =
  show "let f x = g (fun y -> x + y)";
  [%expect
    {|
    let lambda1 x y =
      x + y
    let f x =
      g (lambda1 x)
    |}]
;;

let%expect_test "applied capturing lambda is lifted" =
  show "let f x = (fun y -> x + y) 3";
  [%expect
    {|
    let lambda1 x y =
      x + y
    let f x =
      lambda1 x 3
    |}]
;;

let%expect_test "capturing let binding is floated" =
  show "let f x = let g = x + 1 in g * 2";
  [%expect
    {|
    let lambda1 x =
      x + 1
    let f x =
      let g =
        lambda1 x
      in
        g * 2
    |}]
;;

let%expect_test "closed let binding is not floated" =
  show "let f x = let g = 1 in g";
  [%expect
    {|
    let f x =
      let g =
        1
      in
        g
    |}]
;;

let%expect_test "only capturing lambdas move out" =
  show "let f x = if x then (fun y -> y + x) else (fun z -> z)";
  [%expect
    {|
    let lambda1 x y =
      y + x
    let f x =
      if x then
        lambda1 x
      else
        fun z -> z
    |}]
;;

let%expect_test "lambda inside a binding is lifted" =
  show "let f x = let g = fun y -> x + y in g";
  [%expect
    {|
    let lambda1 x y =
      x + y
    let f x =
      let g =
        lambda1 x
      in
        g
    |}]
;;

let%expect_test "recursive name becomes a leading parameter" =
  show "let rec f n = f (fun a -> n + a)";
  [%expect
    {|
    let lambda1 n a =
      n + a
    let rec f n =
      f (lambda1 n)
    |}]
;;

let%expect_test "cps factorial" =
  show "let rec fac n k = if n < 2 then k 1 else fac (n - 1) (fun a -> k (a * n))";
  [%expect
    {|
    let lambda1 k n a =
      k (a * n)
    let rec fac n k =
      if n < 2 then
        k 1
      else
        fac (n - 1) (lambda1 k n)
    |}]
;;

let%expect_test "cps fibonacci" =
  show
    "let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
     -> k (a + b)))";
  [%expect
    {|
    let lambda1 a k b =
      k (a + b)
    let lambda2 fib k n a =
      fib (n - 2) (lambda1 a k)
    let rec fib n k =
      if n < 2 then
        k n
      else
        fib (n - 1) (lambda2 fib k n)
    |}]
;;
