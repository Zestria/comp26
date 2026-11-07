[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
module CC = SchonfinkelML_middleend.Closure_conversion
module Lifting = SchonfinkelML_middleend.Lambda_lifting

(* The pass runs after closure conversion, so the tests convert first: every
   abstraction of its input is closed. *)
let lift_of_string s =
  match Top_down.Parser.parse_expr s with
  | Ok e -> Lifting.lift (CC.closure_convert e)
  | Error err -> raise (Failure (Top_down.Parser.string_of_error err))
;;

let print_lifted s = print_endline (string_of_expr (lift_of_string s))

(* After lifting an abstraction may only be bound by a [let]/[let rec]: the
   right-hand side of a binding (possibly a curried chain) or the body of such a
   chain.  An abstraction anywhere else -- an argument, a condition, a branch, a
   tail -- is a closure the pass failed to hoist. *)
let rec chain_ok = function
  | Abstr (_, body) -> chain_ok body
  | e -> expr_ok e

and expr_ok = function
  | Ident _ | Const _ -> true
  | Abstr _ -> false
  | App (a, b) -> expr_ok a && expr_ok b
  | If (c, t, f) -> expr_ok c && expr_ok t && expr_ok f
  | Let (_, _, rhs, rest) -> chain_ok rhs && expr_ok rest
;;

let print_shape s =
  let lifted = lift_of_string s in
  print_endline (if expr_ok lifted then "all lambdas are let-bound" else "stray lambda")
;;

let cps_fact =
  {|
let rec fact n k =
  if n = 0 then k 1
  else fact (n - 1) (fun acc -> k (n * acc))
in
fact 5 (fun x -> x)
|}
;;

let cps_fib =
  {|
let rec fib n k =
  if n < 2 then k n
  else fib (n - 1) (fun x -> fib (n - 2) (fun y -> k (x + y)))
in
fib 6 (fun x -> x)
|}
;;

(* A closed lambda becomes a top-level function; the binding closure conversion
   left for it only renames the lifted function, so it disappears too. *)
let%expect_test "closed lambda is lifted" =
  print_lifted "let f = fun x -> x + 1 in f 5";
  [%expect {| let lift1 = fun x -> x + 1 in (lift1 5) |}]
;;

(* The captured variable is a parameter of the lifted function already: closure
   conversion put it in front of the lambda, so lifting adds nothing. *)
let%expect_test "captured variable is already a parameter" =
  print_lifted "let a = 1 in let f = fun x -> x + a in f 5";
  [%expect
    {|
    let lift1 = fun a -> fun x -> x + a in
      let a = 1 in let f = (lift1 a) in (f 5)
    |}]
;;

(* A curried chain is lifted as a single function: no partial application of the
   lifted function, hence no closure created by the lift itself. *)
let%expect_test "curried chain is lifted as a whole" =
  print_lifted "let a = 1 in let f = fun x -> fun y -> x + y + a in f 1 2";
  [%expect
    {|
    let lift1 = fun a -> fun x -> fun y -> (x + y) + a in
      let a = 1 in let f = (lift1 a) in ((f 1) 2)
    |}]
;;

(* A recursive binding is not replaced by the name of its chain: its body calls
   it by name.  The binding is kept as it is (here it is already at the front). *)
let%expect_test "recursive function keeps its binding and calls itself" =
  print_lifted "let rec f = fun n -> f n in f 1";
  [%expect {| let rec f = fun n -> (f n) in (f 1) |}]
;;

let%expect_test "lambda inside a recursive function is lifted" =
  print_lifted "let rec f = fun n -> let g = fun x -> f x in g n in f 1";
  [%expect
    {|
    let lift1 = fun f -> fun x -> (f x) in
      let rec f = fun n -> let g = (lift1 f) in (g n) in (f 1)
    |}]
;;

let%expect_test "lambdas in the branches of an if are lifted" =
  print_lifted "if c then (fun x -> x) else (fun y -> y)";
  [%expect
    {|
    let lift1 = fun x -> x in
      let lift2 = fun y -> y in if c then lift1 else lift2
    |}]
;;

(* A recursive function keeps its own binding, so it can still call itself; that
   binding is floated to the front like every other definition, so a nested
   recursive function is not re-created on every call of its parent. *)
let%expect_test "nested recursive function is floated to the front" =
  print_lifted
    "let rec f = fun n -> let rec g = fun i -> if i = 0 then 0 else g (i - 1) in g n in \
     f 3";
  [%expect
    {|
    let rec g = fun i -> if i = 0 then 0 else (g (i - 1)) in
      let rec f = fun n -> (g n) in (f 3)
    |}]
;;

let%expect_test "lifted names avoid collisions with source names" =
  print_lifted "let lift1 = 1 in let f = fun x -> x + lift1 in f 2";
  [%expect
    {|
    let lift2 = fun lift1 -> fun x -> x + lift1 in
      let lift1 = 1 in let f = (lift2 lift1) in (f 2)
    |}]
;;

let%expect_test "cps factorial" =
  print_lifted cps_fact;
  [%expect
    {|
    let lift1 = fun k -> fun n -> fun acc -> (k (n * acc)) in
      let lift2 = fun x -> x in
        let rec fact =
          fun n -> fun k -> if n = 0 then (k 1) else
                              ((fact (n - 1)) ((lift1 k) n))
          in ((fact 5) lift2)
    |}]
;;

let%expect_test "cps fibonacci" =
  print_lifted cps_fib;
  [%expect
    {|
    let lift1 = fun k -> fun x -> fun y -> (k (x + y)) in
      let lift2 =
        fun fib -> fun k -> fun n -> fun x -> ((fib (n - 2)) ((lift1 k) x)) in
        let lift3 = fun x_1 -> x_1 in
          let rec fib =
            fun n -> fun k -> if n < 2 then (k n) else
                                ((fib (n - 1)) (((lift2 fib) k) n))
            in ((fib 6) lift3)
    |}]
;;

let%expect_test "cps factorial has no stray lambdas" =
  print_shape cps_fact;
  [%expect {| all lambdas are let-bound |}]
;;

let%expect_test "cps fibonacci has no stray lambdas" =
  print_shape cps_fib;
  [%expect {| all lambdas are let-bound |}]
;;

let%expect_test "cps factorial lifts every lambda" =
  print_shape
    "let a = 1 in let f = fun x -> fun y -> x + y + a in let g = fun z -> z in f 1 2";
  [%expect {| all lambdas are let-bound |}]
;;
