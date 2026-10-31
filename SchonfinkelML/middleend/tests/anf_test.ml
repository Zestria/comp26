[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
module Anf = Middleend.Anf

let print_anf e = print_endline (Anf.string_of_aexpr (Anf.create_anf e))

let%expect_test "int literal" =
  print_anf (econsti 42);
  [%expect {| 42 |}]
;;

let%expect_test "unit literal" =
  print_anf eunit;
  [%expect {| () |}]
;;

let%expect_test "identifier" =
  print_anf (Ident "x");
  [%expect {| x |}]
;;

let%expect_test "binary op becomes a let-bound temporary" =
  print_anf (eadd (econsti 1) (econsti 2));
  [%expect {| 1 + 2 |}]
;;

let%expect_test "if" =
  print_anf (eif (econsti 1) (econsti 2) (econsti 3));
  [%expect {| if 1 then 2 else 3 |}]
;;

let%expect_test "abstraction" =
  print_anf (elambda "x" (Ident "x"));
  [%expect {| fun x -> x |}]
;;

let%expect_test "application" =
  print_anf (eapp (Ident "f") (Ident "x"));
  [%expect {| (f x) |}]
;;

let%expect_test "curried application" =
  print_anf (eapp (eapp (Ident "f") (Ident "x")) (Ident "y"));
  [%expect {| (f x y) |}]
;;

let%expect_test "let with trivial body" =
  print_anf (elet "x" (econsti 1) (Ident "x"));
  [%expect {| let x = 1 in x |}]
;;

let%expect_test "let with non-trivial body keeps evaluation order" =
  print_anf (elet "x" (econsti 1) (eadd (Ident "x") (econsti 2)));
  [%expect {| let x = 1 in x + 2 |}]
;;

let%expect_test "add" =
  print_anf (eadd (eadd (econsti 2) (econsti 2)) (econsti 2));
  [%expect {| let v1 = 2 + 2 in v1 + 2 |}]
;;

let%expect_test "add funcs" =
  print_anf (eadd (eapp (Ident "f") (Ident "x")) (eapp (Ident "g") (Ident "x")));
  [%expect {| let v1 = (f x) in let v2 = (g x) in v1 + v2 |}]
;;

let%expect_test "let" =
  print_anf (elet "foo" (eapp (Ident "f") (Ident "x")) (eadd (Ident "foo") (econsti 3)));
  [%expect {| let foo = (f x) in foo + 3 |}]
;;

let%expect_test "lambda" =
  print_anf
    (elet
       "foo"
       (elambda "x" (eadd (Ident "x") (econsti 4)))
       (eapp (Ident "foo") (econsti 3)));
  [%expect {| let foo = fun x -> x + 4 in (foo 3) |}]
;;

let%expect_test "if expression" =
  print_anf
    (elet "foo" (eif (eeq (Ident "x") (Ident "y")) (Ident "x") (Ident "y")) (econsti 3));
  [%expect {| let foo = let v1 = x = y in if v1 then x else y in 3 |}]
;;

let%expect_test "slides num 9" =
  print_anf
    (eadd (eadd (econsti 1) (eif (Ident "x") (Ident "y") (Ident "z"))) (econsti 2));
  [%expect {| let v1 = if x then y else z in let v2 = 1 + v1 in v2 + 2 |}]
;;
