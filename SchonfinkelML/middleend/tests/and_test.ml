[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Common
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
  [%expect {| let v1 = 1 + 2 in v1 |}]
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
  [%expect {| let x = 1 in let v2 = x + 2 in v2 |}]
;;

let%expect_test "complex add" =
  print_anf (eadd (eadd (econsti 2) (econsti 2)) (econsti 2));
  [%expect {| let v4 = 2 + 2 in let v3 = v4 + 2 in v3 |}]
;;
