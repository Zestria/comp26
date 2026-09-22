[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml.Ast

let int value = Expr_constant (Const_int value)
let var name = Expr_ident name

let app function_ arguments =
  List.fold_left (fun f a -> Expr_app (f, a)) function_ arguments
;;

let binary operator left right = app (var operator) [ left; right ]

let binding name expression =
  { binding_pattern = Pat_var name; binding_expression = expression }
;;

let fack_source =
  "let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))"
;;

let fack =
  Structure_value
    ( Recursive
    , binding
        "fack"
        (Expr_fun
           ( Pat_var "n"
           , [ Pat_var "k" ]
           , Expr_if
               ( binary "<=" (var "n") (int 1)
               , app (var "k") [ int 1 ]
               , app
                   (var "fack")
                   [ binary "-" (var "n") (int 1)
                   ; Expr_fun
                       (Pat_var "m", [], app (var "k") [ binary "*" (var "m") (var "n") ])
                   ] ) ))
    , [] )
;;

let fibk_source =
  "let rec fibk n k =\n\
  \  if n < 2 then k n else fibk (n - 1) (fun a -> fibk (n - 2) (fun b -> k (a + b)))"
;;

let fibk =
  Structure_value
    ( Recursive
    , binding
        "fibk"
        (Expr_fun
           ( Pat_var "n"
           , [ Pat_var "k" ]
           , Expr_if
               ( binary "<" (var "n") (int 2)
               , app (var "k") [ var "n" ]
               , app
                   (var "fibk")
                   [ binary "-" (var "n") (int 1)
                   ; Expr_fun
                       ( Pat_var "a"
                       , []
                       , app
                           (var "fibk")
                           [ binary "-" (var "n") (int 2)
                           ; Expr_fun
                               ( Pat_var "b"
                               , []
                               , app (var "k") [ binary "+" (var "a") (var "b") ] )
                           ] )
                   ] ) ))
    , [] )
;;

let main =
  let identity = Expr_fun (Pat_var "x", [], var "x") in
  Structure_value
    ( Non_recursive
    , binding
        "main"
        (Expr_tuple
           ( app (var "fack") [ int 5; identity ]
           , app (var "fibk") [ int 10; identity ]
           , [] ))
    , [] )
;;
