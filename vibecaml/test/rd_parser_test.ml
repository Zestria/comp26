[@@@ocaml.text "/*"]

(* Copyright 2026, vibecaml team *)
(* SPDX-License-Identifier: LGPL-3.0-or-later *)
[@@@ocaml.text "/*"]

open Vibecaml
open Ast
open Samples

let fail format =
  Printf.ksprintf
    (fun message ->
       prerr_endline message;
       exit 1)
    format
;;

let parsed_expression source =
  match Rd_parser.parse_expr source with
  | Ok expression -> expression
  | Error message -> fail "не удалось разобрать %S: %s" source message
;;

let parsed_structure source =
  match Rd_parser.parse_structure source with
  | Ok structure -> structure
  | Error message -> fail "не удалось разобрать программу: %s" message
;;

let check_expression source expected =
  let actual = parsed_expression source in
  if actual <> expected
  then
    fail
      "для %S ожидалось %s, получено %s"
      source
      (Pprint.string_of_expression expected)
      (Pprint.string_of_expression actual)
;;

let () =
  check_expression "1 + 2 * 3" (binary "+" (int 1) (binary "*" (int 2) (int 3)));
  check_expression "1 - 2 - 3" (binary "-" (binary "-" (int 1) (int 2)) (int 3));
  check_expression "2 ** 3 ** 4" (binary "**" (int 2) (binary "**" (int 3) (int 4)));
  check_expression "f x y" (app (var "f") [ var "x"; var "y" ]);
  check_expression
    "let rec f x y = x + y and g = f 1 in g"
    (Expr_let
       ( Recursive
       , binding
           "f"
           (Expr_fun (Pat_var "x", [ Pat_var "y" ], binary "+" (var "x") (var "y")))
       , [ binding "g" (app (var "f") [ int 1 ]) ]
       , var "g" ));
  check_expression
    "if x < 2 then f x else g x"
    (Expr_if
       (binary "<" (var "x") (int 2), app (var "f") [ var "x" ], app (var "g") [ var "x" ]));
  check_expression
    "let (-1) = 0 in 0"
    (Expr_let
       ( Non_recursive
       , { binding_pattern = Pat_constant (Const_int (-1)); binding_expression = int 0 }
       , []
       , int 0 ));
  check_expression
    "let ( = ) = fun x -> x in ( = ) 1"
    (Expr_let
       ( Non_recursive
       , binding "=" (Expr_fun (Pat_var "x", [], var "x"))
       , []
       , app (var "=") [ int 1 ] ));
  check_expression "(-4611686018427387904)" (int min_int)
;;

let () =
  let source =
    "let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))\n"
    ^ "let rec fibk n k = if n < 2 then k n else fibk (n - 1) (fun a -> fibk (n - 2) \
       (fun b -> k (a + b)))"
  in
  match parsed_structure source with
  | [ _; _ ] -> ()
  | _ -> fail "эталонные программы должны дать два объявления"
;;

let () =
  match Rd_parser.parse_expr "let x =" with
  | Error message when String.length message > 0 -> ()
  | Error _ -> fail "пустое сообщение об ошибке"
  | Ok _ -> fail "некорректный ввод не должен разбираться"
;;
