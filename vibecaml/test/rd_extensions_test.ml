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

let config = { Config.default_extensions with glued_keywords = true }

let parsed_expression source =
  match Rd_parser.parse_expr config source with
  | Ok expression -> expression
  | Error message -> fail "не удалось разобрать %S: %s" source message
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
  check_expression
    "letx = 1 andy = 2 in x + y"
    (Expr_let
       ( Non_recursive
       , binding "x" (int 1)
       , [ binding "y" (int 2) ]
       , binary "+" (var "x") (var "y") ));
  check_expression
    "letrecf x y = x + y andg = f 1 ing"
    (Expr_let
       ( Recursive
       , binding
           "f"
           (Expr_fun (Pat_var "x", [ Pat_var "y" ], binary "+" (var "x") (var "y")))
       , [ binding "g" (app (var "f") [ int 1 ]) ]
       , var "g" ));
  check_expression
    "ifx=y theny elsex"
    (Expr_if (binary "=" (var "x") (var "y"), var "y", var "x"));
  check_expression
    "letid = (funx -> x) inid"
    (Expr_let
       (Non_recursive, binding "id" (Expr_fun (Pat_var "x", [], var "x")), [], var "id"));
  (* В задании никак не описано, что в таком случае делать,
     оставил "жадное" поведение: лексер будет отцеплять токены от слова, пока может
     "thentrueelsefalse" -> "then|true|else|false" -> [then] [true] [else] [false] *)
  check_expression
    "if x <> 0 thentrueelsefalse"
    (Expr_if
       ( binary "<>" (var "x") (int 0)
       , Expr_constant (Const_bool true)
       , Expr_constant (Const_bool false) ))
;;
