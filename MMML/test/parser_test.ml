[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open QCheck

let var name = Variable name
let pvar name = PVariable name
let int value = Constant (Integer value)
let bool value = Constant (Boolean value)
let parse_expr source = Topdown_parser.parse_expression source
let parse_program source = Topdown_parser.parse_program source

let check_expr source expected =
  match parse_expr source with
  | Error error ->
    Test.fail_reportf "input %S failed: %s" source (Error_monad.show_error error)
  | Ok actual ->
    if actual <> expected
    then
      Test.fail_reportf
        "input %S@,expected: %s@,actual:   %s"
        source
        (show_expr expected)
        (show_expr actual)
;;

let check_program source expected =
  match parse_program source with
  | Error error ->
    Test.fail_reportf "input %S failed: %s" source (Error_monad.show_error error)
  | Ok actual ->
    if actual <> expected
    then
      Test.fail_reportf
        "input %S@,expected: %s@,actual:   %s"
        source
        (show_program expected)
        (show_program actual)
;;

let check_error source ~line ~column ~message =
  match parse_expr source with
  | Ok _ -> Test.fail_reportf "input %S unexpectedly parsed" source
  | Error error ->
    if
      error.location.line <> line
      || error.location.column <> column
      || error.message <> message
    then
      Test.fail_reportf
        "input %S@,expected line %d, column %d: %s@,actual: %s"
        source
        line
        column
        message
        (Error_monad.show_error error)
;;

let case name check =
  Test.make ~name ~count:1 unit (fun () ->
    check ();
    true)
;;

(** Constants, variables, unit, and parentheses are parsed as atoms. *)
let atoms =
  case "atoms" (fun () ->
    check_expr "1" (int 1);
    check_expr "true" (bool true);
    check_expr "false" (bool false);
    check_expr "()" (Constant Unit);
    check_expr "x" (var "x");
    check_expr "(x)" (var "x");
    check_expr "((1))" (int 1))
;;

(** Each priority level binds tighter than the previous one. *)
let precedence =
  case "operator precedence and associativity" (fun () ->
    check_expr "1 + 2 * 3" (infix "+" (int 1) (infix "*" (int 2) (int 3)));
    check_expr "2 * 3 + 4" (infix "+" (infix "*" (int 2) (int 3)) (int 4));
    check_expr "1 + (2 + 3)" (infix "+" (int 1) (infix "+" (int 2) (int 3)));
    check_expr "1 - 2 - 3" (infix "-" (infix "-" (int 1) (int 2)) (int 3));
    check_expr "8 / 4 / 2" (infix "/" (infix "/" (int 8) (int 4)) (int 2));
    check_expr
      "1 < 2 && 3 > 4"
      (infix "&&" (infix "<" (int 1) (int 2)) (infix ">" (int 3) (int 4)));
    check_expr
      "true || false && true"
      (infix "||" (bool true) (infix "&&" (bool false) (bool true)));
    check_expr "1 < 2 = false" (infix "=" (infix "<" (int 1) (int 2)) (bool false));
    check_expr "1 <> 2" (infix "<>" (int 1) (int 2));
    check_expr "1 <= 2" (infix "<=" (int 1) (int 2));
    check_expr "1 >= 2" (infix ">=" (int 1) (int 2)))
;;

(** Application associates to the left and binds tighter than operators. *)
let applications =
  case "application" (fun () ->
    check_expr "f x" (application (var "f") [ var "x" ]);
    check_expr "f x y" (application (var "f") [ var "x"; var "y" ]);
    check_expr "f (g x)" (application (var "f") [ application (var "g") [ var "x" ] ]);
    check_expr "f x + 1" (infix "+" (application (var "f") [ var "x" ]) (int 1));
    check_expr "(f) x" (application (var "f") [ var "x" ]))
;;

(** Unary minus and plus fold integer literals and prefix other operands. *)
let unary =
  case "unary operators" (fun () ->
    check_expr "-1" (int (-1));
    check_expr "+1" (int 1);
    check_expr "-x" (prefix "~-" (var "x"));
    check_expr "-(1 + 2)" (prefix "~-" (infix "+" (int 1) (int 2)));
    check_expr "1 - -2" (infix "-" (int 1) (int (-2))))
;;

(** Several [fun] parameters become nested abstractions. *)
let lambdas =
  case "lambda" (fun () ->
    check_expr "fun x -> x" (function_ [ pvar "x" ] (var "x"));
    check_expr
      "fun x y -> x + y"
      (function_ [ pvar "x"; pvar "y" ] (infix "+" (var "x") (var "y")));
    check_expr "fun _ -> 1" (function_ [ PWildcard ] (int 1));
    check_expr "fun (x) -> x" (function_ [ pvar "x" ] (var "x")))
;;

(** Conditionals nest without parentheses. *)
let conditionals =
  case "if then else" (fun () ->
    check_expr "if true then 1 else 2" (If_then_else (bool true, int 1, int 2));
    check_expr
      "if 1 < 2 then x else y"
      (If_then_else (infix "<" (int 1) (int 2), var "x", var "y"));
    check_expr
      "if true then (if false then 1 else 2) else 3"
      (If_then_else (bool true, If_then_else (bool false, int 1, int 2), int 3)))
;;

(** Local bindings support wildcards and the parameterized sugar. *)
let let_in =
  case "let in" (fun () ->
    check_expr "let x = 1 in x" (Let_in (Nonrecursive, pvar "x", int 1, var "x"));
    check_expr
      "let f x = x in f"
      (Let_in (Nonrecursive, pvar "f", function_ [ pvar "x" ] (var "x"), var "f"));
    check_expr
      "let _ = f 1 in 2"
      (Let_in (Nonrecursive, PWildcard, application (var "f") [ int 1 ], int 2)))
;;

(** Several top-level definitions are read until end of input. *)
let program_case =
  case "program" (fun () ->
    check_program
      "let x = 1 let y = x"
      [ Definition (Nonrecursive, pvar "x", int 1)
      ; Definition (Nonrecursive, pvar "y", var "x")
      ])
;;

(** A complete CPS factorial definition is parsed. *)
let cps_factorial =
  case "cps factorial program" (fun () ->
    let fac_body =
      If_then_else
        ( infix "<" (var "n") (int 2)
        , application (var "k") [ int 1 ]
        , application
            (var "fac")
            [ infix "-" (var "n") (int 1)
            ; function_
                [ pvar "a" ]
                (application (var "k") [ infix "*" (var "a") (var "n") ])
            ] )
    in
    check_program
      "let rec fac n k = if n < 2 then k 1 else fac (n - 1) (fun a -> k (a * n))"
      [ Definition (Recursive, pvar "fac", function_ [ pvar "n"; pvar "k" ] fac_body) ])
;;

(** A complete CPS fibonacci definition is parsed. *)
let cps_fibonacci =
  case "cps fibonacci program" (fun () ->
    let fib_body =
      If_then_else
        ( infix "<" (var "n") (int 2)
        , application (var "k") [ var "n" ]
        , application
            (var "fib")
            [ infix "-" (var "n") (int 1)
            ; function_
                [ pvar "a" ]
                (application
                   (var "fib")
                   [ infix "-" (var "n") (int 2)
                   ; function_
                       [ pvar "b" ]
                       (application (var "k") [ infix "+" (var "a") (var "b") ])
                   ])
            ] )
    in
    check_program
      "let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
       -> k (a + b)))"
      [ Definition (Recursive, pvar "fib", function_ [ pvar "n"; pvar "k" ] fib_body) ])
;;

(** Syntax and lexical failures carry a positioned diagnostic. *)
let syntax_errors =
  case "syntax errors" (fun () ->
    check_error "1 +" ~line:1 ~column:4 ~message:"unexpected end of input";
    check_error "(1" ~line:1 ~column:3 ~message:"unexpected end of input";
    check_error "1 2)" ~line:1 ~column:4 ~message:"unexpected token \")\"";
    check_error "let = 1 in 2" ~line:1 ~column:5 ~message:"unexpected token \"=\"";
    check_error
      "9name"
      ~line:1
      ~column:1
      ~message:"invalid identifier \"9name\": identifiers cannot start with a digit")
;;

let () =
  exit
    (QCheck_runner.run_tests
       [ atoms
       ; precedence
       ; applications
       ; unary
       ; lambdas
       ; conditionals
       ; let_in
       ; program_case
       ; cps_factorial
       ; cps_fibonacci
       ; syntax_errors
       ])
;;
