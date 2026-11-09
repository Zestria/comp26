[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Ast = MLate_frontend.Ast
module Topdown = MLate_frontend.Topdown
module Parse = MLate_frontend.Parse
open Ast

let parsers = [ "Angstrom", Topdown.parse; "Menhir", Parse.parse ]

let check name expected source =
  List.iter
    (fun (parser_name, parse) ->
       match parse source with
       | Ok actual when actual = expected -> ()
       | Ok _ -> failwith (Printf.sprintf "%s / %s: unexpected AST" parser_name name)
       | Error (`Parsing_error message) ->
         failwith (Printf.sprintf "%s / %s: %s" parser_name name message))
    parsers
;;

let check_error name source =
  List.iter
    (fun (parser_name, parse) ->
       match parse source with
       | Error (`Parsing_error _) -> ()
       | Ok _ -> failwith (Printf.sprintf "%s / %s: expected an error" parser_name name))
    parsers
;;

let declaration ?(flag = Nonrec) name value = { flag; name; value }

let check_value name value source =
  check name [ declaration "result" value ] ("let result = " ^ source)
;;

let () =
  List.iter
    (fun (normal, glued) ->
       let expected =
         match Topdown.parse normal with
         | Ok program -> program
         | Error _ -> failwith "invalid glued-keyword reference program"
       in
       if Topdown.parse_with ~glued_keywords:true glued <> Ok expected
       then failwith ("glued keywords: " ^ glued);
       check_error "glued syntax is opt-in and topdown-only" glued)
    [ "let x = 1", "letx=1"
    ; "let f = fun x -> x", "letf=funx->x"
    ; "let x = let y = 1 in y", "letx=lety=1iny"
    ; "let rec f n = if n < 1 then n else f (n - 1)", "letrecf n=ifn<1thennelsef(n-1)"
    ; "let x=1 let y=2", "letx=1lety=2"
    ; ( "let rec fact n k = if n < 1 then k 1 else fact (n-1) (fun r -> k (n*r))"
      , "letrecfact n k=ifn<1thenk 1elsefact(n-1)(funr->k(n*r))" )
    ; ( "let rec fib n k = if n < 2 then k n else fib (n-1) (fun a -> fib (n-2) (fun b \
         -> k (a+b)))"
      , "letrecfib n k=ifn<2thenk nelsefib(n-1)(funa->fib(n-2)(funb->k(a+b)))" )
    ];
  List.iter
    (fun source ->
       match Topdown.parse_with ~glued_keywords:true source with
       | Error _ -> ()
       | Ok _ -> failwith ("invalid glued program accepted: " ^ source))
    [ "letx=123abc"; "letx=0x10"; "letx=" ^ String.make 100 '9' ]
;;

let () =
  check "empty program" [] " \t\r\n";
  check
    "top-level declarations"
    [ declaration "x" (Int 1); declaration "y" (Binop (Plus, Var "x", Int 2)) ]
    "let x=1\nlet y=x+2";
  check
    "keyword prefixes and identifier suffixes"
    [ declaration "record" (Int 1)
    ; declaration "recx" (Var "record")
    ; declaration "ifthenelse" (Var "recx")
    ; declaration "x_A1" (Var "ifthenelse")
    ]
    "let record=1 let recx=record let ifthenelse=recx let x_A1=ifthenelse";
  check_value "decimal literal" (Int 12345) "0012345";
  check_value "largest integer" (Int max_int) (string_of_int max_int);
  check_value "whitespace" (Var "x") " \n (x) \r\t";
  check_value
    "application is left associative"
    (App (App (Var "f", Var "x"), Var "y"))
    "f(x)(y)";
  check_value
    "operator precedence"
    (Binop (Lt, Binop (Plus, App (Var "f", Var "x"), Binop (Times, Int 2, Int 3)), Int 9))
    "f x+2*3<9";
  check_value
    "subtraction is left associative"
    (Binop (Minus, Binop (Minus, Int 9, Int 3), Int 1))
    "9-3-1";
  check_value
    "multiplication is left associative"
    (Binop (Times, Binop (Times, Int 2, Int 3), Int 4))
    "2*3*4";
  check_value
    "function parameters"
    (Fun ("x", Fun ("y", App (Var "x", Var "y"))))
    "fun x y->x y";
  check
    "binding parameters"
    [ declaration ~flag:Rec "f" (Fun ("x", Fun ("y", Var "x"))) ]
    "let rec f x y=x";
  check_value
    "local binding"
    (Let (declaration "x" (Int 1), Binop (Plus, Var "x", Int 2)))
    "let x=1 in x+2";
  check_value
    "nested binding in a value"
    (Let (declaration "x" (Let (declaration "y" (Int 1), Var "y")), Var "x"))
    "let x=let y=1 in y in x";
  check_value
    "local recursive binding"
    (Let (declaration ~flag:Rec "f" (Fun ("x", App (Var "f", Var "x"))), Var "f"))
    "let rec f x=f x in f";
  check_value
    "nested conditional"
    (If (Var "a", If (Var "b", Int 1, Int 2), Int 3))
    "if a then if b then 1 else 2 else 3";
  check_value
    "conditional after an operator with parentheses"
    (Binop (Plus, Int 1, If (Var "c", Int 2, Int 3)))
    "1+(if c then 2 else 3)";
  check_value
    "parenthesized function argument"
    (App (Var "f", Fun ("x", Var "x")))
    "f (fun x->x)";
  check_value
    "parenthesized conditional argument"
    (App (Var "f", If (Var "c", Int 1, Int 2)))
    "f (if c then 1 else 2)";
  check_value
    "parenthesized local binding argument"
    (App (Var "f", Let (declaration "x" (Int 1), Var "x")))
    "f (let x=1 in x)";
  check_value
    "parenthesized comparison"
    (Binop (Lt, Binop (Lt, Int 1, Int 2), Int 3))
    "(1<2)<3";
  List.iter
    (fun (name, source) -> check_error name source)
    [ "bare expression at top level", "1+2"
    ; "keyword as identifier", "let rec=1"
    ; "uppercase initial", "let X=1"
    ; "underscore initial", "let _x=1"
    ; "integer overflow", "let x=" ^ String.make 100 '9'
    ; "glued integer and keyword", "let x=1in x"
    ; "glued integer and identifier", "let x=123abc"
    ; "glued integer and uppercase", "let x=1A"
    ; "glued integer and underscore", "let x=1_2"
    ; "hexadecimal literal", "let x=0x10"
    ; "comparison chain", "let x=1<2<3"
    ; "equality expression", "let x=1=2"
    ; "unsupported comparison", "let x=1<=2"
    ; "division", "let x=1/2"
    ; "unary minus", "let x= -1"
    ; "function as bare argument", "let x=f fun y->y"
    ; "conditional as bare argument", "let x=f if c then 1 else 2"
    ; "binding as bare argument", "let x=f let y=1 in y"
    ; "conditional as bare operand", "let x=1+if c then 2 else 3"
    ; "function as bare operand", "let x=1+fun y->y"
    ; "binding as bare operand", "let x=1+let y=2 in y"
    ; "empty function parameters", "let f=fun ->1"
    ; "missing arrow", "let f=fun x x"
    ; "missing in", "let x=(let y=1)"
    ; "missing else", "let x=if c then 1"
    ; "missing operand", "let x=1+"
    ; "missing parenthesis", "let x=(1"
    ; "trailing junk", "let x=1 @"
    ; "unfinished next declaration", "let x=1 let y="
    ];
  (match Parse.parse "let x=1\nlet y=@" with
   | Error (`Parsing_error "line 2, column 7: unexpected character '@'") -> ()
   | _ -> failwith "lexer error location");
  match Parse.parse "let x=1\nlet y=(1" with
  | Error (`Parsing_error "line 2, column 9: unexpected end of input") -> ()
  | _ -> failwith "parser error location"
;;

let () =
  let app2 f x y = App (App (Var f, x), y) in
  let fact =
    Fun
      ( "n"
      , Fun
          ( "k"
          , If
              ( Binop (Lt, Var "n", Int 1)
              , App (Var "k", Int 1)
              , app2
                  "fact"
                  (Binop (Minus, Var "n", Int 1))
                  (Fun ("r", App (Var "k", Binop (Times, Var "n", Var "r")))) ) ) )
  in
  let fib =
    Fun
      ( "n"
      , Fun
          ( "k"
          , If
              ( Binop (Lt, Var "n", Int 2)
              , App (Var "k", Var "n")
              , app2
                  "fib"
                  (Binop (Minus, Var "n", Int 1))
                  (Fun
                     ( "a"
                     , app2
                         "fib"
                         (Binop (Minus, Var "n", Int 2))
                         (Fun ("b", App (Var "k", Binop (Plus, Var "a", Var "b")))) )) )
          ) )
  in
  check
    "CPS factorial and Fibonacci"
    [ declaration ~flag:Rec "fact" fact
    ; declaration ~flag:Rec "fib" fib
    ; declaration "fact_result" (app2 "fact" (Int 5) (Fun ("x", Var "x")))
    ; declaration "fib_result" (app2 "fib" (Int 10) (Fun ("x", Var "x")))
    ]
    {|let rec fact n k =
        if n < 1 then k 1
        else fact (n - 1) (fun r -> k (n * r))
      let rec fib n k =
        if n < 2 then k n
        else fib (n - 1) (fun a ->
          fib (n - 2) (fun b -> k (a + b)))
      let fact_result = fact 5 (fun x -> x)
      let fib_result = fib 10 (fun x -> x)|}
;;
