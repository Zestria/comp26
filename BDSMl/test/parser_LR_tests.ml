(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open BDSMl_frontend
open Ast
open LL_parser.Parser

let parse_LL source =
  match parse_string_LL source with
  | Ok program -> Ok program
  | Error (`lexer_error message | `parse_error message) -> Error message
;;

let expect_program source expected =
  List.iter
    (fun (name, parse) ->
       match parse source with
       | Ok actual when actual = expected -> ()
       | Ok actual ->
         failwith
           (Printf.sprintf
              "%s: %S: expected %s, got %s"
              name
              source
              (Printer.print_ast expected)
              (Printer.print_ast actual))
       | Error message -> failwith (Printf.sprintf "%s: %S: %s" name source message))
    [ "LL", parse_LL; "LR", Parse.parse ]
;;

let regression_tests () =
  expect_program "" [];
  expect_program " \t\n" [];
  expect_program ";;;;" [];
  expect_program "1;;;;2" [ StrEval (Int 1); StrEval (Int 2) ];
  List.iter
    (fun (source, expected) -> expect_program source [ StrEval expected ])
    [ "()", Unit
    ; "( )", Unit
    ; "( \t\n )", Unit
    ; "f ( )", App (Var "f", Unit)
    ; "fun ( ) -> ( )", Abs (PUnit, Unit)
    ; "fun ( \t ) x -> x", Abs (PUnit, Abs (PVar "x", Var "x"))
    ; "fun () _ x -> x", Abs (PUnit, Abs (PUnderscore, Abs (PVar "x", Var "x")))
    ; "f x y", App (App (Var "f", Var "x"), Var "y")
    ; "-f x * 2", BinOp (Mult, UnOp (Neg, App (Var "f", Var "x")), Int 2)
    ; "not not true", UnOp (Not, UnOp (Not, Bool true))
    ; "1 + 2 * 3", BinOp (Plus, Int 1, BinOp (Mult, Int 2, Int 3))
    ; "8 - 3 - 1", BinOp (Minus, BinOp (Minus, Int 8, Int 3), Int 1)
    ; "8 / 2 / 2", BinOp (Div, BinOp (Div, Int 8, Int 2), Int 2)
    ; "1 < 2 = true", BinOp (Equal, BinOp (Less, Int 1, Int 2), Bool true)
    ; "true || false && true", BinOp (Or, Bool true, BinOp (And, Bool false, Bool true))
    ; "true || false || true", BinOp (Or, Bool true, BinOp (Or, Bool false, Bool true))
    ; "true && false && true", BinOp (And, Bool true, BinOp (And, Bool false, Bool true))
    ; ( "let x = let y = 1 in y in x"
      , Let (NonRec, PVar "x", Let (NonRec, PVar "y", Int 1, Var "y"), Var "x") )
    ; ( "if true then if false then 1 else 2 else 3"
      , If (Bool true, If (Bool false, Int 1, Int 2), Int 3) )
    ];
  expect_program
    "let rec f () _ x = x;; let _ = f () 1 2;; ()"
    [ StrLet (Rec, PVar "f", Abs (PUnit, Abs (PUnderscore, Abs (PVar "x", Var "x"))))
    ; StrLet (NonRec, PUnderscore, App (App (App (Var "f", Unit), Int 1), Int 2))
    ; StrEval Unit
    ];
  expect_program "let () = ();;" [ StrLet (NonRec, PUnit, Unit) ];
  expect_program "let ( ) = ( )" [ StrLet (NonRec, PUnit, Unit) ];
  expect_program "let f ( ) = ( )" [ StrLet (NonRec, PVar "f", Abs (PUnit, Unit)) ];
  let right_minus = with_associativity default_config Minus Right in
  (match parse_string_LL ~config:right_minus "8 - 3 - 1" with
   | Ok [ StrEval (BinOp (Minus, Int 8, BinOp (Minus, Int 3, Int 1))) ] -> ()
   | _ -> failwith "LL associativity override for minus was ignored");
  let left_or = with_associativity default_config Or Left in
  (match parse_string_LL ~config:left_or "true || false || true" with
   | Ok [ StrEval (BinOp (Or, BinOp (Or, Bool true, Bool false), Bool true)) ] -> ()
   | _ -> failwith "LL associativity override for or was ignored");
  List.iter
    (fun source ->
       match parse_string_LL source, Parse.parse source with
       | Error _, Error _ -> ()
       | _ -> failwith (Printf.sprintf "Expected a syntax error on %S" source))
    [ "fun -> 1"
    ; "fun x x"
    ; "let = 1"
    ; "let x ="
    ; "let x = 1 in"
    ; "if true then 1"
    ; "if true 1 else 2"
    ; "(1"
    ; "1)"
    ; "1 +"
    ; "f _"
    ];
  (* Preserve the original Menhir grammar beyond the shared LL subset. *)
  List.iter
    (fun (source, expected) ->
       if Parse.parse source <> Ok expected
       then failwith (Printf.sprintf "Original LR syntax changed on %S" source))
    [ "", []
    ; " \t\n", []
    ; ";;", []
    ; "1;;;;2", [ StrEval (Int 1); StrEval (Int 2) ]
    ; ( "let x = 1 let y = 2"
      , [ StrLet (NonRec, PVar "x", Int 1); StrLet (NonRec, PVar "y", Int 2) ] )
    ; "( )", [ StrEval Unit ]
    ; "fun ( ) -> ()", [ StrEval (Abs (PUnit, Unit)) ]
    ; ( "1 + if true then 2 else 3"
      , [ StrEval (BinOp (Plus, Int 1, If (Bool true, Int 2, Int 3))) ] )
    ; ( "true || false || true"
      , [ StrEval (BinOp (Or, Bool true, BinOp (Or, Bool false, Bool true))) ] )
    ; ( "true && false && true"
      , [ StrEval (BinOp (And, Bool true, BinOp (And, Bool false, Bool true))) ] )
    ];
  List.iter
    (fun source ->
       if LL_parser.Lexer.tokenize source <> LR_parser.Lexer.tokenize source
       then failwith (Printf.sprintf "Lexers disagree on %S" source))
    [ "Foo _foo x1 123abc iffy inlet lettuce trueish funny recover thenable nothing"
    ; "() ( ) -> - <> <= < >= > && || ;;"
    ; "\r\n\r\n@"
    ; "a\r  @"
    ; "a\n\t@"
    ; "x'"
    ];
  (match Parse.parse "(* outer\n (* nested *) *) let x = 1;;x" with
   | Ok [ StrLet (NonRec, PVar "x", Int 1); StrEval (Var "x") ] -> ()
   | _ -> failwith "Nested comments were not preserved");
  List.iter
    (fun (source, expected) ->
       match Parse.parse source with
       | Error message when message = expected -> ()
       | _ -> failwith (Printf.sprintf "Incorrect lexer error on %S" source))
    [ "\r\n@", "2:1: unexpected character '@'"
    ; "(*\r\n*)\r@", "3:1: unexpected character '@'"
    ; "  (* unterminated", "1:3: unterminated comment"
    ];
  (match Parse.parse "let x =\r\n)" with
   | Error "Line 2, column 1: Syntax error" -> ()
   | _ -> failwith "Incorrect syntax error location");
  (match parse_string_LL "1) @" with
   | Error (`parse_error _) -> ()
   | _ -> failwith "LL parser read past a syntax error");
  (match Parse.parse "1) @" with
   | Error "Line 1, column 2: Syntax error" -> ()
   | _ -> failwith "LR parser read past a syntax error");
  (match parse_string_LL "1 @" with
   | Error (`lexer_error "1:3: unexpected character '@'") -> ()
   | _ -> failwith "LL parser did not report a consumed lexer error");
  (match Parse.parse "fun _ -> ()" with
   | Ok [ StrEval (Abs (PUnderscore, Unit)) ] -> ()
   | _ -> failwith "Parse.parse compatibility entry point failed");
  match
    LR_parser.Parser.expression LR_parser.Lexer.token (Lexing.from_string "1 + 2")
  with
  | BinOp (Plus, Int 1, Int 2) -> ()
  | _ -> failwith "LR_parser.Parser.expression entry point failed"
;;

(* Fully parenthesize generated expressions so the expected AST is unambiguous. *)
let binop_text = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "="
  | NotEqual -> "<>"
  | More -> ">"
  | Less -> "<"
  | EMore -> ">="
  | ELess -> "<="
  | And -> "&&"
  | Or -> "||"
;;

let rec_flag_text = function
  | NonRec -> ""
  | Rec -> "rec "
;;

let rec source_of_expr = function
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var x -> x
  | BinOp (op, left, right) ->
    Printf.sprintf
      "(%s %s %s)"
      (source_of_expr left)
      (binop_text op)
      (source_of_expr right)
  | UnOp (op, body) ->
    Printf.sprintf "(%s %s)" (if op = Neg then "-" else "not") (source_of_expr body)
  | If (cond, yes, no) ->
    Printf.sprintf
      "(if %s then %s else %s)"
      (source_of_expr cond)
      (source_of_expr yes)
      (source_of_expr no)
  | Let (flag, pattern, value, body) ->
    Printf.sprintf
      "(let %s%s = %s in %s)"
      (rec_flag_text flag)
      (Printer.print_pattern pattern)
      (source_of_expr value)
      (source_of_expr body)
  | Abs (pattern, body) ->
    Printf.sprintf "(fun %s -> %s)" (Printer.print_pattern pattern) (source_of_expr body)
  | App (fn, arg) -> Printf.sprintf "(%s %s)" (source_of_expr fn) (source_of_expr arg)
;;

let source_of_program program =
  let source_of_item = function
    | StrEval expr -> source_of_expr expr
    | StrLet (flag, pattern, value) ->
      Printf.sprintf
        "let %s%s = %s"
        (rec_flag_text flag)
        (Printer.print_pattern pattern)
        (source_of_expr value)
  in
  String.concat ";;\n" (List.map source_of_item program)
;;

let gen_program =
  let open QCheck.Gen in
  let ident = oneof_list [ "x"; "y"; "k"; "_foo"; "fooBar"; "iffy" ] in
  let pattern = oneof [ map (fun x -> PVar x) ident; return PUnit; return PUnderscore ] in
  let flag = oneof_list [ NonRec; Rec ] in
  let op =
    oneof_list
      [ Plus; Minus; Mult; Div; Equal; NotEqual; More; Less; EMore; ELess; And; Or ]
  in
  let atom =
    oneof
      [ return Unit
      ; map (fun n -> Int n) (int_bound 1000)
      ; map (fun b -> Bool b) bool
      ; map (fun x -> Var x) ident
      ]
  in
  let rec expr depth =
    if depth = 0
    then atom
    else (
      let sub = expr (depth - 1) in
      oneof_weighted
        [ 3, atom
        ; 2, map3 (fun op left right -> BinOp (op, left, right)) op sub sub
        ; 1, map2 (fun op body -> UnOp (op, body)) (oneof_list [ Neg; Not ]) sub
        ; 1, map3 (fun cond yes no -> If (cond, yes, no)) sub sub sub
        ; ( 1
          , map3
              (fun (flag, pattern) value body -> Let (flag, pattern, value, body))
              (pair flag pattern)
              sub
              sub )
        ; 1, map2 (fun pattern body -> Abs (pattern, body)) pattern sub
        ; 2, map2 (fun fn arg -> App (fn, arg)) sub sub
        ])
  in
  let item =
    oneof
      [ map (fun expr -> StrEval expr) (expr 4)
      ; map3 (fun flag pattern body -> StrLet (flag, pattern, body)) flag pattern (expr 4)
      ]
  in
  list_size (int_range 1 4) item
;;

let () =
  regression_tests ();
  let programs = QCheck.make ~print:source_of_program gen_program in
  let tokens =
    QCheck.make
      ~print:(Printf.sprintf "%S")
      QCheck.Gen.(
        map
          (String.concat " ")
          (list_size
             (int_bound 40)
             (oneof_list
                [ "let"
                ; "rec"
                ; "in"
                ; "fun"
                ; "->"
                ; "if"
                ; "then"
                ; "else"
                ; "true"
                ; "false"
                ; "not"
                ; "()"
                ; "("
                ; ")"
                ; "_"
                ; ";;"
                ; "42"
                ; "x"
                ; "X1"
                ; "_foo"
                ; "iffy"
                ; "123abc"
                ; "+"
                ; "-"
                ; "*"
                ; "/"
                ; "="
                ; "<>"
                ; ">"
                ; "<"
                ; ">="
                ; "<="
                ; "&&"
                ; "||"
                ])))
  in
  QCheck_runner.run_tests_main
    [ QCheck.Test.make ~name:"LL/LR AST round trip" ~count:2000 programs (fun expected ->
        let source = source_of_program expected in
        parse_string_LL source = Ok expected && Parse.parse source = Ok expected)
    ; QCheck.Test.make ~name:"LL/LR token agreement" ~count:2000 tokens (fun source ->
        LL_parser.Lexer.tokenize source = LR_parser.Lexer.tokenize source)
    ]
;;
