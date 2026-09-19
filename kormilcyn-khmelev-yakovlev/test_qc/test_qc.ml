[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Kormilcyn_khmelev_yakovlev_lib

module Parse = struct
  type result = (Ast.expr, string) Stdlib.result

  let menhir source =
    try Ok (Main.parse_with_menhir source) with
    | Parser.Error -> Error "Parser.Error"
    | exn -> Error (Printexc.to_string exn)
  ;;

  let combinator source =
    match Main.parse_with_comb source with
    | Ok ast -> Ok ast
    | Error error -> Error (Format.asprintf "%a" Parser_comb.pp_error error)
  ;;
end

module Ast_gen = struct
  open QCheck
  let names =
    [ "x"
    ; "y"
    ; "z"
    ; "n"
    ; "k"
    ; "f"
    ; "g"
    ; "x0"
    ; "acc"
    ; "tmp"
    ; "value_2"
    ; "camelCase"
    ; "result"
    ]
  ;;

  let name = Gen.oneof_list names

  let binop =
    Gen.oneof_list
      [ Ast.Add; Ast.Sub; Ast.Mul; Ast.Div; Ast.Eq; Ast.Lt; Ast.Gt; Ast.Le; Ast.Ge ]
  ;;

  let leaf =
    Gen.oneof_weighted
      [ 2, Gen.map (fun n -> Ast.Const n) (Gen.int_range 0 1000)
      ; 1, Gen.map (fun name -> Ast.Var name) name
      ]
  ;;

  let rec expr_sized size =
    if size <= 0
    then leaf
    else (
      let sub = expr_sized (size / 2) in
      let sub3 = expr_sized (size / 3) in
      Gen.oneof_weighted
        [ 5, leaf
        ; 2, Gen.map (fun expr -> Ast.Neg expr) sub
        ; 2, Gen.map2 (fun name body -> Ast.Abs (name, body)) name sub
        ; 3, Gen.map2 (fun fn arg -> Ast.App (fn, arg)) sub sub
        ; ( 5
          , Gen.map3
              (fun op left right -> Ast.BinOp (op, left, right))
              binop
              sub
              sub )
        ; 2, Gen.map3 (fun cond yes no -> Ast.If (cond, yes, no)) sub3 sub3 sub3
        ; 2, Gen.map3 (fun name value body -> Ast.Let (name, value, body)) name sub sub
        ; ( 2
          , Gen.map
              (fun (fn, param, fn_body, body) -> Ast.LetRec (fn, param, fn_body, body))
              (Gen.quad name name sub3 sub3) )
        ])
  ;;

  let expr = Gen.sized expr_sized

  let rec shrink_expr expr =
    let open Iter in
    match expr with
    | Ast.Const 0 | Ast.Var _ -> empty
    | Ast.Const n -> of_list [ Ast.Const 0; Ast.Const (n / 2) ]
    | Ast.Neg inner ->
      return inner <+> (shrink_expr inner >>= fun inner -> return (Ast.Neg inner))
    | Ast.Abs (name, body) ->
      return body <+> (shrink_expr body >>= fun body -> return (Ast.Abs (name, body)))
    | Ast.App (fn, arg) ->
      of_list [ fn; arg ]
      <+> (shrink_expr fn >>= fun fn -> return (Ast.App (fn, arg)))
      <+> (shrink_expr arg >>= fun arg -> return (Ast.App (fn, arg)))
    | Ast.BinOp (op, left, right) ->
      of_list [ left; right ]
      <+> (shrink_expr left >>= fun left -> return (Ast.BinOp (op, left, right)))
      <+> (shrink_expr right >>= fun right -> return (Ast.BinOp (op, left, right)))
    | Ast.If (cond, yes, no) ->
      of_list [ cond; yes; no ]
      <+> (shrink_expr cond >>= fun cond -> return (Ast.If (cond, yes, no)))
      <+> (shrink_expr yes >>= fun yes -> return (Ast.If (cond, yes, no)))
      <+> (shrink_expr no >>= fun no -> return (Ast.If (cond, yes, no)))
    | Ast.Let (name, value, body) ->
      of_list [ value; body ]
      <+> (shrink_expr value >>= fun value -> return (Ast.Let (name, value, body)))
      <+> (shrink_expr body >>= fun body -> return (Ast.Let (name, value, body)))
    | Ast.LetRec (fn, param, fn_body, body) ->
      of_list [ fn_body; body ]
      <+> (shrink_expr fn_body
           >>= fun fn_body -> return (Ast.LetRec (fn, param, fn_body, body)))
      <+> (shrink_expr body
           >>= fun body -> return (Ast.LetRec (fn, param, fn_body, body)))
  ;;

  let print expr =
    Format.asprintf "AST: %a\nSource: %S" Ast.pp_expr expr (Printast.show expr)
  ;;

  let arbitrary = QCheck.make ~print ~shrink:shrink_expr expr
end

let expect_parse ~parser_name parser source expected =
  match parser source with
  | Ok actual when actual = expected -> ()
  | Ok actual ->
    QCheck.Test.fail_reportf
      "%s produced a different AST for %S\nexpected: %a\nactual:   %a"
      parser_name
      source
      Ast.pp_expr
      expected
      Ast.pp_expr
      actual
  | Error error ->
    QCheck.Test.fail_reportf "%s rejected %S: %s" parser_name source error
;;

let expect_both source expected =
  expect_parse ~parser_name:"menhir" Parse.menhir source expected;
  expect_parse ~parser_name:"combinator" Parse.combinator source expected
;;

let fixed_case name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    expect_both source expected;
    true)
;;

let operator_cases =
  let open Ast in
  [ "add", "+", Add
  ; "subtract", "-", Sub
  ; "multiply", "*", Mul
  ; "divide", "/", Div
  ; "equal", "=", Eq
  ; "less", "<", Lt
  ; "greater", ">", Gt
  ; "less-or-equal", "<=", Le
  ; "greater-or-equal", ">=", Ge
  ]
  |> List.map (fun (name, symbol, op) ->
    fixed_case ("operator: " ^ name) ("1 " ^ symbol ^ " 2") (BinOp (op, Const 1, Const 2)))
;;

let syntax_cases =
  let open Ast in
  [ fixed_case "constant" "67" (Const 67)
  ; fixed_case "variable" "result" (Var "result")
  ; fixed_case "unary minus" "-67" (Neg (Const 67))
  ; fixed_case
      "operator precedence"
      "1 + 2 * 3"
      (BinOp (Add, Const 1, BinOp (Mul, Const 2, Const 3)))
  ; fixed_case
      "operator associativity"
      "1 - 2 - 3"
      (BinOp (Sub, BinOp (Sub, Const 1, Const 2), Const 3))
  ; fixed_case "single argument function" "fun x -> x" (Abs ("x", Var "x"))
  ; fixed_case
      "multiple argument function"
      "fun x y -> x"
      (Abs ("x", Abs ("y", Var "x")))
  ; fixed_case
      "function application"
      "f x y"
      (App (App (Var "f", Var "x"), Var "y"))
  ; fixed_case
      "nested function application"
      "f (g x)"
      (App (Var "f", App (Var "g", Var "x")))
  ; fixed_case "conditional" "if x then y else z" (If (Var "x", Var "y", Var "z"))
  ; fixed_case "let binding" "let x = 1 in x" (Let ("x", Const 1, Var "x"))
  ; fixed_case
      "recursive binding"
      "let rec f x = f x in f"
      (LetRec ("f", "x", App (Var "f", Var "x"), Var "f"))
  ]
  @ operator_cases
;;

let cps_factorial =
  {|
let rec fact n k =
  if n = 0 then k 1
  else fact (n - 1) (fun x -> k (n * x))
in fact 5 (fun x -> x)
|}
;;

let cps_fibonacci =
  {|
let rec fib n k =
  if n <= 1 then k n
  else fib (n - 1) (fun x -> fib (n - 2) (fun y -> k (x + y)))
in fib 10 (fun x -> x)
|}
;;

let successful_for_both name source =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    match Parse.menhir source, Parse.combinator source with
    | Ok menhir, Ok combinator when menhir = combinator -> true
    | Ok menhir, Ok combinator ->
      QCheck.Test.fail_reportf
        "parsers disagree for %S\nmenhir:     %a\ncombinator: %a"
        source
        Ast.pp_expr
        menhir
        Ast.pp_expr
        combinator
    | Error error, _ -> QCheck.Test.fail_reportf "menhir rejected %S: %s" source error
    | _, Error error -> QCheck.Test.fail_reportf "combinator rejected %S: %s" source error)
;;

let invalid_case name source =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let expect_rejection parser_name = function
      | Error _ -> ()
      | Ok ast ->
        QCheck.Test.fail_reportf
          "%s unexpectedly accepted %S as %a"
          parser_name
          source
          Ast.pp_expr
          ast
    in
    expect_rejection "menhir" (Parse.menhir source);
    expect_rejection "combinator" (Parse.combinator source);
    true)
;;

let invalid_cases =
  [ invalid_case "reject unclosed parenthesis" "(1"
  ; invalid_case "reject incomplete conditional" "if x then y"
  ; invalid_case "reject incomplete let" "let x = 1"
  ; invalid_case "reject function without parameter" "fun -> 1"
  ; invalid_case "reject incomplete operation" "1 +"
  ]
;;

let round_trip name parser =
  QCheck.Test.make ~name ~count:500 Ast_gen.arbitrary (fun expected ->
    let source = Printast.show expected in
    expect_parse ~parser_name:name parser source expected;
    true)
;;

let tests =
  syntax_cases
  @ [ successful_for_both "CPS factorial" cps_factorial
    ; successful_for_both "CPS fibonacci" cps_fibonacci
    ]
  @ invalid_cases
  @ [ round_trip "menhir round-trip" Parse.menhir
    ; round_trip "combinator round-trip" Parse.combinator
    ]
;;

let () = QCheck_runner.run_tests_main tests
