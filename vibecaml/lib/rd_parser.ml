(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type token_kind =
  | Int of int
  | Ident of string
  | Operator of string
  | Let
  | Rec
  | And
  | In
  | Fun
  | If
  | Then
  | Else
  | True
  | False
  | Lparen
  | Rparen
  | Comma
  | Equal
  | Arrow
  | Eof

type token =
  { kind : token_kind
  ; offset : int
  }

(* Поле [index] — положение курсора: разбор последовательно потребляет
   токены, поэтому изменяемое состояние здесь проще и не дублирует массив. *)
type state =
  { tokens : token array
  ; mutable index : int
  }

type associativity =
  | Left
  | Right

exception Parse_error of string

let fail offset message =
  raise (Parse_error (Printf.sprintf "позиция %d: %s" offset message))
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let is_operator_char = function
  | '!'
  | '$'
  | '%'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '|'
  | '~' -> true
  | _ -> false
;;

let word_kind = function
  | "let" -> Let
  | "rec" -> Rec
  | "and" -> And
  | "in" -> In
  | "fun" -> Fun
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "true" -> True
  | "false" -> False
  | ("mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" | "or") as op -> Operator op
  | word -> Ident word
;;

(* Лексер специально остаётся маленьким: он отделяет комментарии и пробелы от
   грамматики, поэтому все сообщения синтаксического анализатора однородны. *)
let lex source =
  let length = String.length source in
  let rec skip_comment start depth index =
    if index >= length
    then fail start "незакрытый комментарий"
    else if index + 1 < length && String.sub source index 2 = "(*"
    then skip_comment start (depth + 1) (index + 2)
    else if index + 1 < length && String.sub source index 2 = "*)"
    then if depth = 1 then index + 2 else skip_comment start (depth - 1) (index + 2)
    else skip_comment start depth (index + 1)
  in
  let rec take_while predicate index =
    if index < length && predicate source.[index]
    then take_while predicate (index + 1)
    else index
  in
  let rec loop index tokens =
    if index >= length
    then List.rev ({ kind = Eof; offset = length } :: tokens)
    else if is_space source.[index]
    then loop (index + 1) tokens
    else if index + 1 < length && String.sub source index 2 = "(*"
    then loop (skip_comment index 1 (index + 2)) tokens
    else (
      let add kind finish = loop finish ({ kind; offset = index } :: tokens) in
      match source.[index] with
      | '(' -> add Lparen (index + 1)
      | ')' -> add Rparen (index + 1)
      | ',' -> add Comma (index + 1)
      | '=' -> add Equal (index + 1)
      | '-' when index + 1 < length && source.[index + 1] = '>' -> add Arrow (index + 2)
      | '0' .. '9' ->
        let finish =
          take_while
            (function
              | '0' .. '9' -> true
              | _ -> false)
            index
        in
        let digits = String.sub source index (finish - index) in
        (match int_of_string_opt digits with
         | Some value -> add (Int value) finish
         | None -> fail index "целое число вне диапазона")
      | character when is_letter character ->
        let finish = take_while is_identifier_char index in
        add (word_kind (String.sub source index (finish - index))) finish
      | character when is_operator_char character ->
        let finish = take_while is_operator_char index in
        add (Operator (String.sub source index (finish - index))) finish
      | character -> fail index (Printf.sprintf "недопустимый символ %C" character))
  in
  loop 0 []
;;

let current state = state.tokens.(state.index)

let take state =
  let token = current state in
  state.index <- state.index + 1;
  token
;;

let expect state kind =
  let token = take state in
  if token.kind <> kind then fail token.offset "получен неожиданный токен"
;;

let binding pattern expression =
  { binding_pattern = pattern; binding_expression = expression }
;;

(* Приоритеты — данные, а не ветви грамматики. В #5 эту таблицу можно будет
   заменить после чтения объявлений операторов. *)
let operator_table =
  [ [ "||" ], 10, Right
  ; [ "&&" ], 20, Right
  ; [ "="; "<>"; "<"; "<="; ">"; ">=" ], 30, Left
  ; [ "+"; "-"; "+."; "-." ], 40, Left
  ; [ "*"; "/"; "*."; "/."; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr" ], 50, Left
  ; [ "**" ], 60, Right
  ]
;;

let operator_info name =
  List.find_map
    (fun (names, precedence, associativity) ->
       if List.mem name names then Some (precedence, associativity) else None)
    operator_table
;;

let starts_pattern = function
  | Int _ | Ident _ | True | False | Lparen -> true
  | _ -> false
;;

let starts_atom = function
  | Int _ | Ident _ | True | False | Lparen -> true
  | _ -> false
;;

let rec parse_pattern state =
  match take state with
  | { kind = Int value; _ } -> Pat_constant (Const_int value)
  | { kind = True; _ } -> Pat_constant (Const_bool true)
  | { kind = False; _ } -> Pat_constant (Const_bool false)
  | { kind = Ident "_"; _ } -> Pat_wildcard
  | { kind = Ident name; _ } -> Pat_var name
  | { kind = Lparen; offset } -> parse_parenthesized_pattern state offset
  | token -> fail token.offset "ожидался паттерн"

and parse_parenthesized_pattern state offset =
  match (current state).kind with
  | Rparen ->
    let _ = take state in
    Pat_constant Const_unit
  | Operator name ->
    let _ = take state in
    expect state Rparen;
    Pat_var name
  | _ ->
    let first = parse_pattern state in
    (match (current state).kind with
     | Rparen ->
       let _ = take state in
       first
     | Comma ->
       let _ = take state in
       let second = parse_pattern state in
       let rec rest values =
         match (current state).kind with
         | Comma ->
           let _ = take state in
           rest (parse_pattern state :: values)
         | Rparen ->
           let _ = take state in
           List.rev values
         | _ -> fail (current state).offset "ожидались запятая или закрывающая скобка"
       in
       Pat_tuple (first, second, rest [])
     | _ -> fail offset "ожидались запятая или закрывающая скобка")
;;

let rec parse_expression state =
  match (current state).kind with
  | Let -> parse_let state
  | Fun -> parse_fun state
  | If -> parse_if state
  | _ -> parse_infix state 0

and parse_let state =
  let _ = take state in
  let flag =
    match (current state).kind with
    | Rec ->
      let _ = take state in
      Recursive
    | _ -> Non_recursive
  in
  let first = parse_value_binding state in
  let rec bindings values =
    match (current state).kind with
    | And ->
      let _ = take state in
      bindings (parse_value_binding state :: values)
    | In ->
      let _ = take state in
      List.rev values
    | _ -> fail (current state).offset "ожидались and или in"
  in
  let rest = bindings [] in
  let body = parse_expression state in
  Expr_let (flag, first, rest, body)

and parse_value_binding state =
  let first = parse_pattern state in
  let rec parameters values =
    match (current state).kind with
    | Equal -> List.rev values
    | kind when starts_pattern kind -> parameters (parse_pattern state :: values)
    | _ -> fail (current state).offset "ожидались параметр или знак ="
  in
  let parameters = parameters [] in
  expect state Equal;
  let expression = parse_expression state in
  let expression =
    match parameters with
    | [] -> expression
    | first :: rest -> Expr_fun (first, rest, expression)
  in
  binding first expression

and parse_fun state =
  let _ = take state in
  let first = parse_pattern state in
  let rec parameters values =
    match (current state).kind with
    | Arrow -> List.rev values
    | kind when starts_pattern kind -> parameters (parse_pattern state :: values)
    | _ -> fail (current state).offset "ожидались параметр или ->"
  in
  let rest = parameters [] in
  expect state Arrow;
  Expr_fun (first, rest, parse_expression state)

and parse_if state =
  let _ = take state in
  let condition = parse_expression state in
  expect state Then;
  let then_branch = parse_expression state in
  expect state Else;
  Expr_if (condition, then_branch, parse_expression state)

and parse_infix state minimum =
  let left = parse_application state in
  let rec continue left =
    let token = current state in
    let name =
      match token.kind with
      | Operator name -> Some name
      | Equal -> Some "="
      | _ -> None
    in
    match Option.bind name operator_info with
    | Some (precedence, association) when precedence >= minimum ->
      let _ = take state in
      let next =
        match association with
        | Left -> precedence + 1
        | Right -> precedence
      in
      let right = parse_infix state next in
      continue (Expr_app (Expr_app (Expr_ident (Option.get name), left), right))
    | _ -> left
  in
  continue left

and parse_application state =
  let first = parse_atom state in
  let rec arguments function_ =
    if starts_atom (current state).kind
    then arguments (Expr_app (function_, parse_atom state))
    else function_
  in
  arguments first

and parse_atom state =
  match take state with
  | { kind = Int value; _ } -> Expr_constant (Const_int value)
  | { kind = True; _ } -> Expr_constant (Const_bool true)
  | { kind = False; _ } -> Expr_constant (Const_bool false)
  | { kind = Ident name; _ } -> Expr_ident name
  | { kind = Operator "-"; offset } ->
    (match take state with
     | { kind = Int value; _ } -> Expr_constant (Const_int (-value))
     | token -> fail token.offset "после унарного - ожидалось целое число")
  | { kind = Lparen; offset } -> parse_parenthesized_expression state offset
  | token -> fail token.offset "ожидалось выражение"

and parse_parenthesized_expression state offset =
  match (current state).kind with
  | Rparen ->
    let _ = take state in
    Expr_constant Const_unit
  | Equal ->
    let _ = take state in
    expect state Rparen;
    Expr_ident "="
  | Operator name ->
    let _ = take state in
    expect state Rparen;
    Expr_ident name
  | _ ->
    let first = parse_expression state in
    (match (current state).kind with
     | Rparen ->
       let _ = take state in
       first
     | Comma ->
       let _ = take state in
       let second = parse_expression state in
       let rec rest values =
         match (current state).kind with
         | Comma ->
           let _ = take state in
           rest (parse_expression state :: values)
         | Rparen ->
           let _ = take state in
           List.rev values
         | _ -> fail (current state).offset "ожидались запятая или закрывающая скобка"
       in
       Expr_tuple (first, second, rest [])
     | _ -> fail offset "ожидались запятая или закрывающая скобка")
;;

let run parse source =
  match lex source with
  | tokens ->
    let state = { tokens = Array.of_list tokens; index = 0 } in
    (try
       let result = parse state in
       match (current state).kind with
       | Eof -> Ok result
       | _ ->
         Error
           (Printf.sprintf
              "позиция %d: лишний текст после программы"
              (current state).offset)
     with
     | Parse_error message -> Error message
     | Failure message -> Error ("не разбирается: " ^ message))
  | exception Parse_error message -> Error message
;;

let parse_expr source = run parse_expression source

let parse_structure source =
  run
    (fun state ->
       let rec items values =
         match (current state).kind with
         | Eof -> List.rev values
         | Let ->
           let _ = take state in
           let flag =
             match (current state).kind with
             | Rec ->
               let _ = take state in
               Recursive
             | _ -> Non_recursive
           in
           let first = parse_value_binding state in
           let rec bindings rest =
             match (current state).kind with
             | And ->
               let _ = take state in
               bindings (parse_value_binding state :: rest)
             | _ -> List.rev rest
           in
           items (Structure_value (flag, first, bindings []) :: values)
         | _ -> fail (current state).offset "в структуре ожидалось let"
       in
       items [])
    source
;;
