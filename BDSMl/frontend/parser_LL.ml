[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Lexer

type error =
  [ `lexer_error of string
  | `parse_error of string
  ]

type associativity =
  | Left
  | Right

type config = { associativity : binop -> associativity }

let default_config =
  { associativity =
      (function
        | And | Or -> Right
        | Plus | Minus | Mult | Div | Equal | NotEqual | More | Less | EMore | ELess ->
          Left)
  }
;;

let with_associativity config target associativity =
  { associativity =
      (fun operator ->
        if operator = target then associativity else config.associativity operator)
  }
;;

let token_to_string = function
  | TUnit -> "()"
  | TInt x -> string_of_int x
  | TIdent v -> v
  | TTrue -> "true"
  | TFalse -> "false"
  | TPlus -> "+"
  | TMinus -> "-"
  | TStar -> "*"
  | TSlash -> "/"
  | TEqual -> "="
  | TNotEqual -> "<>"
  | TMore -> ">"
  | TLess -> "<"
  | TEMore -> ">="
  | TELess -> "<="
  | TAnd -> "&&"
  | TOr -> "||"
  | TNot -> "not"
  | TLParen -> "("
  | TRParen -> ")"
  | TUnderscore -> "_"
  | TSemisemi -> ";;"
  | TEOF -> "<eof>"
  | TIF -> "if"
  | TThen -> "then"
  | TElse -> "else"
  | TLet -> "let"
  | TRec -> "rec"
  | TIn -> "in"
  | TFun -> "fun"
  | TArrow -> "->"
;;

let ( let* ) m f =
  match m with
  | Ok m -> f m
  | Error e -> Error e
;;

(* Чуть поразмыслить *)
let expect expected tokens =
  match tokens with
  | t :: ts when t = expected -> Ok ((), ts)
  | t :: _ ->
    Error
      (`parse_error
          (Printf.sprintf
             "expected '%s', got '%s'"
             (token_to_string expected)
             (token_to_string t)))
  | [] -> Error (`parse_error "unexpected end of input")
;;

let parse_pattern = function
  | TIdent s :: ts -> Ok (PVar s, ts)
  | TUnit :: ts -> Ok (PUnit, ts)
  | TUnderscore :: ts -> Ok (PUnderscore, ts)
  | t :: _ -> Error (`parse_error ("Expected pattern, got '" ^ token_to_string t ^ "'"))
  | [] -> Error (`parse_error "Unexpected end of input")
;;

let rec parse_pattern_list tokens =
  match tokens with
  | (TIdent _ | TUnderscore | TUnit) :: _ ->
    let* p, ts = parse_pattern tokens in
    let* ps, ts = parse_pattern_list ts in
    Ok (p :: ps, ts)
  | ts -> Ok ([], ts)
;;

let wrap_abs param body = List.fold_right (fun p acc -> Abs (p, acc)) param body

(* wtf *)
let rec parse_expr config tokens =
  match tokens with
  | TLet :: _ ->
    let* (rec_flag, pat, value), ts = parse_let_header config tokens in
    let* (), ts = expect TIn ts in
    let* body, ts = parse_expr config ts in
    Ok (Let (rec_flag, pat, value, body), ts)
  | TIF :: ts ->
    let* cond, ts = parse_expr config ts in
    let* (), ts = expect TThen ts in
    let* e_then, ts = parse_expr config ts in
    let* (), ts = expect TElse ts in
    let* e_else, ts = parse_expr config ts in
    Ok (If (cond, e_then, e_else), ts)
  | TFun :: ts ->
    let* params, ts = parse_pattern_list ts in
    if params = []
    then Error (`parse_error "'fun' expects at least one parameter")
    else
      let* (), ts = expect TArrow ts in
      let* body, ts = parse_expr config ts in
      Ok (wrap_abs params body, ts)
  | ts -> parse_binary config 0 ts

(* wtf *)
and parse_let_header config tokens =
  match tokens with
  | TLet :: ts ->
    let rec_flag, ts =
      match ts with
      | TRec :: rest -> Rec, rest
      | _ -> NonRec, ts
    in
    let* pat, ts = parse_pattern ts in
    let* params, ts = parse_pattern_list ts in
    let* (), ts = expect TEqual ts in
    let* value, ts = parse_expr config ts in
    Ok ((rec_flag, pat, wrap_abs params value), ts)
  | t :: _ -> Error (`parse_error ("expected 'let', got '" ^ token_to_string t ^ "'"))
  | [] -> Error (`parse_error "unexpected end of input")

and op_of_token = function
  | TPlus -> Some Plus
  | TMinus -> Some Minus
  | TStar -> Some Mult
  | TSlash -> Some Div
  | TEqual -> Some Equal
  | TNotEqual -> Some NotEqual
  | TMore -> Some More
  | TLess -> Some Less
  | TEMore -> Some EMore
  | TELess -> Some ELess
  | TAnd -> Some And
  | TOr -> Some Or
  | _ -> None

and precedence = function
  | Or -> 10
  | And -> 20
  | Equal | NotEqual | More | Less | EMore | ELess -> 30
  | Plus | Minus -> 40
  | Mult | Div -> 50

and parse_binary config min_precedence tokens =
  let* left, tokens = parse_unary config tokens in
  let rec loop left tokens =
    match tokens with
    | token :: rest ->
      (match op_of_token token with
       | Some operator when precedence operator >= min_precedence ->
         let operator_precedence = precedence operator in
         let next_precedence =
           match config.associativity operator with
           | Left -> operator_precedence + 1
           | Right -> operator_precedence
         in
         let* right, rest = parse_binary config next_precedence rest in
         loop (BinOp (operator, left, right)) rest
       | _ -> Ok (left, tokens))
    | [] -> Ok (left, [])
  in
  loop left tokens

and parse_unary config = function
  | TMinus :: ts ->
    let* e, ts = parse_unary config ts in
    Ok (UnOp (Neg, e), ts)
  | TNot :: ts ->
    let* e, ts = parse_unary config ts in
    Ok (UnOp (Not, e), ts)
  | ts -> parse_app config ts

and is_atom_start = function
  | TInt _ | TIdent _ | TTrue | TFalse | TUnit | TLParen -> true
  | _ -> false

and parse_app config tokens =
  let* f, ts = parse_atom config tokens in
  let rec loop acc ts =
    match ts with
    | t :: _ when is_atom_start t ->
      let* arg, ts' = parse_atom config ts in
      loop (App (acc, arg)) ts'
    | _ -> Ok (acc, ts)
  in
  loop f ts

and parse_atom config = function
  | TInt n :: ts -> Ok (Int n, ts)
  | TTrue :: ts -> Ok (Bool true, ts)
  | TFalse :: ts -> Ok (Bool false, ts)
  | TUnit :: ts -> Ok (Unit, ts)
  | TIdent s :: ts -> Ok (Var s, ts)
  | TLParen :: ts ->
    let* e, ts = parse_expr config ts in
    let* (), ts = expect TRParen ts in
    Ok (e, ts)
  | t :: _ -> Error (`parse_error ("unexpected token '" ^ token_to_string t ^ "'"))
  | [] -> Error (`parse_error "unexpected end of input")
;;

let parse_structure_item config tokens =
  match tokens with
  | TLet :: _ ->
    let* (rec_flag, pat, value), ts = parse_let_header config tokens in
    (match ts with
     | TIn :: ts ->
       let* body, ts = parse_expr config ts in
       Ok (StrEval (Let (rec_flag, pat, value, body)), ts)
     | _ -> Ok (StrLet (rec_flag, pat, value), ts))
  | _ ->
    let* e, ts = parse_expr config tokens in
    Ok (StrEval e, ts)
;;

let rec parse_items config tokens =
  let* item, ts = parse_structure_item config tokens in
  match ts with
  | TEOF :: _ -> Ok [ item ]
  | TSemisemi :: rest ->
    (match rest with
     | TEOF :: _ -> Ok [ item ]
     | _ ->
       let* items = parse_items config rest in
       Ok (item :: items))
  | t :: _ ->
    Error
      (`parse_error
          (Printf.sprintf "expected ';;' before next item, got '%s'" (token_to_string t)))
  | [] -> Error (`parse_error "unexpected end of input")
;;

let parse ?(config = default_config) (tokens : token list) : (program, [> error ]) result =
  parse_items config tokens
;;

let parse_string_LL
      ?(config = default_config)
      (src : string)
  : (program, [> error | Lexer.error ]) result
  =
  match Lexer.tokenize src with
  | Error e -> Error e
  | Ok tokens -> (parse ~config tokens :> (program, [> error | Lexer.error ]) result)
;;
