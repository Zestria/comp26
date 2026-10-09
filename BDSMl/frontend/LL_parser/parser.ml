[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Tokens

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

type 'e token_stream = (token, 'e) result Seq.t

type 'e stream_head =
  | Nil
  | Cons of token * 'e token_stream
  | LexError of 'e

let step (s : 'e token_stream) : 'e stream_head =
  match s () with
  | Seq.Nil -> Nil
  | Seq.Cons (Ok t, rest) -> Cons (t, rest)
  | Seq.Cons (Error e, _) -> LexError e
;;

let expect expected tokens =
  match step tokens with
  | Cons (t, ts) when t = expected -> Ok ((), ts)
  | Cons (t, _) ->
    Error
      (`parse_error
          (Printf.sprintf
             "expected '%s', got '%s'"
             (token_to_string expected)
             (token_to_string t)))
  | Nil -> Error (`parse_error "unexpected end of input")
  | LexError e -> Error (e :> [> error ])
;;

let parse_pattern tokens =
  match step tokens with
  | Cons (TIdent s, ts) -> Ok (PVar s, ts)
  | Cons (TUnit, ts) -> Ok (PUnit, ts)
  | Cons (TLParen, ts) ->
    let* (), ts = expect TRParen ts in
    Ok (PUnit, ts)
  | Cons (TUnderscore, ts) -> Ok (PUnderscore, ts)
  | Cons (t, _) ->
    Error (`parse_error ("Expected pattern, got '" ^ token_to_string t ^ "'"))
  | Nil -> Error (`parse_error "Unexpected end of input")
  | LexError e -> Error (e :> [> error ])
;;

let rec parse_pattern_list tokens =
  match step tokens with
  | Cons ((TIdent _ | TUnderscore | TUnit | TLParen), _) ->
    let* p, ts = parse_pattern tokens in
    let* ps, ts = parse_pattern_list ts in
    Ok (p :: ps, ts)
  | Cons _ | Nil -> Ok ([], tokens)
  | LexError e -> Error (e :> [> error ])
;;

let wrap_abs param body = List.fold_right (fun p acc -> Abs (p, acc)) param body

let rec parse_expr config tokens =
  match step tokens with
  | Cons (TLet, _) ->
    let* (rec_flag, pat, value), ts = parse_let_header config tokens in
    let* (), ts = expect TIn ts in
    let* body, ts = parse_expr config ts in
    Ok (Let (rec_flag, pat, value, body), ts)
  | Cons (TIF, ts) ->
    let* cond, ts = parse_expr config ts in
    let* (), ts = expect TThen ts in
    let* e_then, ts = parse_expr config ts in
    let* (), ts = expect TElse ts in
    let* e_else, ts = parse_expr config ts in
    Ok (If (cond, e_then, e_else), ts)
  | Cons (TFun, ts) ->
    let* params, ts = parse_pattern_list ts in
    if params = []
    then Error (`parse_error "'fun' expects at least one parameter")
    else
      let* (), ts = expect TArrow ts in
      let* body, ts = parse_expr config ts in
      Ok (wrap_abs params body, ts)
  | Cons _ | Nil -> parse_binary config 0 tokens
  | LexError e -> Error (e :> [> error ])

and parse_let_header config tokens =
  match step tokens with
  | Cons (TLet, ts) ->
    let rec_flag, ts =
      match step ts with
      | Cons (TRec, rest) -> Rec, rest
      | _ -> NonRec, ts
    in
    let* pat, ts = parse_pattern ts in
    let* params, ts = parse_pattern_list ts in
    let* (), ts = expect TEqual ts in
    let* value, ts = parse_expr config ts in
    Ok ((rec_flag, pat, wrap_abs params value), ts)
  | Cons (t, _) ->
    Error (`parse_error ("expected 'let', got '" ^ token_to_string t ^ "'"))
  | Nil -> Error (`parse_error "unexpected end of input")
  | LexError e -> Error (e :> [> error ])

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
    match step tokens with
    | Cons (token, rest) ->
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
    | Nil -> Ok (left, tokens)
    | LexError e -> Error (e :> [> error ])
  in
  loop left tokens

and parse_unary config tokens =
  match step tokens with
  | Cons (TMinus, ts) ->
    let* e, ts = parse_unary config ts in
    Ok (UnOp (Neg, e), ts)
  | Cons (TNot, ts) ->
    let* e, ts = parse_unary config ts in
    Ok (UnOp (Not, e), ts)
  | Cons _ | Nil | LexError _ -> parse_app config tokens

and is_atom_start = function
  | TInt _ | TIdent _ | TTrue | TFalse | TUnit | TLParen -> true
  | _ -> false

and parse_app config tokens =
  let* f, ts = parse_atom config tokens in
  let rec loop acc ts =
    match step ts with
    | Cons (t, _) when is_atom_start t ->
      let* arg, ts' = parse_atom config ts in
      loop (App (acc, arg)) ts'
    | Cons _ | Nil -> Ok (acc, ts)
    | LexError e -> Error (e :> [> error ])
  in
  loop f ts

and parse_atom config tokens =
  match step tokens with
  | Cons (TInt n, ts) -> Ok (Int n, ts)
  | Cons (TTrue, ts) -> Ok (Bool true, ts)
  | Cons (TFalse, ts) -> Ok (Bool false, ts)
  | Cons (TUnit, ts) -> Ok (Unit, ts)
  | Cons (TIdent s, ts) -> Ok (Var s, ts)
  | Cons (TLParen, ts) ->
    (match step ts with
     | Cons (TRParen, rest) -> Ok (Unit, rest)
     | Cons _ | Nil ->
       let* e, ts = parse_expr config ts in
       let* (), ts = expect TRParen ts in
       Ok (e, ts)
     | LexError e -> Error (e :> [> error ]))
  | Cons (t, _) -> Error (`parse_error ("unexpected token '" ^ token_to_string t ^ "'"))
  | Nil -> Error (`parse_error "unexpected end of input")
  | LexError e -> Error (e :> [> error ])
;;

let parse_structure_item config tokens =
  match step tokens with
  | Cons (TLet, _) ->
    let* (rec_flag, pat, value), ts = parse_let_header config tokens in
    (match step ts with
     | Cons (TIn, ts') ->
       let* body, ts'' = parse_expr config ts' in
       Ok (StrEval (Let (rec_flag, pat, value, body)), ts'')
     | Cons _ | Nil -> Ok (StrLet (rec_flag, pat, value), ts)
     | LexError e -> Error (e :> [> error ]))
  | _ ->
    let* e, ts = parse_expr config tokens in
    Ok (StrEval e, ts)
;;

let rec parse_items config tokens =
  match step tokens with
  | Cons (TEOF, _) -> Ok []
  | Cons (TSemisemi, rest) -> parse_items config rest
  | Cons _ ->
    let* item, ts = parse_structure_item config tokens in
    (match step ts with
     | Cons (TEOF, _) -> Ok [ item ]
     | Cons (TSemisemi, rest) ->
       let* items = parse_items config rest in
       Ok (item :: items)
     | Cons (TLet, _) ->
       let* items = parse_items config ts in
       Ok (item :: items)
     | Cons (t, _) ->
       Error
         (`parse_error
             (Printf.sprintf
                "expected ';;' before next item, got '%s'"
                (token_to_string t)))
     | Nil -> Error (`parse_error "unexpected end of input")
     | LexError e -> Error (e :> [> error ]))
  | Nil -> Error (`parse_error "unexpected end of input")
  | LexError e -> Error (e :> [> error ])
;;

let parse ?(config = default_config) (tokens : 'e token_stream) =
  parse_items config tokens
;;

let parse_string_LL ?(config = default_config) (src : string) =
  let tokens = Lexer.tokenize_lazy src |> Seq.memoize in
  parse ~config tokens
;;
