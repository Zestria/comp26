[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Lexer_LL

type error =
  [ `lexer_error of string
  | `parse_error of string
  ]

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

(*Чуть не понятно, что делать с 1м аргументом, хотя должен сжираться 1ой веткой*)

(* wtf *)
let wrap_abs param body = List.fold_right (fun p acc -> Abs (p, acc)) param body

let rec parse_expr tokens =
  match tokens with
  | TLet :: _ ->
    let* (rec_flag, pat, value), ts = parse_let_header tokens in
    let* (), ts = expect TIn ts in
    let* body, ts = parse_expr ts in
    Ok (Let (rec_flag, pat, value, body), ts)
  | TIF :: ts ->
    let* cond, ts = parse_expr ts in
    let* (), ts = expect TThen ts in
    let* e_then, ts = parse_expr ts in
    let* (), ts = expect TElse ts in
    let* e_else, ts = parse_expr ts in
    Ok (If (cond, e_then, e_else), ts)
  | TFun :: ts ->
    let* params, ts = parse_pattern_list ts in
    if params = []
    then Error (`parse_error "'fun' expects at least one parameter")
    else
      let* (), ts = expect TArrow ts in
      let* body, ts = parse_expr ts in
      Ok (wrap_abs params body, ts)
  | ts -> parse_or ts

(* wtf *)
and parse_let_header tokens =
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
    let* value, ts = parse_expr ts in
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

(* all wtf *)
and parse_binop parse_next allowed tokens =
  let* left, ts = parse_next tokens in
  let rec loop acc ts =
    match ts with
    | tok :: rest ->
      (match op_of_token tok with
       | Some op when List.mem op allowed ->
         let* right, ts' = parse_next rest in
         loop (BinOp (op, acc, right)) ts'
       | _ -> Ok (acc, ts))
    | [] -> Ok (acc, ts)
  in
  loop left ts

and parse_or ts = parse_binop parse_and [ Or ] ts
and parse_and ts = parse_binop parse_cmp [ And ] ts
and parse_cmp ts = parse_binop parse_add [ Equal; NotEqual; More; Less; EMore; ELess ] ts
and parse_add ts = parse_binop parse_mul [ Plus; Minus ] ts
and parse_mul ts = parse_binop parse_unary [ Mult; Div ] ts

and parse_unary = function
  | TMinus :: ts ->
    let* e, ts = parse_unary ts in
    Ok (UnOp (Neg, e), ts)
  | TNot :: ts ->
    let* e, ts = parse_unary ts in
    Ok (UnOp (Not, e), ts)
  | ts -> parse_app ts

and is_atom_start = function
  | TInt _ | TIdent _ | TTrue | TFalse | TUnit | TLParen -> true
  | _ -> false

and parse_app tokens =
  let* f, ts = parse_atom tokens in
  let rec loop acc ts =
    match ts with
    | t :: _ when is_atom_start t ->
      let* arg, ts' = parse_atom ts in
      loop (App (acc, arg)) ts'
    | _ -> Ok (acc, ts)
  in
  loop f ts

and parse_atom = function
  | TInt n :: ts -> Ok (Int n, ts)
  | TTrue :: ts -> Ok (Bool true, ts)
  | TFalse :: ts -> Ok (Bool false, ts)
  | TUnit :: ts -> Ok (Unit, ts)
  | TIdent s :: ts -> Ok (Var s, ts)
  | TLParen :: ts ->
    let* e, ts = parse_expr ts in
    let* (), ts = expect TRParen ts in
    Ok (e, ts)
  | t :: _ -> Error (`parse_error ("unexpected token '" ^ token_to_string t ^ "'"))
  | [] -> Error (`parse_error "unexpected end of input")
;;

let parse_structure_item tokens =
  match tokens with
  | TLet :: _ ->
    let* (rec_flag, pat, value), ts = parse_let_header tokens in
    (match ts with
     | TIn :: ts ->
       let* body, ts = parse_expr ts in
       Ok (StrEval (Let (rec_flag, pat, value, body)), ts)
     | _ -> Ok (StrLet (rec_flag, pat, value), ts))
  | _ ->
    let* e, ts = parse_expr tokens in
    Ok (StrEval e, ts)
;;

let rec parse_items tokens =
  let* item, ts = parse_structure_item tokens in
  match ts with
  | TEOF :: _ -> Ok [ item ]
  | TSemisemi :: rest ->
    (match rest with
     | TEOF :: _ -> Ok [ item ]
     | _ ->
       let* items = parse_items rest in
       Ok (item :: items))
  | t :: _ ->
    Error
      (`parse_error
          (Printf.sprintf "expected ';;' before next item, got '%s'" (token_to_string t)))
  | [] -> Error (`parse_error "unexpected end of input")
;;

let parse (tokens : token list) : (program, [> error ]) result = parse_items tokens

let parse_string_LL (src : string) : (program, [> error | Lexer_LL.error ]) result =
  match Lexer_LL.tokenize src with
  | Error e -> Error e
  | Ok tokens -> (parse tokens :> (program, [> error | Lexer_LL.error ]) result)
;;
