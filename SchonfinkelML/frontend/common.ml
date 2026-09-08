[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type token =
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | IDENT of string
  | INT of int
  (* | FLOAT of float *)
  (* | STRING of string *)
  (* | BOOL of bool *)
  | UNIT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | LT
  | GT
  | LPAREN
  | RPAREN
  | EOF

type literal = LitInt of int
(* | LitFloat of float *)
(* | LitString of string *)
(* | LitBool of bool *)

type expr =
  | Var of string
  | Const of literal
  | App of expr * expr
  | Let of string * expr * expr

let pp_literal ppf = function
  | LitInt i -> Format.fprintf ppf "%d" i
;;

(* | LitFloat f -> Format.fprintf ppf "%g" f *)
(* | LitString s -> Format.fprintf ppf "\"%s\"" s *)
(* | LitBool b -> Format.fprintf ppf (if b then "true" else "false") *)

let pp_token ppf = function
  | IF -> Format.fprintf ppf "if"
  | THEN -> Format.fprintf ppf "then"
  | ELSE -> Format.fprintf ppf "else"
  | LET -> Format.fprintf ppf "let"
  | IN -> Format.fprintf ppf "in"
  | IDENT s -> Format.fprintf ppf "%s" s
  | INT i -> Format.fprintf ppf "%d" i
  (* | FLOAT f -> Format.fprintf ppf "%g" f *)
  (* | STRING s -> Format.fprintf ppf "\"%s\"" s *)
  (* | BOOL b -> Format.fprintf ppf (if b then "true" else "false") *)
  | UNIT -> Format.fprintf ppf "()"
  | PLUS -> Format.fprintf ppf "+"
  | MINUS -> Format.fprintf ppf "-"
  | TIMES -> Format.fprintf ppf "*"
  | DIV -> Format.fprintf ppf "/"
  | EQ -> Format.fprintf ppf "="
  | LT -> Format.fprintf ppf "<"
  | GT -> Format.fprintf ppf ">"
  | LPAREN -> Format.fprintf ppf "("
  | RPAREN -> Format.fprintf ppf ")"
  | EOF -> Format.fprintf ppf "EOF"
;;

let rec pp_expr ppf = function
  | Var x -> Format.fprintf ppf "%s" x
  | Const l -> pp_literal ppf l
  | App (e1, e2) -> Format.fprintf ppf "@[(%a %a)@]" pp_expr e1 pp_expr e2
  | Let (x, e1, e2) ->
    Format.fprintf ppf "@[<2>let %s =@ %a@ in@ %a@]" x pp_expr e1 pp_expr e2
;;

let string_of_expr e = Format.asprintf "%a" pp_expr e
