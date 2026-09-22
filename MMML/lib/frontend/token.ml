[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Tokens of the minimal MiniML syntax. *)

type t =
  | INT of int
  | IDENT of string
  | LET
  | REC
  | IN
  | FUN
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | UNDERSCORE
  | LPAREN
  | RPAREN
  | ARROW
  | EQUAL
  | NOT_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | AND_AND
  | OR_OR
  | EOF
[@@deriving show { with_path = false }]

(** A token paired with its half-open range in the source. *)
type located =
  { token : t
  ; span : Location.span
  }
[@@deriving show { with_path = false }]

(** Recognize a supported keyword or produce an identifier. *)
let keyword_or_identifier = function
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  | "fun" -> FUN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "_" -> UNDERSCORE
  | identifier -> IDENT identifier
;;

(** Recognize one of the supported binary operators. *)
let operator_of_lexeme = function
  | "=" -> Some EQUAL
  | "<>" -> Some NOT_EQUAL
  | "<" -> Some LESS
  | "<=" -> Some LESS_EQUAL
  | ">" -> Some GREATER
  | ">=" -> Some GREATER_EQUAL
  | "+" -> Some PLUS
  | "-" -> Some MINUS
  | "*" -> Some STAR
  | "/" -> Some SLASH
  | "&&" -> Some AND_AND
  | "||" -> Some OR_OR
  | _ -> None
;;

(** Return the canonical source spelling of a token. *)
let to_lexeme = function
  | INT value -> string_of_int value
  | IDENT identifier -> identifier
  | LET -> "let"
  | REC -> "rec"
  | IN -> "in"
  | FUN -> "fun"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | TRUE -> "true"
  | FALSE -> "false"
  | UNDERSCORE -> "_"
  | LPAREN -> "("
  | RPAREN -> ")"
  | ARROW -> "->"
  | EQUAL -> "="
  | NOT_EQUAL -> "<>"
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | AND_AND -> "&&"
  | OR_OR -> "||"
  | EOF -> ""
;;
