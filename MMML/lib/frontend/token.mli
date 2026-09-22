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

type located =
  { token : t
  ; span : Location.span
  }
[@@deriving show { with_path = false }]

val keyword_or_identifier : string -> t
val operator_of_lexeme : string -> t option
val to_lexeme : t -> string
