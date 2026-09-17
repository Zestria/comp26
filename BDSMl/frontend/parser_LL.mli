[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  [ `lexer_error of string
  | `parse_error of string
  ]

type associativity =
  | Left
  | Right

type config

val default_config : config

val with_associativity : config -> binop -> associativity -> config

val parse_string_LL : ?config:config -> string -> (program, [> error ]) result
