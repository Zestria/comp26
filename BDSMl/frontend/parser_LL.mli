[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  [ `lexer_error of string
  | `parse_error of string
  ]

val parse_string_LL : string -> (program, [> error ]) result
