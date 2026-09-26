[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val parse_expression : string -> (Ast.expression, string) result
val parse_program : string -> (Ast.program, string) result
