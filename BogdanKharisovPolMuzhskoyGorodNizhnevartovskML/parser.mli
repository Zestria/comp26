[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Nested comments support quoted strings with ASCII delimiters. *)
val parse_expression : string -> (Ast.expression, string) result

val parse_program : string -> (Ast.program, string) result

(** Top-level [infixl], [infixr] and [prefix] declarations affect subsequent input. *)
val parse_program_with_operators : string -> (Ast.program, string) result
