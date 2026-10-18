[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Parse a complete MiniML input using recursive descent. *)

val parse_expression : string -> Ast.expr Error_monad.t
val parse_program : string -> Ast.program Error_monad.t
