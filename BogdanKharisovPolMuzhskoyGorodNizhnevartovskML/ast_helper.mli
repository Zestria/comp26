[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val binary : Name.t -> Ast.expression -> Ast.expression -> Ast.expression
val negate : Ast.expression -> Ast.expression
