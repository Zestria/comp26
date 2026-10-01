[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val parse_structure : ?table:Syntax.table -> string -> (Ast.structure, string) result
val parse_expr : ?table:Syntax.table -> string -> (Ast.expression, string) result
