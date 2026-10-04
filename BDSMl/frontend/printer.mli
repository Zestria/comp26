[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val print_binop : Ast.binop -> string
val print_unop : Ast.unop -> string
val print_pattern_ast : Ast.pattern -> string
val print_pattern : Ast.pattern -> string
val print_ast : Ast.program -> string
val print_program : Ast.program -> string
