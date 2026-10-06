[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Unambiguous rendering of the surface AST as MiniML source. *)

(** Print a pattern. *)
val pp_pattern : Format.formatter -> Ast.pattern -> unit

(** Print an expression. *)
val pp_expr : Format.formatter -> Ast.expr -> unit

(** Print a complete program. *)
val pp_program : Format.formatter -> Ast.program -> unit

(** Render an expression as a string. *)
val expr_to_string : Ast.expr -> string

(** Render a complete program as a string. *)
val program_to_string : Ast.program -> string
