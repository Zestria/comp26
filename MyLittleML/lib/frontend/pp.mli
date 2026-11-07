[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val pp_expr : Format.formatter -> Ast.expr -> unit
val pp_prog : Format.formatter -> Ast.prog -> unit
val to_string : Ast.prog -> string
