[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val pp_expression : Format.formatter -> Ast.expression -> unit
val pp_pattern : Format.formatter -> Ast.pattern -> unit
val pp_structure_item : Format.formatter -> Ast.structure_item -> unit
val pp_structure : Format.formatter -> Ast.structure -> unit
val string_of_expression : Ast.expression -> string
val string_of_structure : Ast.structure -> string
