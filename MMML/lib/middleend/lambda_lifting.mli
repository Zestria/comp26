[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Lift functions that capture local variables into top-level definitions. *)

val lift_program : Ast.program -> Ast.program
