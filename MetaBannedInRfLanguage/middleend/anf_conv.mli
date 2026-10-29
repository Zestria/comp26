[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Conversion of the source AST to A-normal form. *)

val convert : Ast.structure -> (Anf.program, string) result
