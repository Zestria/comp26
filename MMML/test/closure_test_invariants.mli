[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Check lexical scope and closed lambda groups independently of conversion. *)

val check : externals:string list -> Ast.program -> (unit, string) result
