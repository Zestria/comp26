[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val free_variables : Anf.aexpr -> string list
val of_program : Anf.program -> Closure_ir.program
