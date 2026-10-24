[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Prints a program in A-normal form as OCaml source. *)

val pp_program : Format.formatter -> Anf.program -> unit
val string_of_program : Anf.program -> string
