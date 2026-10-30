[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Stable GNU assembler text with a final newline. *)
val print : Asm.program -> string

val register_name : Asm.register -> string
