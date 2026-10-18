[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Diagnostic ANF output. Local IDs, externals and builtins are distinguished;
    this format is not MiniML source and is not parsed by the frontend. *)
val pp_id : Format.formatter -> Anf.id -> unit

val pp_imm : Format.formatter -> Anf.imm -> unit
val pp_term : Format.formatter -> Anf.term -> unit
val pp_program : Format.formatter -> Anf.program -> unit
val show_term : Anf.term -> string
val show_program : Anf.program -> string
