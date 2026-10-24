[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Removes unused pure bindings and [let x = e in x].
    [is_prim name] tells whether [name] still means the built-in operator. *)

val program : is_prim:(string -> bool) -> Anf.program -> Anf.program
