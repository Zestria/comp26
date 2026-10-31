[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Print code declarations and the entry expression with stable local names. *)
val program : Format.formatter -> Lifted.program -> unit
