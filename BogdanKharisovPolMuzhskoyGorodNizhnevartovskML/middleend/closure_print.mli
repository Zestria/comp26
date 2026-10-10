[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Print nested closures with stable local names. In
    [closure [values] (fun [slots] parameters -> body)], captured values belong
    to the construction scope; fresh slots belong to the function body. *)
val program : Format.formatter -> Closure.expression -> unit
