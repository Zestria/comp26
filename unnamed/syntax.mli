[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val is_reserved : string -> bool

type associativity =
  | Left
  | Right

val infix_operators : (associativity * string list) list
val prefix_operators : (int * string list) list
val lookup_infix : string -> (int * associativity) option
val lookup_prefix : string -> (int * int) option
val infix_names : string list
val prefix_names : string list
val operator_names : string list
val is_operator : string -> bool
