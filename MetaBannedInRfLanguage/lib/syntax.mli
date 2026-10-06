[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val is_reserved : string -> bool

type assoc =
  | Left
  | Right

type fixity =
  | Prefix of int
  | Infix of int * assoc

type entry =
  { operators : string list
  ; fixity : fixity
  }

type table

val default_table : table
val apply_infix_overrides : (string * int * assoc) list -> table -> table
val apply_prefix_overrides : (string * int) list -> table -> table
val infix_entries : table -> entry list
val prefix_entries : table -> entry list
val operator_names : table -> string list
val lookup_infix : table -> string -> (int * assoc) option
val lookup_prefix : table -> string -> (int * int) option
val is_operator : table -> string -> bool
