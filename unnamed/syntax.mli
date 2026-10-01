[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val is_reserved : string -> bool

type associativity =
  | Left
  | Right

type infix_override =
  { operator : string
  ; precedence : int
  ; associativity : associativity
  }

type prefix_override =
  { operator : string
  ; arity : int
  }

type infix_entry =
  { precedence : int
  ; associativity : associativity
  ; operators : string list
  }

type prefix_entry =
  { arity : int
  ; operators : string list
  }

type table

val default_table : table
val apply_infix_overrides : infix_override list -> table -> table
val apply_prefix_overrides : prefix_override list -> table -> table
val infix_entries : table -> infix_entry list
val prefix_entries : table -> prefix_entry list
val operator_names : table -> string list
val lookup_infix : table -> string -> (int * associativity) option
val lookup_prefix : table -> string -> (int * int) option
val is_operator : table -> string -> bool
