[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VTuple of value * value * value list
  | VClosure of (value list -> (value, string) result)

val equal : value -> value -> bool
val to_string : value -> string
val is_primitive : string -> bool
val apply_primitive : string -> value list -> (value, string) result
