[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VTuple of value * value * value list
  (** Takes the arguments and the remaining fuel, returns the result and the
      fuel left after the call. *)
  | VClosure of (value list -> int -> (value * int, string) result)

val equal : value -> value -> bool
val to_string : value -> string
val is_primitive : string -> bool

val primitive_arity : string -> int option

val apply_primitive : string -> value list -> (value, string) result
