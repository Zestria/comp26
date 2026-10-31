[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Negate
  | Equal
  | Not_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | And
  | Or
  | Not
  | Print_int

val name : t -> string
val all : t list
