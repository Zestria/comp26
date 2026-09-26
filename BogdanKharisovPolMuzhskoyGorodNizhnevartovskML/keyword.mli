[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t =
  | Else
  | False
  | Fun
  | If
  | In
  | Let
  | Rec
  | Then
  | True
  | Mod

val to_string : t -> string
val of_string : string -> t option
