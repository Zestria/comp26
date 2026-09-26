(* SPDX-License-Identifier: LGPL-3.0-or-later *)

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
