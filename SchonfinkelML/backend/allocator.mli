[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Allocator : sig
  val registers : Common.reg list
  val next_register : Common.reg list ref
  val alloc : unit -> Common.reg
  val find_var : (string * 'a) list -> string -> 'a
end
