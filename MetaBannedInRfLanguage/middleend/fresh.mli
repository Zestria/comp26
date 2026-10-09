[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module SSet : Set.S with type elt = string

val program_idents : Anf.program -> SSet.t

type t

val create : taken:SSet.t -> t
val create_from_program : Anf.program -> t
val fresh : ?suffix:string -> t -> string -> string
