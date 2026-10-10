[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module SSet : Set.S with type elt = string

val program_idents : Anf.program -> SSet.t

type t

val create : taken:SSet.t -> t
val create_from_program : Anf.program -> t

(** [fresh ~suffix t base] is a name [base ^ suffix ^ n] that is neither taken
    nor generated before, and the generator that remembers it. *)
val fresh : ?suffix:string -> t -> string -> string * t

(** A computation that draws fresh names. *)
type 'a gen = t -> 'a * t

val return : 'a -> 'a gen
val ( let* ) : 'a gen -> ('a -> 'b gen) -> 'b gen
val name : ?suffix:string -> string -> string gen
val map_list : ('a -> 'b gen) -> 'a list -> 'b list gen
val run : t -> 'a gen -> 'a
