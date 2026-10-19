[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Identity of a binding, independent of its spelling. *)
type t

(** Immutable state for allocating binding identities. *)
type supply

(** Start an independent translation. Terms that will be combined must use one
    continuous supply, including across compiler passes. Restarting from [initial]
    repeats identities; it does not create a separate namespace. *)
val initial : supply

(** No further identities can be allocated from this supply. *)
type error = Exhausted

val error_to_string : error -> string

(** Allocate an identity and return the supply for subsequent allocations.
    Names are diagnostic hints and need not be valid source identifiers.
    Returns [Error Exhausted] instead of reusing an identity after overflow. *)
val fresh : supply -> string -> (t * supply, error) result

(** Allocate a new binding with the same diagnostic name. *)
val rename : supply -> t -> (t * supply, error) result

val name : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
