[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Immutable counter and reserved identifiers. *)
type t

(** Start an independent supply, reserving all given identifiers. *)
val create : string list -> t

(** Return an unused [base_N] identifier and the updated supply. *)
val fresh_ident : string -> t -> string * t

(** Return a value without consuming identifiers. *)
val return : 'a -> t -> 'a * t

(** Sequence two computations using the same identifier supply. *)
val bind : (t -> 'a * t) -> ('a -> t -> 'b * t) -> t -> 'b * t
