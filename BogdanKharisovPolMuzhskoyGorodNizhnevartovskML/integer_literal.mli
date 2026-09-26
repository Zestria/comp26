(* SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Decimal integer literals, without a machine integer range restriction. *)
type t

type error = Invalid_integer_literal of string

(** Accepts an optional minus, a digit, then digits or underscores. *)
val of_string : string -> (t, error) result

val to_string : t -> string
val negate : t -> t
val error_to_string : error -> string
