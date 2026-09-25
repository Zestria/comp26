(* SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Validated, unqualified value names. *)
type t

type error =
  | Invalid_identifier of string
  | Invalid_operator of string

(** An ordinary ASCII identifier, excluding language's keywords and the wildcard [_]. *)
val of_identifier : string -> (t, error) result

(** A prefix or infix operator name without surrounding parentheses. *)
val of_operator : string -> (t, error) result

val to_string : t -> string
val error_to_string : error -> string
