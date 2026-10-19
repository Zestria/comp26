[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** [captures] are values from the construction scope, corresponding in order
    to the fresh [environment] bindings visible inside the function. *)
type t =
  { environment : Id.t list
  ; parameter : Id.t
  ; parameters : Id.t list
  ; body : expression
  ; captures : Anf.atom list
  }

and expression = (t, Anf.atom) Anf.term

type error =
  | Binding of Lambda.invariant_error
  | Capture_count of int * int

val error_to_string : error -> string

(** Check ANF binding invariants, explicit closure scope and matching numbers
    of environment slots and captured values. *)
val validate : expression -> (unit, error) result
