[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Values and computations over a private persistent heap. *)
type t

type kind =
  | Integer
  | Boolean
  | Unit_value
  | Tuple_value
  | Function_value

type runtime_error =
  | Division_by_zero
  | Functional_comparison

type invariant_error =
  | Uninitialized_value of t
  | Unexpected_value of kind * kind
  | Field_out_of_bounds of int * int
  | Incompatible_comparison of kind * kind
  | Unbound_variable of Bkml_middleend.Id.t
  | Unknown_code of Bkml_middleend.Id.t
  | Capture_arity of int * int
  | Call_arity of int * int

type error =
  | Runtime of runtime_error
  | Invalid_ir of invariant_error
  | Unsupported_integer of Bkml_frontend.Integer_literal.t
  | Heap_exhausted

val runtime_error_to_string : runtime_error -> string
val error_to_string : error -> string

type 'a evaluation

type node =
  | Int of int
  | Bool of bool
  | Unit
  | Tuple of t list
  | Function of (t -> t evaluation)

val return : 'a -> 'a evaluation
val fail : error -> 'a evaluation
val variable : t Bkml_middleend.Id.Map.t -> Bkml_middleend.Id.t -> t evaluation
val ( let* ) : 'a evaluation -> ('a -> 'b evaluation) -> 'b evaluation
val make : node -> t evaluation

(** Reserve a recursive address, eagerly evaluate its initializer, then backpatch.
    The initializer may copy the address but may not inspect it before completion. *)
val recursive : (t -> t evaluation) -> t evaluation

val boolean : t -> bool evaluation
val field : t -> int -> t evaluation
val constant : Bkml_frontend.Constant.t -> t evaluation
val primitive : Bkml_middleend.Primitive.t -> t evaluation

val function_
  :  t Bkml_middleend.Id.Map.t
  -> Bkml_middleend.Id.t list
  -> (t Bkml_middleend.Id.Map.t -> t evaluation)
  -> t evaluation

val map : ('a -> 'b evaluation) -> 'a list -> 'b list evaluation
val apply_arguments : t -> t list -> t evaluation

(** Run with a fresh heap. Successful values are rendered for test comparison.
    Runtime errors remain distinct from invalid IR and interpreter limitations. *)
val observe : (unit -> t evaluation) -> (string, error) result

(** As [observe], but also return all text printed by the program, in order. *)
val observe_with_output : (unit -> t evaluation) -> (string * string, error) result
