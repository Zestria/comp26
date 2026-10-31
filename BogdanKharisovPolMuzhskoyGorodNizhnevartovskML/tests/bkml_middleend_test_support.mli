[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Lambda_eval = Lambda_eval
module Anf_eval = Anf_eval
module Eval_value = Eval_value

val fail : string -> string -> 'a
val allocated : string -> ('a, Bkml_middleend.Id.error) result -> 'a

val fresh
  :  Bkml_middleend.Id.supply
  -> string
  -> Bkml_middleend.Id.t * Bkml_middleend.Id.supply

val rename
  :  Bkml_middleend.Id.supply
  -> Bkml_middleend.Id.t
  -> Bkml_middleend.Id.t * Bkml_middleend.Id.supply

val parsed : (string -> ('a, string) result) -> string -> 'a
val invariant_error : Bkml_middleend.Lambda.invariant_error -> string

val checked
  :  string
  -> Bkml_middleend.Lambda.expression
  -> Bkml_middleend.Lambda.expression

val lower_with_supply
  :  Bkml_middleend.Id.supply
  -> string
  -> Bkml_middleend.Lambda.expression * Bkml_middleend.Id.supply

val lower : string -> Bkml_middleend.Lambda.expression
val equal : string -> string -> string -> bool

(** Render program outcomes; invalid IR and interpreter limitations fail the test.
    Unexpected host exceptions retain the source and original backtrace. *)
val observe : string -> ('a -> (string, Eval_value.error) result) -> 'a -> string

(** Configure failure diagnostics and run the test executable. *)
val run_tests : QCheck.Test.t list -> unit

val cps_factorial : int -> string
val cps_fibonacci : int -> string
