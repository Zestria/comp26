[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | Source_error of Lower.error
  | Invariant_error of string * Lambda.invariant_error
  | Identifier_error of Id.error

val error_to_string : error -> string

(* Each entry point starts an independent translation. To combine results,
    call the individual passes with one continuous [Id.supply]. *)
val anf : Frontend.Ast.program -> (Anf.program, error) result
