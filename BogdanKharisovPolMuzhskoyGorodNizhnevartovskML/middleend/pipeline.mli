[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | Source_error of Lower.error
  | Recursive_error of Recursive_check.error
  | Invariant_error of string * Lambda.invariant_error
  | Closure_error of Closure.error
  | Lifted_error of Lifted.error
  | Identifier_error of Id.error

val error_to_string : error -> string

(* Each entry point starts an independent translation. To combine results,
    call the individual passes with one continuous [Id.supply]. *)
val anf : Bkml_frontend.Ast.program -> (Anf.program, error) result

(** Run source checks, normalization and closure conversion, validating every
    intermediate representation. Function code remains nested. *)
val cc : Bkml_frontend.Ast.program -> (Closure.expression, error) result

val compile : Bkml_frontend.Ast.program -> (Lifted.program, error) result
val compile_expression : Bkml_frontend.Ast.expression -> (Lifted.program, error) result
