[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Interpret closure code using only its explicit environment and arguments. *)
val closed : Bkml_middleend.Closure.expression -> (string, Eval_value.error) result

val lifted : Bkml_middleend.Lifted.program -> (string, Eval_value.error) result

val lifted_with_output
  :  Bkml_middleend.Lifted.program
  -> (string * string, Eval_value.error) result
