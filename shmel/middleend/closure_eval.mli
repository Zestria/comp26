[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VBuiltin
  | VClosure of closure

and closure

type outcome =
  { output : string
  ; result : (value, Eval.error) result
  }

val run : Closure_ir.program -> outcome
val pp_value : Format.formatter -> value -> unit
