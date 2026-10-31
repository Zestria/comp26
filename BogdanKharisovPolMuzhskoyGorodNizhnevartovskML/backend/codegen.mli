[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_middleend

(** RISC-V 64 code for a lifted program, linked with [runtime/runtime.c].
    The value representation, calling convention and runtime symbols are
    specified in [runtime/runtime.c] and in [Abi]. *)

type error =
  | Literal_out_of_range of string (** integer literal outside [[-2^62, 2^62 - 1]] *)
  | Internal of string (** broken compiler invariant; never for valid input *)

val error_to_string : error -> string

(** The complete assembly file, exporting [ml_main]. *)
val compile : Lifted.program -> (string, error) result
