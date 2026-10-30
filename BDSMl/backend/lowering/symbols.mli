[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type function_info =
  { symbol : string
  ; arity : int
  }

type binding =
  | Function of function_info
  | Global of string

type t

exception Error of string

(** Requires alpha-renamed top-level bindings. Generated symbols never contain
    user text, and cannot collide with runtime or [main]. *)
val create : BDSMl_middleend.Anf.Anf_ast.aprogram -> t

val find : t -> string -> binding option
val require : t -> string -> binding
val globals : t -> string list
