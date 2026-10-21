[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Independent source and ANF interpreters for semantic comparisons. *)

type failure =
  | Runtime of string
  | Fuel_exhausted

type observation =
  { result : (Ast.constant, failure) result
  ; trace : int list
  }

val source : ?fuel:int -> Ast.expr -> observation
val source_program : ?fuel:int -> Ast.program -> observation
val anf : ?fuel:int -> Middleend.Anf.term -> observation
val anf_program : ?fuel:int -> Middleend.Anf.program -> observation
val show_observation : observation -> string
val equivalent : observation -> observation -> bool
