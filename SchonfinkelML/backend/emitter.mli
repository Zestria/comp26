[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Anf = SchonfinkelML_middleend.Anf

type item =
  | Instruction of Common.instr
  | Label of string

val get_rtl : Anf.aexpr -> (item list, Common.error) result
