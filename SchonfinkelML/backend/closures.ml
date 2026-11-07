[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Anf = SchonfinkelML_middleend.Anf

type call =
  | Direct of string
  | Partial of string * int
  | Over of string * int
  | Unknown

let classify ~arity_of head nargs =
  match head with
  | Anf.ImmId f ->
    (match arity_of f with
     | Some arity when nargs = arity -> Direct f
     | Some arity when nargs < arity -> Partial (f, arity)
     | Some arity -> Over (f, arity)
     | None -> Unknown)
  | Anf.ImmNum _ | Anf.ImmBool _ | Anf.ImmUnit -> Unknown
;;
