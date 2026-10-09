[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module SMap : Map.S with type key = string

type env = Value.value SMap.t

val eval_expression : env -> Ast.expression -> (Value.value, string) result
val eval_structure : Ast.structure -> (env, string) result
