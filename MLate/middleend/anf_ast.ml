[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Ast = MLate_frontend.Ast

type immexpr =
  | IInt of int
  | IVar of Ast.name

type cexpr =
  | CImm of immexpr
  | CBinop of Ast.binop * immexpr * immexpr
  | CApp of immexpr * immexpr
  | CFun of Ast.name * aexpr
  | CIf of immexpr * aexpr * aexpr

and aexpr =
  | ACExpr of cexpr
  | ALet of Ast.rec_flag * Ast.name * cexpr * aexpr

type binding =
  { flag : Ast.rec_flag
  ; name : Ast.name
  ; body : aexpr
  }

type program = binding list
