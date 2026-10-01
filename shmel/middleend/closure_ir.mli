[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type imm = Anf.imm =
  | ImmInt of int
  | ImmVar of string

type cond = Anf.cond =
  | CondImm of imm
  | CondCmp of Shmel_lib.Ast.binop * imm * imm

type cexpr =
  | CImm of imm
  | CNeg of imm
  | CBinOp of Shmel_lib.Ast.binop * imm * imm
  | CIf of cond * aexpr * aexpr
  | MakeClosure of code * imm list
  | GetEnv of string * int
  | ApplyClosure of imm * imm list

and aexpr =
  | ALet of string * cexpr * aexpr
  | AExpr of cexpr

and code =
  { label : string
  ; self : string
  ; params : string list
  ; captures : string list
  ; body : aexpr
  }

type decl =
  | DLet of string * aexpr
  | DExpr of aexpr

type program = decl list

val pp_program : Format.formatter -> program -> unit
