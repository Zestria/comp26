[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** Values that can be used directly as operands without evaluation. *)
type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmId of Ast.ident

(** One operation with immediate operands. Conditional branches remain ANF
    expressions so that only the selected branch is evaluated. *)
type cexpr =
  | CImm of immexpr
  | CBinOp of Ast.binop * immexpr * immexpr
  | CUnOp of Ast.unop * immexpr
  | CApp of immexpr * immexpr list
  | CIf of immexpr * aexpr * aexpr

(** A sequence of bindings ending in one computation. *)
and aexpr =
  | ALet of Ast.pattern * cexpr * aexpr
  | ACExpr of cexpr

(** A top-level function or value, or a top-level expression. Function
    parameters are explicit; function bodies contain no abstractions. *)
type decl =
  | ADecl of Ast.rec_flag * Ast.pattern * Ast.pattern list * aexpr
  | AEval of aexpr

type aprogram = decl list
