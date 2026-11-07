[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** Values that can be used directly as operands without evaluation. *)
type immexpr =
  | ImmInt of int (** Integer literal. *)
  | ImmBool of bool (** Boolean literal. *)
  | ImmUnit (** Unit literal. *)
  | ImmId of Ast.ident (** Variable reference. *)

(** One operation with immediate operands. Conditional branches remain ANF
    expressions so that only the selected branch is evaluated. *)
type cexpr =
  | CImm of immexpr (** Immediate result. *)
  | CBinOp of Ast.binop * immexpr * immexpr (** Binary operation on immediate operands. *)
  | CUnOp of Ast.unop * immexpr (** Unary operation on an immediate operand. *)
  | CApp of immexpr * immexpr list (** Function and arguments in source order. *)
  | CIf of immexpr * aexpr * aexpr (** Condition and lazily evaluated branches. *)

(** A sequence of bindings ending in one computation. *)
and aexpr =
  | ALet of Ast.pattern * cexpr * aexpr (** Bind a computation result before the body. *)
  | ACExpr of cexpr (** Final computation. *)

(** A top-level function or value, or a top-level expression. Function
    parameters are explicit; function bodies contain no abstractions. *)
type decl =
  | ADecl of Ast.rec_flag * Ast.pattern * Ast.pattern list * aexpr (** Binding flag, name pattern, parameters, and body. *)
  | AEval of aexpr (** Top-level computation. *)

(** ANF program in declaration order. *)
type aprogram = decl list
