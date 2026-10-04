[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmId of Ast.ident

type cexpr =
  | CImm of immexpr
  | CBinOp of Ast.binop * immexpr * immexpr
  | CUnOp of Ast.unop * immexpr
  | CApp of immexpr * immexpr list (* f a1 ... an *)
  | CIf of immexpr * aexpr * aexpr
  (** Условие --- атом: [if f x then ...] превращается в
          [let t = f x in if t then ...]. Ветки остаются [aexpr], их
          вычисления нельзя выносить наружу. *)

and aexpr =
  | ALet of Ast.pattern * cexpr * aexpr
  (** [let x = ...], [let _ = ...], [let () = ...] *)
  | ACExpr of cexpr

type decl =
  | ADecl of Ast.rec_flag * Ast.pattern * Ast.pattern list * aexpr
  (** [let [rec] name p1 ... pn = body]; для значения список параметров пуст. *)
  | AEval of aexpr

type aprogram = decl list
