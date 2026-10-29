[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type ident = string [@@deriving show { with_path = false }]

type imm =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmVar of ident
[@@deriving show { with_path = false }]

type cexpr =
  | CImm of imm
  | CApp of imm * imm list
  | CTuple of imm * imm * imm list
  | CIte of imm * aexpr * aexpr
  | CFun of ident list * aexpr
  | CMatchFailure
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of ident * cexpr * aexpr
  | ALetTuple of ident option list * imm * aexpr
  | ALetRec of (ident * cexpr) list * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type item =
  | AValue of ident * aexpr
  | ARec of (ident * cexpr) list
  | AEval of aexpr
[@@deriving show { with_path = false }]

type program = item list [@@deriving show { with_path = false }]
