[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base

type ident = string [@@deriving show { with_path = false }]

type constant =
  | CInt of int
  | CBool of bool
  | CUnit
[@@deriving show { with_path = false }]

type rec_flag =
  | Rec
  | NonRec
[@@deriving show { with_path = false }]

type pattern =
  | PAny
  | PVar of ident
  | PConstant of constant
  | PTuple of pattern * pattern * pattern list
[@@deriving show { with_path = false }]

type expression =
  | EConst of constant
  | EIdent of ident
  | EFun of pattern * expression
  | EApply of expression * expression
  | ETuple of expression * expression * expression list
  | EIf of expression * expression * expression option
  | ELet of rec_flag * value_binding * value_binding list * expression
  | EFunction of case * case list
  | EMatch of expression * case * case list
[@@deriving show { with_path = false }]

and value_binding =
  { vb_pat : pattern
  ; vb_expr : expression
  }
[@@deriving show { with_path = false }]

and case =
  { case_pat : pattern
  ; case_expr : expression
  }
[@@deriving show { with_path = false }]

type structure_item =
  | SEval of expression
  | SValue of rec_flag * value_binding * value_binding list
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
