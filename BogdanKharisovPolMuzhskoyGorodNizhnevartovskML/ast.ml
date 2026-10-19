[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type constant = Constant.t =
  | Int of Integer_literal.t
  | Bool of bool
  | Unit

type pattern =
  | PVar of Name.t
  | PWildcard
  | PUnit
  | PTuple of pattern * pattern * pattern list

type rec_flag =
  | Nonrecursive
  | Recursive

type expression =
  | Constant of constant
  | Var of Name.t
  | Tuple of expression * expression * expression list
  | Fun of function_definition
  | Apply of expression * expression * expression list
  | Let of binding * expression
  | If of expression * expression * expression option

and function_definition =
  { parameter : pattern
  ; parameters : pattern list
  ; body : expression
  }

and binding =
  { recursion : rec_flag
  ; pattern : pattern
  ; expression : expression
  }

type structure_item =
  | Definition of binding
  | Evaluation of expression

type program = structure_item list
