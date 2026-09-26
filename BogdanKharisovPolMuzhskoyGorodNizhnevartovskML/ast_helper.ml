[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let binary name left right = Apply (Var name, left, [ right ])

let negate = function
  | Constant (Int literal) -> Constant (Int (Integer_literal.negate literal))
  | expression -> Apply (Var Name.unary_minus, expression, [])
;;
