[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Frontend.Ast
open Anf

exception Eval_error of string

type body =
  | Src of A.expr
  | Anf of aexp

type value =
  | VInt of int
  | VBool of bool
  | VClos of
      { params : string list
      ; body : body
      ; env : env
      ; self_name : string option
      ; self_cell : value option ref
      }

and env = (string * value) list

val eval_src : Frontend.Ast.prog -> value
val eval_anf : aprogram -> value
