[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Ast
open Anf

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
      }
  | VAddr of int

and env = (string * value) list

val eval_src : A.prog -> (value, string) result
val eval_anf : aprogram -> (value, string) result
val eval_cc : aprogram -> (value, string) result
val eval_ll : aprogram -> (value, string) result
