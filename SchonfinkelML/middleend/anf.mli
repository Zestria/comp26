[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common

(** A-normal form: every argument of an application, every condition and every
    right-hand side of a let-binding is an immediate expression, and every
    intermediate computation is bound to a fresh name. *)

type immexpr =
  | ImmNum of int
  | ImmId of string
  | ImmBool of bool
  | ImmUnit

type cexpr =
  | CBinop of string * immexpr * immexpr
  | CImmExpr of immexpr
  | CIfExpr of immexpr * aexpr * aexpr
  | CApp of immexpr * immexpr * immexpr list
  | CAbstr of string * aexpr

and aexpr =
  (* The right-hand side is an aexpr, not a cexpr: a nested let in the
     right-hand side of a recursive binding has to stay in scope of the name. *)
  | ALet of let_flag * string * aexpr * aexpr
  | ACExpr of cexpr

val create_anf : expr -> aexpr
val string_of_aexpr : aexpr -> string
