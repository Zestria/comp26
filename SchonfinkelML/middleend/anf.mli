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

module StringSet : Set.S with type elt = string

type naming =
  { reserved : StringSet.t
  ; counter : int
  }

val naming_of : StringSet.t -> naming
val naming_of_aexpr : aexpr -> naming
val gensym : naming -> ?prefix:string -> unit -> string * naming
val gensym_id : naming -> unit -> string * naming
val fv_imm : immexpr -> StringSet.t
val fv_cexpr : cexpr -> StringSet.t
val fv_aexpr : aexpr -> StringSet.t
val free_vars : aexpr -> string list
val all_idents : aexpr -> StringSet.t
val collect_params : aexpr -> string list * aexpr
val build_abstr : string list -> aexpr -> aexpr
val idents_of_expr : StringSet.t -> expr -> StringSet.t
