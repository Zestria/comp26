[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

module String_set : Set.S with type elt = string

(** [free_vars expression] returns the identifiers used by [expression] but
    not bound inside it.  The result is ordered, so passes which turn the set
    into additional function parameters are deterministic. *)
val free_vars : Ast.expr -> String_set.t
