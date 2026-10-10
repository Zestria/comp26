[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** Rename each variable binder to a distinct name and update its uses.
    Free names and built-ins stay unchanged. Top-level definitions are
    processed in source order, including repeated names. *)
val alpha : Ast.program -> Ast.program
