[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** Hoist local functions before the top-level item that creates them.
    Expects alpha conversion and closure conversion to have run first.
    In the result, abstractions occur only at the start of top-level
    definitions, and local recursive function bindings have been removed. *)
val lambda_lift : Ast.program -> Ast.program
