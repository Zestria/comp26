[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** Convert a lifted program; report remaining local recursion or abstractions. *)
val anf : Ast.program -> (Anf_ast.aprogram, string) result
