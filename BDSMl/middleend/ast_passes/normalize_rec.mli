[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** Normalize an alpha-renamed program before closure conversion.
    Hoist initializer bindings that do not use the recursive name, preserving
    their evaluation order. Remove recursion from non-function values that do
    not use their own name; reject unsupported recursive initializers. *)
val run : Ast.program -> (Ast.program, string) result
