[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** [closure_conv program] makes the environment of every function explicit.
    The input must be alpha-renamed and have recursive initializers normalized
    by [Normalize_rec.run]. Free local variables become leading parameters, and the corresponding
    values are applied when the function is created.  Top-level definitions
    and standard built-ins are treated as globals and are not captured. *)
val closure_conv : Ast.program -> Ast.program
