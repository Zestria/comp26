[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Well-typed programs of the source language, for checking that the
    middle-end passes preserve semantics. *)
val arb_structure : Ast.structure QCheck.arbitrary

(** Programs paired with an integer result binding to observe. Tuple fields
    are projected and function values are applied to generated arguments. *)
val arb_observed_structure : (Ast.structure * string) QCheck.arbitrary

(** Curried functions are partially applied, then their closures are applied
    to obtain an integer result. *)
val arb_applied_structure : (Ast.structure * string) QCheck.arbitrary
