[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Well-typed programs of the source language, for checking that the
    middle-end passes preserve semantics. *)
val arb_structure : Ast.structure QCheck.arbitrary
