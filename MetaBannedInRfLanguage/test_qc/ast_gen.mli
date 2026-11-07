[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** QuickCheck generators of source programs for the parser round-trip tests. *)

(** Whole programs of 1 to 5 top-level items, with a shrinker. *)
val arb_structure : Ast.structure QCheck.arbitrary

(** Single expressions of size up to 20, with a shrinker. *)
val arb_expression : Ast.expression QCheck.arbitrary
