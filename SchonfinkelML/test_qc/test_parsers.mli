[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Unified QuickCheck properties for both parsers.

    The round-trip and idempotence properties are parameterised by a [parser]
    adapter and run against both engines; the remaining properties exercise the
    top-down-only operator table and its error positions.  The executable is
    just the final [let () = ...] that runs the tests, so every value built
    before it is exported to keep the module free of unused values. *)

open SchonfinkelML_frontend.Common

(** A parser under test, paired with the name used in the property names. *)
type parser =
  { name : string (** The engine's name, for instance ["top-down"]. *)
  ; parse_expr : string -> (expr, unit) result
    (** The engine, with every error collapsed to [Error ()] so that the
        properties can treat both parsers uniformly. *)
  }

val bottom_up : parser
val top_down : parser

(** [surface_operand e] prints [e] as an operand of an infix expression, adding
    parentheses around the forms that would otherwise re-associate. *)
val surface_operand : expr -> string

(** [surface_pp e] prints [e] in the surface syntax both parsers accept. *)
val surface_pp : expr -> string

val shrink_expr : expr -> expr QCheck.Iter.t
val arb_expr : expr QCheck.arbitrary
val arb_triple : (expr * expr * expr) QCheck.arbitrary
val arb_source : string QCheck.arbitrary
val arb_table_src : string QCheck.arbitrary
val test_roundtrip : parser -> QCheck.Test.t
val test_idempotence : parser -> QCheck.Test.t

(** [table src] is the operator table [src] denotes; it fails when the table is
    not valid, which would make the test itself wrong. *)
val table : string -> Top_down.Operators.t

val ops_right_plus : Top_down.Operators.t
val ops_plus_tighter : Top_down.Operators.t
val ops_nonassoc_eq : Top_down.Operators.t
val test_totality_lexable : QCheck.Test.t
val test_right_plus_assoc : QCheck.Test.t
val test_plus_tighter_than_mul : QCheck.Test.t
val test_nonassoc_eq : QCheck.Test.t
val test_table_roundtrip : QCheck.Test.t
