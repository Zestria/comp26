[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Predicates shared by the parser unit tests: each one runs the same source
    through the top-down and the menhir-generated bottom-up parser and answers
    whether the two engines agree with the expected result.  The [let%test]s of
    the module are the only callers, so the predicates are exported to keep the
    module free of unused values. *)

open SchonfinkelML_frontend.Common

(** [check_expr_both ?ops src expected] is [true] when [src] parses to [expected]
    in both engines.  A [~ops] table is a top-down-only feature, so the bottom-up
    parser is not consulted when one is given. *)
val check_expr_both : ?ops:Top_down.Operators.t -> string -> expr -> bool

(** [check_error_both ?ops src] is [true] when both engines reject [src]; as in
    [check_expr_both], a [~ops] table disables the bottom-up half. *)
val check_error_both : ?ops:Top_down.Operators.t -> string -> bool

(** [does_not_raise_both src] is [true] when neither engine raises on [src]. *)
val does_not_raise_both : string -> bool
