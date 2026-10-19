[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Predicates used by the inline tests of the top-down parser: source to AST,
    plus the operator table the parser is driven by.  The [let%test]s of the
    module are the only callers, so the predicates are exported to keep the
    module free of unused values. *)

open SchonfinkelML_frontend.Common

(** [check_expr ops src expected] is [true] when [src] parses to [expected] under
    the operator table written as [ops]. *)
val check_expr : string -> string -> expr -> bool

(** [check_error ops src] is [true] when [src] is rejected under the operator
    table written as [ops]. *)
val check_error : string -> string -> bool

(** [check_table src expected] is [true] when [src] denotes an operator table
    whose printed form is [expected]. *)
val check_table : string -> string -> bool

(** [check_bad_table src] is [true] when [src] is not a valid operator table. *)
val check_bad_table : string -> bool

(** [check_round_trip src] is [true] when reading back the printed form of the
    table [src] denotes gives the same table. *)
val check_round_trip : string -> bool

(** [does_not_raise src] is [true] when the parser does not raise on [src]:
    pathological input is expected to come back as a parse error. *)
val does_not_raise : string -> bool
