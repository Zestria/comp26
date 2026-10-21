[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Evaluate a fully known, correctly saturated primitive when its operand
    types and totality are established by the constants themselves. [None]
    means to retain the call, not to report a type or runtime error.

    Currently folds boolean operations and comparisons of booleans or units.
    Integer operations are not folded. No expression evaluation, binding
    lookup, type inference or effect analysis belongs to this module. *)
val apply : Primitive.t -> Frontend.Constant.t list -> Frontend.Constant.t option
