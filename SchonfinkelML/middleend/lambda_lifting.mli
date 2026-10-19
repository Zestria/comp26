[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common

(** Lambda lifting on the ordinary AST.  The pass is meant to run right after
    closure conversion, so every abstraction in its input is already closed:
    nothing has to become an extra parameter.

    Every abstraction that is not part of the right-hand side of a [let rec] is
    hoisted to the front of the expression, and the abstraction itself is
    replaced by the name of the lifted function.  A curried chain is lifted as a
    whole, so a lifted function is never partially applied just to pass its own
    parameters: the pass creates no closure of its own.

    A recursive binding cannot be replaced by the name of its chain -- its body
    calls it by name -- but the binding itself is floated to the front as it is:
    the chain keeps its parameters and calls itself directly, and the function is
    created once instead of on every evaluation of the binding. *)
val lift : expr -> expr
