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

    A recursive function stays in place, keeps its chain of parameters and calls
    itself directly; only the abstractions nested deeper inside it are hoisted. *)
val lift : expr -> expr
