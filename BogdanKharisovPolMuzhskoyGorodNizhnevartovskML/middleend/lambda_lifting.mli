[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Move closed function code to program scope, retaining closure construction
    at its original evaluation point. The input must satisfy [Closure.validate].
    Continue the supply used to build the input; return the supply for later passes. *)
val lift
  :  Id.supply
  -> Closure.expression
  -> (Lifted.program * Id.supply, Id.error) result
