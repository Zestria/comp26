[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Make every function's captures explicit. The input must satisfy
    [Anf.validate]; the result satisfies [Closure.validate]. Free variables are
    computed bottom-up once, before assigning fresh environment slots.
    Continue the supply used to build the input; return the supply for later passes. *)
val convert
  :  Id.supply
  -> Anf.expression
  -> (Closure.expression * Id.supply, Id.error) result
