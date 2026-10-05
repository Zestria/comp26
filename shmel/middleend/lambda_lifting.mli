[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Moves the closed functions produced by {!Closure_conversion} to fresh
    top-level definitions. Partial applications and the order of evaluation
    of the original declarations are preserved. The result contains no
    [CFun] or local [ALetRec] nodes. Main expressions become fresh top-level
    value bindings, so the printed result is an ordinary OCaml program. *)
val of_program : Anf.program -> Anf.program
