[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Remove adjacent redundant loads/moves/jumps and saves of [ra] in leaf
    functions. Input is code emitted by this backend, where stack saves of
    [ra] belong to the prologue/epilogue. Frame sizes and live stores remain
    unchanged. This local cleanup does not perform register allocation. *)
val run : Asm.program -> Asm.program
