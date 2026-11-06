[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V 64 code generator (Task 3). *)

(** Lower lexical ANF into explicit closures and lifted functions.
    Pass the output of [Anf.anf_prog], before the diagnostic CC/LL passes. *)
val from_aprogram : Mylittleml.Middleend.Anf.aprogram -> (Program.program, string) result

(** Emit RISC-V 64 assembly for a {!Program.program}.  The [print_int]
    helper is appended iff some body uses {!Program.PrintInt}. *)
val compile_program : Program.program -> (Asm.t, string) result
