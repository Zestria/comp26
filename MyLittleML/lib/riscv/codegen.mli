[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V 64 code generator (Task 3). *)

(** Raised by both stages for programs the backend does not support
    (partial applications, too many parameters, …).  The message is
    user-facing. *)
exception Unsupported of string

(** Lower a lambda-lifted ANF program (Task 2 output) into the {!Program}
    IL: nested recursive lambdas are hoisted, curried application chains
    are merged into multi-argument calls, constant globals are inlined and
    [print_int] applications become {!Program.PrintInt}. *)
val from_aprogram : Mylittleml.Middleend.Anf.aprogram -> Program.program

(** Emit RISC-V 64 assembly for a {!Program.program}.  The [print_int]
    helper is appended iff some body uses {!Program.PrintInt}. *)
val compile_program : Program.program -> Asm.t
