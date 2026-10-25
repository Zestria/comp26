[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Generate RISC-V 64 assembly from ANF after closure conversion and lambda lifting. *)
val gen_program : Anf.program -> (Riscv.asm, string) result
