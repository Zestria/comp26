[@@@ocaml.text "Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors"]

[@@@ocaml.text "SPDX-License-Identifier: LGPL-3.0-or-later"]

val compile : Shmel_middleend.Anf.program -> (Riscv.instr list, string) result
