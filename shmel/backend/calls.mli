[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Generates an application of a closure through [shmel_apply]. [load] must
    read operands from frame-pointer-relative slots or globals, as provided
    by {!Codegen}. It may use [t6] as address scratch but must not change [sp].
    The generated code uses [t0], [t1], [a0] .. [a2], and the runtime may
    destroy all caller-saved registers. It restores [sp] and returns in [a0]. *)
val call
  :  load:(Riscv.reg -> Shmel_middleend.Anf.imm -> (Riscv.instr list, string) result)
  -> Shmel_middleend.Anf.imm
  -> Shmel_middleend.Anf.imm list
  -> (Riscv.instr list, string) result

(** Connects closure calls to a global loader supplied by the whole-program
    backend. A global function's value must be a closure pointer, not a raw
    code address. The loader follows {!Codegen.hooks}'s register contract. *)
val hooks
  :  load_global:(Riscv.reg -> string -> (Riscv.instr list, string) result)
  -> Codegen.hooks
