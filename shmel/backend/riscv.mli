[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V 64 instructions used by the code generator, and their printing as
    GNU assembler text. *)

type reg =
  | Zero
  | Ra (** return address *)
  | Sp (** stack pointer *)
  | Fp (** frame pointer, printed as [s0] *)
  | A of int (** arguments and the result: [a0] .. [a7] *)
  | T of int (** temporaries: [t0] .. [t6] *)
  | S of int (** saved registers: [s1] .. [s11] *)

type label = string

(** Register-register operations. *)
type rop =
  | Add
  | Sub
  | Mul
  | Div
  | Slt
  | Xor

(** Register-immediate operations, the immediate is 12-bit signed. *)
type iop =
  | Addi
  | Slti
  | Xori

(** Conditional branches; [Bgt] and [Ble] are assembler pseudo-instructions. *)
type bcond =
  | Beq
  | Bne
  | Blt
  | Bge
  | Bgt
  | Ble

type instr =
  | R of rop * reg * reg * reg (** [op rd, rs1, rs2] *)
  | I of iop * reg * reg * int (** [op rd, rs1, imm] *)
  | Li of reg * int
  | La of reg * label
  | Mv of reg * reg
  | Neg of reg * reg
  | Seqz of reg * reg
  | Ld of reg * int * reg (** [ld rd, offset(base)] *)
  | Sd of reg * int * reg (** [sd rs, offset(base)] *)
  | Branch of bcond * reg * reg * label
  (** printed as [beqz]/[bnez] when compared with [zero] *)
  | J of label
  | Call of label
  | Jalr of reg (** call through a register *)
  | Ret
  | Label of label
  | Directive of string (** e.g. [.globl main] *)
  | Comment of string

(** Whether a constant fits into the 12-bit immediate of [addi] and friends. *)
val fits_imm12 : int -> bool

val pp_reg : Format.formatter -> reg -> unit
val pp_instr : Format.formatter -> instr -> unit

(** One instruction per line, labels and directives are not indented. *)
val pp_instrs : Format.formatter -> instr list -> unit
