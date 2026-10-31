[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RV64IM assembly syntax used by the backend. Constructors denote exactly one
    machine instruction (or a label/directive), except [Li] and [La], which are
    standard assembler pseudo-instructions. Register [T 6] is reserved for the
    legalizing helpers below and is never allocated nor used as an operand by
    instruction selection. *)

type reg =
  | Zero
  | Ra
  | Sp
  | Fp (** [s0], frame pointer *)
  | S of int (** [s1] .. [s11], callee-saved, allocatable *)
  | A of int (** [a0] .. [a7] *)
  | T of int (** [t0] .. [t6] *)

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Slt
  | Sltu
  | Xor
  | And
  | Or

type imm_op =
  | Addi
  | Xori
  | Andi
  | Ori
  | Slti
  | Slli
  | Srai

type branch =
  | Beq
  | Bne
  | Blt
  | Bge

type instr =
  | Li of reg * int64
  | La of reg * string
  | Mv of reg * reg
  | Binop of binop * reg * reg * reg (** [op rd, rs1, rs2] *)
  | Imm of imm_op * reg * reg * int
      (** [op rd, rs, imm]; imm must fit 12 bits, or [0 .. 63] for [Slli]/[Srai] *)
  | Seqz of reg * reg
  | Snez of reg * reg
  | Ld of reg * int * reg (** [ld rd, off(base)]; off must fit 12 bits *)
  | Sd of reg * int * reg (** [sd src, off(base)]; off must fit 12 bits *)
  | Call of string
  | Jalr of reg (** [jalr ra, 0(reg)] *)
  | J of string
  | Branch of branch * reg * reg * string
  | Beqz of reg * string
  | Bnez of reg * string
  | Label of string
  | Ret
  | Directive of string (** printed verbatim, e.g. [".globl ml_main"] *)
  | Comment of string

(** The legalization scratch register, [T 6]. *)
val scratch : reg

(** [-2048 <= n <= 2047]. *)
val fits_imm12 : int -> bool

(** [add_imm rd rs n]: [rd := rs + n]. One [addi] when [n] fits, otherwise
    [li t6, n; add rd, rs, t6]. Raises [Invalid_argument] if [rd] or [rs] is
    [scratch]. [rd = rs] is allowed. *)
val add_imm : reg -> reg -> int -> instr list

(** [load rd off base]: [ld rd, off(base)]. Out-of-range offsets use
    [li t6, off; add t6, base, t6; ld rd, 0(t6)], so [base] is read before [t6]
    is written and [rd = base] is allowed. Raises [Invalid_argument] if [rd] or
    [base] is [scratch]. *)
val load : reg -> int -> reg -> instr list

(** [store src off base]: [sd src, off(base)], legalized like [load]. Neither
    [src] nor [base] may be [scratch] (raises [Invalid_argument]); both are
    read only after [t6] has been computed from [base]. *)
val store : reg -> int -> reg -> instr list

val reg_to_string : reg -> string

(** One instruction per line, labels unindented, instructions indented with a
    tab. *)
val to_string : instr list -> string
