[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* RISC-V calling convention:
   - Argument and temporary registers are caller-saved.
   - Saved registers are callee-saved; S0 is the frame pointer. *)
type reg =
  | X0         (* hardwired to zero; writes are ignored *)
  | RA         (* return address for calls *)
  | SP         (* stack pointer *)
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
               (* function arguments; A0-A1 also hold return values *)
  | T0 | T1 | T2 | T3 | T4 | T5 | T6
               (* temporaries *)
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
               (* saved; preserved across calls. S0 is used as frame pointer *)

let x0 = X0
let ra = RA
let sp = SP
let fp = S0
let a0 = A0
let a1 = A1
let a2 = A2
let t0 = T0
let t1 = T1
let t2 = T2

let reg_to_string = function
  | X0 -> "x0"
  | RA -> "ra"
  | SP -> "sp"
  | A0 -> "a0"
  | A1 -> "a1"
  | A2 -> "a2"
  | A3 -> "a3"
  | A4 -> "a4"
  | A5 -> "a5"
  | A6 -> "a6"
  | A7 -> "a7"
  | T0 -> "t0"
  | T1 -> "t1"
  | T2 -> "t2"
  | T3 -> "t3"
  | T4 -> "t4"
  | T5 -> "t5"
  | T6 -> "t6"
  | S0 -> "fp"
  | S1 -> "s1"
  | S2 -> "s2"
  | S3 -> "s3"
  | S4 -> "s4"
  | S5 -> "s5"
  | S6 -> "s6"
  | S7 -> "s7"
  | S8 -> "s8"
  | S9 -> "s9"
  | S10 -> "s10"
  | S11 -> "s11"
;;

(* Address [offset(base)]; offsets are measured in bytes. *)
type mem =
  { offset : int
  ; base : reg
  }

type instr =
  (* Arithmetic *)
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Xor of reg * reg * reg
  | Addi of reg * reg * int
  (* Comparisons *)
  | Slt of reg * reg * reg
  | Seqz of reg * reg
  | Snez of reg * reg
  (* Physical memory accesses *)
  | Ld of reg * mem
  | Sd of reg * mem
  (* Control flow *)
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | Blt of reg * reg * string
  | Bge of reg * reg * string
  | Label of string
  (* Assembler pseudoinstructions *)
  | J of string
  | Call of string
  | Ret
  | Li of reg * int
  | La of reg * string
  | Mv of reg * reg

let add k rd rs1 rs2 = k (Add (rd, rs1, rs2))
let sub k rd rs1 rs2 = k (Sub (rd, rs1, rs2))
let mul k rd rs1 rs2 = k (Mul (rd, rs1, rs2))
let div k rd rs1 rs2 = k (Div (rd, rs1, rs2))
let and_ k rd rs1 rs2 = k (And (rd, rs1, rs2))
let or_ k rd rs1 rs2 = k (Or (rd, rs1, rs2))
let xor k rd rs1 rs2 = k (Xor (rd, rs1, rs2))
let addi k rd rs imm = k (Addi (rd, rs, imm))
let slt k rd rs1 rs2 = k (Slt (rd, rs1, rs2))
let seqz k rd rs = k (Seqz (rd, rs))
let snez k rd rs = k (Snez (rd, rs))
let ld k rd mem = k (Ld (rd, mem))
let sd k rs mem = k (Sd (rs, mem))
let beq k rs1 rs2 label = k (Beq (rs1, rs2, label))
let bne k rs1 rs2 label = k (Bne (rs1, rs2, label))
let blt k rs1 rs2 label = k (Blt (rs1, rs2, label))
let bge k rs1 rs2 label = k (Bge (rs1, rs2, label))
let j k label = k (J label)
let call k label = k (Call label)
let ret k = k Ret
let label k name = k (Label name)
let li k rd imm = k (Li (rd, imm))
let la k rd label = k (La (rd, label))
let mv k rd rs = k (Mv (rd, rs))

type section =
  | Text
  | Data
  | Bss
  | Rodata

type directive =
  | Section of section
  | Globl of string
  | P2align of int (* Align to 2^n bytes *)
  | Quad of int64
  | Zero of int (* Reserve this many zero-initialized bytes *)

type asm_item =
  | Instruction of instr
  | Directive of directive

type asm = asm_item list
