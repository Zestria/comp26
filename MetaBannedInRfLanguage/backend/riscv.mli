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

val x0 : reg
val ra : reg
val sp : reg
val fp : reg
val a0 : reg
val a1 : reg
val a2 : reg
val t0 : reg
val t1 : reg
val t2 : reg

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
  | Slli of reg * reg * int
  | Srai of reg * reg * int
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

val add : (instr -> 'a) -> reg -> reg -> reg -> 'a
val sub : (instr -> 'a) -> reg -> reg -> reg -> 'a
val mul : (instr -> 'a) -> reg -> reg -> reg -> 'a
val div : (instr -> 'a) -> reg -> reg -> reg -> 'a
val and_ : (instr -> 'a) -> reg -> reg -> reg -> 'a
val or_ : (instr -> 'a) -> reg -> reg -> reg -> 'a
val xor : (instr -> 'a) -> reg -> reg -> reg -> 'a
val addi : (instr -> 'a) -> reg -> reg -> int -> 'a
val slli : (instr -> 'a) -> reg -> reg -> int -> 'a
val srai : (instr -> 'a) -> reg -> reg -> int -> 'a
val slt : (instr -> 'a) -> reg -> reg -> reg -> 'a
val seqz : (instr -> 'a) -> reg -> reg -> 'a
val snez : (instr -> 'a) -> reg -> reg -> 'a
val ld : (instr -> 'a) -> reg -> mem -> 'a
val sd : (instr -> 'a) -> reg -> mem -> 'a
val beq : (instr -> 'a) -> reg -> reg -> string -> 'a
val bne : (instr -> 'a) -> reg -> reg -> string -> 'a
val blt : (instr -> 'a) -> reg -> reg -> string -> 'a
val bge : (instr -> 'a) -> reg -> reg -> string -> 'a
val j : (instr -> 'a) -> string -> 'a
val call : (instr -> 'a) -> string -> 'a
val ret : (instr -> 'a) -> 'a
val label : (instr -> 'a) -> string -> 'a
val li : (instr -> 'a) -> reg -> int -> 'a
val la : (instr -> 'a) -> reg -> string -> 'a
val mv : (instr -> 'a) -> reg -> reg -> 'a

type section =
  | Text
  | Data
  | Bss
  | Rodata

type directive =
  | Section of section
  | Globl of string
  | P2align of int (** Align to 2^n bytes *)
  | Quad of int
  | Zero of int (** Reserve this many zero-initialized bytes *)

type asm_item =
  | Instruction of instr
  | Directive of directive
  | Comment of string

type asm = asm_item list

val pp_asm : asm -> string
