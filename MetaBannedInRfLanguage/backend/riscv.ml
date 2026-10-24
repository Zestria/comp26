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

let check_imm12 name imm =
  if imm < -2048 || imm > 2047
  then
    invalid_arg
      (Printf.sprintf "Riscv.%s: immediate %d is outside [-2048, 2047]" name imm)
;;

let pp_rrr name rd rs1 rs2 =
  Printf.sprintf
    "  %s %s, %s, %s"
    name
    (reg_to_string rd)
    (reg_to_string rs1)
    (reg_to_string rs2)
;;

let pp_rr name rd rs =
  Printf.sprintf "  %s %s, %s" name (reg_to_string rd) (reg_to_string rs)
;;

let pp_rri name rd rs imm =
  check_imm12 name imm;
  Printf.sprintf "  %s %s, %s, %d" name (reg_to_string rd) (reg_to_string rs) imm
;;

let pp_mem name reg { offset; base } =
  check_imm12 name offset;
  Printf.sprintf "  %s %s, %d(%s)" name (reg_to_string reg) offset (reg_to_string base)
;;

let pp_branch name rs1 rs2 label =
  Printf.sprintf "  %s %s, %s, %s" name (reg_to_string rs1) (reg_to_string rs2) label
;;

let pp_instr = function
  | Add (rd, rs1, rs2) -> pp_rrr "add" rd rs1 rs2
  | Sub (rd, rs1, rs2) -> pp_rrr "sub" rd rs1 rs2
  | Mul (rd, rs1, rs2) -> pp_rrr "mul" rd rs1 rs2
  | Div (rd, rs1, rs2) -> pp_rrr "div" rd rs1 rs2
  | And (rd, rs1, rs2) -> pp_rrr "and" rd rs1 rs2
  | Or (rd, rs1, rs2) -> pp_rrr "or" rd rs1 rs2
  | Xor (rd, rs1, rs2) -> pp_rrr "xor" rd rs1 rs2
  | Addi (rd, rs, imm) -> pp_rri "addi" rd rs imm
  | Slt (rd, rs1, rs2) -> pp_rrr "slt" rd rs1 rs2
  | Seqz (rd, rs) -> pp_rr "seqz" rd rs
  | Snez (rd, rs) -> pp_rr "snez" rd rs
  | Ld (rd, mem) -> pp_mem "ld" rd mem
  | Sd (rs, mem) -> pp_mem "sd" rs mem
  | Beq (rs1, rs2, label) -> pp_branch "beq" rs1 rs2 label
  | Bne (rs1, rs2, label) -> pp_branch "bne" rs1 rs2 label
  | Blt (rs1, rs2, label) -> pp_branch "blt" rs1 rs2 label
  | Bge (rs1, rs2, label) -> pp_branch "bge" rs1 rs2 label
  | J label -> Printf.sprintf "  j %s" label
  | Call label -> Printf.sprintf "  call %s" label
  | Ret -> "  ret"
  | Label label -> Printf.sprintf "%s:" label
  | Li (rd, imm) -> Printf.sprintf "  li %s, %d" (reg_to_string rd) imm
  | La (rd, label) -> Printf.sprintf "  la %s, %s" (reg_to_string rd) label
  | Mv (rd, rs) -> pp_rr "mv" rd rs
;;

let pp_directive = function
  | Section section ->
    (match section with
     | Text -> "  .text"
     | Data -> "  .data"
     | Bss -> "  .bss"
     | Rodata -> "  .section .rodata")
  | Globl label -> Printf.sprintf "  .globl %s" label
  | P2align n ->
    if n < 0 then invalid_arg "Riscv.pp_directive: negative alignment exponent";
    Printf.sprintf "  .p2align %d" n
  | Quad n -> Printf.sprintf "  .quad %Ld" n
  | Zero bytes ->
    if bytes < 0 then invalid_arg "Riscv.pp_directive: negative zero-fill size";
    Printf.sprintf "  .zero %d" bytes
;;

let pp_asm_item = function
  | Instruction instr -> pp_instr instr
  | Directive directive -> pp_directive directive
;;

let pp_asm (asm : asm) =
  match asm with
  | [] -> ""
  | _ -> String.concat "\n" (List.map pp_asm_item asm) ^ "\n"
;;
