[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Common
open Emitter

let string_of_reg = function
  | Zero -> "zero"
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
  | S0 -> "s0"
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

let write_instr fd = function
  | LB (rd, base, off) ->
    Printf.fprintf fd "\tlb %s, %d(%s)\n" (string_of_reg rd) off (string_of_reg base)
  | LH (rd, base, off) ->
    Printf.fprintf fd "\tlh %s, %d(%s)\n" (string_of_reg rd) off (string_of_reg base)
  | LW (rd, base, off) ->
    Printf.fprintf fd "\tlw %s, %d(%s)\n" (string_of_reg rd) off (string_of_reg base)
  | LD (rd, base, off) ->
    Printf.fprintf fd "\tld %s, %d(%s)\n" (string_of_reg rd) off (string_of_reg base)
  | LBU (rd, base, off) ->
    Printf.fprintf fd "\tlbu %s, %d(%s)\n" (string_of_reg rd) off (string_of_reg base)
  | LHU (rd, base, off) ->
    Printf.fprintf fd "\tlhu %s, %d(%s)\n" (string_of_reg rd) off (string_of_reg base)
  | LWU (rd, base, off) ->
    Printf.fprintf fd "\tlwu %s, %d(%s)\n" (string_of_reg rd) off (string_of_reg base)
  | SB (rs, base, off) ->
    Printf.fprintf fd "\tsb %s, %d(%s)\n" (string_of_reg rs) off (string_of_reg base)
  | SH (rs, base, off) ->
    Printf.fprintf fd "\tsh %s, %d(%s)\n" (string_of_reg rs) off (string_of_reg base)
  | SW (rs, base, off) ->
    Printf.fprintf fd "\tsw %s, %d(%s)\n" (string_of_reg rs) off (string_of_reg base)
  | SD (rs, base, off) ->
    Printf.fprintf fd "\tsd %s, %d(%s)\n" (string_of_reg rs) off (string_of_reg base)
  | ADDI (rd, rs, imm) ->
    Printf.fprintf fd "\taddi %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | SLTI (rd, rs, imm) ->
    Printf.fprintf fd "\tslti %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | SLTIU (rd, rs, imm) ->
    Printf.fprintf fd "\tsltiu %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | XORI (rd, rs, imm) ->
    Printf.fprintf fd "\txori %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | ORI (rd, rs, imm) ->
    Printf.fprintf fd "\tori %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | ANDI (rd, rs, imm) ->
    Printf.fprintf fd "\tandi %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | SLLI (rd, rs, imm) ->
    Printf.fprintf fd "\tslli %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | SRLI (rd, rs, imm) ->
    Printf.fprintf fd "\tsrli %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | SRAI (rd, rs, imm) ->
    Printf.fprintf fd "\tsrai %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | ADD (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tadd %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SUB (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsub %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SLL (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsll %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SLT (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tslt %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SLTU (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsltu %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | XOR (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\txor %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SRL (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsrl %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SRA (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsra %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | OR (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tor %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | AND (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tand %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | LUI (rd, imm) -> Printf.fprintf fd "\tlui %s, %d\n" (string_of_reg rd) imm
  | AUIPC (rd, imm) -> Printf.fprintf fd "\tauipc %s, %d\n" (string_of_reg rd) imm
  | JAL (rd, label) -> Printf.fprintf fd "\tjal %s, %s\n" (string_of_reg rd) label
  | JALR (rd, rs, imm) ->
    Printf.fprintf fd "\tjalr %s, %d(%s)\n" (string_of_reg rd) imm (string_of_reg rs)
  | BEQ (rs1, rs2, label) ->
    Printf.fprintf fd "\tbeq %s, %s, %s\n" (string_of_reg rs1) (string_of_reg rs2) label
  | BNE (rs1, rs2, label) ->
    Printf.fprintf fd "\tbne %s, %s, %s\n" (string_of_reg rs1) (string_of_reg rs2) label
  | BLT (rs1, rs2, label) ->
    Printf.fprintf fd "\tblt %s, %s, %s\n" (string_of_reg rs1) (string_of_reg rs2) label
  | BGE (rs1, rs2, label) ->
    Printf.fprintf fd "\tbge %s, %s, %s\n" (string_of_reg rs1) (string_of_reg rs2) label
  | BLTU (rs1, rs2, label) ->
    Printf.fprintf fd "\tbltu %s, %s, %s\n" (string_of_reg rs1) (string_of_reg rs2) label
  | BGEU (rs1, rs2, label) ->
    Printf.fprintf fd "\tbgeu %s, %s, %s\n" (string_of_reg rs1) (string_of_reg rs2) label
  | MUL (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tmul %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | MULH (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tmulh %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | MULHSU (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tmulhsu %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | MULHU (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tmulhu %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | DIV (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tdiv %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | DIVU (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tdivu %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | REM (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\trem %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | REMU (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tremu %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | ADDIW (rd, rs, imm) ->
    Printf.fprintf fd "\taddiw %s, %s, %d\n" (string_of_reg rd) (string_of_reg rs) imm
  | ADDW (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\taddw %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SUBW (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsubw %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SLLW (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsllw %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SRLW (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsrlw %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | SRAW (rd, rs1, rs2) ->
    Printf.fprintf
      fd
      "\tsraw %s, %s, %s\n"
      (string_of_reg rd)
      (string_of_reg rs1)
      (string_of_reg rs2)
  | ECALL -> Printf.fprintf fd "\tecall\n"
  | MV (rd, rs) -> Printf.fprintf fd "\tmv %s, %s\n" (string_of_reg rd) (string_of_reg rs)
  | LI (rd, imm) -> Printf.fprintf fd "\tli %s, %d\n" (string_of_reg rd) imm
  | NOP -> Printf.fprintf fd "\tnop\n"
  | RET -> Printf.fprintf fd "\tret\n"
;;

let write_item fd = function
  | Emitter.Label name -> Printf.fprintf fd "%s:\n" name
  | Emitter.Instruction instr -> write_instr fd instr
;;

let write (fd : out_channel) (rtl : Emitter.item list) : unit =
  Printf.fprintf fd ".global _start\n";
  Printf.fprintf fd ".text\n";
  List.iter (write_item fd) rtl;
  Printf.fprintf fd ".end\n";
  flush fd
;;
