[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type reg =
  | Zero
  | RA
  | SP
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11

type instr =
  (* Loads *)
  | LB of reg * reg * int
  | LH of reg * reg * int
  | LW of reg * reg * int
  | LD of reg * reg * int
  | LBU of reg * reg * int
  | LHU of reg * reg * int
  | LWU of reg * reg * int
  (* Stores *)
  | SB of reg * reg * int
  | SH of reg * reg * int
  | SW of reg * reg * int
  | SD of reg * reg * int
  (* Immediate arithmetic *)
  | ADDI of reg * reg * int
  | SLTI of reg * reg * int
  | SLTIU of reg * reg * int
  | XORI of reg * reg * int
  | ORI of reg * reg * int
  | ANDI of reg * reg * int
  | SLLI of reg * reg * int
  | SRLI of reg * reg * int
  | SRAI of reg * reg * int
  (* Register arithmetic *)
  | ADD of reg * reg * reg
  | SUB of reg * reg * reg
  | SLL of reg * reg * reg
  | SLT of reg * reg * reg
  | SLTU of reg * reg * reg
  | XOR of reg * reg * reg
  | SRL of reg * reg * reg
  | SRA of reg * reg * reg
  | OR of reg * reg * reg
  | AND of reg * reg * reg
  (* Upper immediate *)
  | LUI of reg * int
  | AUIPC of reg * int
  (* Control flow *)
  | JAL of reg * string
  | JALR of reg * reg * int
  | BEQ of reg * reg * string
  | BNE of reg * reg * string
  | BLT of reg * reg * string
  | BGE of reg * reg * string
  | BLTU of reg * reg * string
  | BGEU of reg * reg * string
  (* We are going to assume that M extension is on target machine *)
  | MUL of reg * reg * reg
  | MULH of reg * reg * reg
  | MULHSU of reg * reg * reg
  | MULHU of reg * reg * reg
  | DIV of reg * reg * reg
  | DIVU of reg * reg * reg
  | REM of reg * reg * reg
  | REMU of reg * reg * reg
  (* RV64 word operations *)
  | ADDIW of reg * reg * int
  | ADDW of reg * reg * reg
  | SUBW of reg * reg * reg
  | SLLW of reg * reg * reg
  | SRLW of reg * reg * reg
  | SRAW of reg * reg * reg
  (* ecall *)
  | ECALL
  (* Pseudoinstructions *)
  | MV of reg * reg
  | LI of reg * int
  | CALL of string
  | NOP
  | RET

let arg_registers = [ A0; A1; A2; A3; A4; A5; A6; A7 ]
let caller_saved = [ RA; A0; A1; A2; A3; A4; A5; A6; A7; T0; T1; T2; T3; T4; T5; T6 ]
let callee_saved = [ S0; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11 ]

type error =
  | Invalid_immediate of
      { instruction : string
      ; value : int
      ; min : int
      ; max : int
      }
  | Invalid_label of string
  | Unsupported of string
  | Symbol_table_error
  | Invalid_application

let pp_error fmt = function
  | Invalid_immediate { instruction; value; min; max } ->
    Format.fprintf
      fmt
      "Invalid immediate for %s: %d (expected %d..%d)"
      instruction
      value
      min
      max
  | Invalid_label label -> Format.fprintf fmt "Invalid label: %S" label
  | Unsupported message -> Format.fprintf fmt "Unsupported: %s" message
  | Symbol_table_error -> Format.fprintf fmt "Symbol table is inconsistent"
  | Invalid_application -> Format.fprintf fmt "Invalid application"
;;

type 'a checked = ('a, error) result

let ok instr = Ok instr

let check_range instruction min max value build =
  if value < min || value > max
  then Error (Invalid_immediate { instruction; value; min; max })
  else Ok (build value)
;;

let imm12 name build value = check_range name (-2048) 2047 value build
let imm20 name build value = check_range name 0 1048575 value build
let shamt64 name build value = check_range name 0 63 value build

let with_label label build =
  if String.trim label = "" then Error (Invalid_label label) else Ok (build label)
;;

let lb rd base offset = imm12 "LB" (fun n -> LB (rd, base, n)) offset
let lh rd base offset = imm12 "LH" (fun n -> LH (rd, base, n)) offset
let lw rd base offset = imm12 "LW" (fun n -> LW (rd, base, n)) offset
let ld rd base offset = imm12 "LD" (fun n -> LD (rd, base, n)) offset
let lbu rd base offset = imm12 "LBU" (fun n -> LBU (rd, base, n)) offset
let lhu rd base offset = imm12 "LHU" (fun n -> LHU (rd, base, n)) offset
let lwu rd base offset = imm12 "LWU" (fun n -> LWU (rd, base, n)) offset
let sb src base offset = imm12 "SB" (fun n -> SB (src, base, n)) offset
let sh src base offset = imm12 "SH" (fun n -> SH (src, base, n)) offset
let sw src base offset = imm12 "SW" (fun n -> SW (src, base, n)) offset
let sd src base offset = imm12 "SD" (fun n -> SD (src, base, n)) offset
let addi rd rs imm = imm12 "ADDI" (fun n -> ADDI (rd, rs, n)) imm
let slti rd rs imm = imm12 "SLTI" (fun n -> SLTI (rd, rs, n)) imm
let sltiu rd rs imm = imm12 "SLTIU" (fun n -> SLTIU (rd, rs, n)) imm
let xori rd rs imm = imm12 "XORI" (fun n -> XORI (rd, rs, n)) imm
let ori rd rs imm = imm12 "ORI" (fun n -> ORI (rd, rs, n)) imm
let andi rd rs imm = imm12 "ANDI" (fun n -> ANDI (rd, rs, n)) imm
let slli rd rs shamt = shamt64 "SLLI" (fun n -> SLLI (rd, rs, n)) shamt
let srli rd rs shamt = shamt64 "SRLI" (fun n -> SRLI (rd, rs, n)) shamt
let srai rd rs shamt = shamt64 "SRAI" (fun n -> SRAI (rd, rs, n)) shamt
let add rd rs1 rs2 = ok (ADD (rd, rs1, rs2))
let sub rd rs1 rs2 = ok (SUB (rd, rs1, rs2))
let sll rd rs1 rs2 = ok (SLL (rd, rs1, rs2))
let slt rd rs1 rs2 = ok (SLT (rd, rs1, rs2))
let sltu rd rs1 rs2 = ok (SLTU (rd, rs1, rs2))
let xor rd rs1 rs2 = ok (XOR (rd, rs1, rs2))
let srl rd rs1 rs2 = ok (SRL (rd, rs1, rs2))
let sra rd rs1 rs2 = ok (SRA (rd, rs1, rs2))
let or_ rd rs1 rs2 = ok (OR (rd, rs1, rs2))
let and_ rd rs1 rs2 = ok (AND (rd, rs1, rs2))
let lui rd imm = imm20 "LUI" (fun n -> LUI (rd, n)) imm
let auipc rd imm = imm20 "AUIPC" (fun n -> AUIPC (rd, n)) imm
let jal rd label = with_label label (fun l -> JAL (rd, l))
let jalr rd rs offset = imm12 "JALR" (fun n -> JALR (rd, rs, n)) offset
let beq rs1 rs2 label = with_label label (fun l -> BEQ (rs1, rs2, l))
let bne rs1 rs2 label = with_label label (fun l -> BNE (rs1, rs2, l))
let blt rs1 rs2 label = with_label label (fun l -> BLT (rs1, rs2, l))
let bge rs1 rs2 label = with_label label (fun l -> BGE (rs1, rs2, l))
let bltu rs1 rs2 label = with_label label (fun l -> BLTU (rs1, rs2, l))
let bgeu rs1 rs2 label = with_label label (fun l -> BGEU (rs1, rs2, l))
let mul rd rs1 rs2 = ok (MUL (rd, rs1, rs2))
let mulh rd rs1 rs2 = ok (MULH (rd, rs1, rs2))
let mulhsu rd rs1 rs2 = ok (MULHSU (rd, rs1, rs2))
let mulhu rd rs1 rs2 = ok (MULHU (rd, rs1, rs2))
let div rd rs1 rs2 = ok (DIV (rd, rs1, rs2))
let divu rd rs1 rs2 = ok (DIVU (rd, rs1, rs2))
let rem rd rs1 rs2 = ok (REM (rd, rs1, rs2))
let remu rd rs1 rs2 = ok (REMU (rd, rs1, rs2))
let addiw rd rs imm = imm12 "ADDIW" (fun n -> ADDIW (rd, rs, n)) imm
let addw rd rs1 rs2 = ok (ADDW (rd, rs1, rs2))
let subw rd rs1 rs2 = ok (SUBW (rd, rs1, rs2))
let sllw rd rs1 rs2 = ok (SLLW (rd, rs1, rs2))
let srlw rd rs1 rs2 = ok (SRLW (rd, rs1, rs2))
let sraw rd rs1 rs2 = ok (SRAW (rd, rs1, rs2))
let ecall = ok ECALL
let call sym = ok (CALL sym)
let mv rd rs = ok (MV (rd, rs))
let li rd imm = ok (LI (rd, imm))
let nop = ok NOP
let ret = ok RET
