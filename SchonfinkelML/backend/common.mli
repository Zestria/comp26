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
  | LB of reg * reg * int
  | LH of reg * reg * int
  | LW of reg * reg * int
  | LD of reg * reg * int
  | LBU of reg * reg * int
  | LHU of reg * reg * int
  | LWU of reg * reg * int
  | SB of reg * reg * int
  | SH of reg * reg * int
  | SW of reg * reg * int
  | SD of reg * reg * int
  | ADDI of reg * reg * int
  | SLTI of reg * reg * int
  | SLTIU of reg * reg * int
  | XORI of reg * reg * int
  | ORI of reg * reg * int
  | ANDI of reg * reg * int
  | SLLI of reg * reg * int
  | SRLI of reg * reg * int
  | SRAI of reg * reg * int
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
  | LUI of reg * int
  | AUIPC of reg * int
  | JAL of reg * string
  | JALR of reg * reg * int
  | BEQ of reg * reg * string
  | BNE of reg * reg * string
  | BLT of reg * reg * string
  | BGE of reg * reg * string
  | BLTU of reg * reg * string
  | BGEU of reg * reg * string
  | MUL of reg * reg * reg
  | MULH of reg * reg * reg
  | MULHSU of reg * reg * reg
  | MULHU of reg * reg * reg
  | DIV of reg * reg * reg
  | DIVU of reg * reg * reg
  | REM of reg * reg * reg
  | REMU of reg * reg * reg
  | ADDIW of reg * reg * int
  | ADDW of reg * reg * reg
  | SUBW of reg * reg * reg
  | SLLW of reg * reg * reg
  | SRLW of reg * reg * reg
  | SRAW of reg * reg * reg
  | MV of reg * reg
  | LI of reg * int
  | NOP
  | RET

type error =
  | Invalid_immediate of
      { instruction : string
      ; value : int
      ; min : int
      ; max : int
      }
  | Invalid_label of string

type 'a checked = ('a, error) result

val ok : 'a -> ('a, 'b) result
val check_range : string -> int -> int -> int -> (int -> 'a) -> ('a, error) result
val imm12 : string -> (int -> 'a) -> int -> ('a, error) result
val imm20 : string -> (int -> 'a) -> int -> ('a, error) result
val shamt64 : string -> (int -> 'a) -> int -> ('a, error) result
val with_label : string -> (string -> 'a) -> ('a, error) result
val lb : reg -> reg -> int -> (instr, error) result
val lh : reg -> reg -> int -> (instr, error) result
val lw : reg -> reg -> int -> (instr, error) result
val ld : reg -> reg -> int -> (instr, error) result
val lbu : reg -> reg -> int -> (instr, error) result
val lhu : reg -> reg -> int -> (instr, error) result
val lwu : reg -> reg -> int -> (instr, error) result
val sb : reg -> reg -> int -> (instr, error) result
val sh : reg -> reg -> int -> (instr, error) result
val sw : reg -> reg -> int -> (instr, error) result
val sd : reg -> reg -> int -> (instr, error) result
val addi : reg -> reg -> int -> (instr, error) result
val slti : reg -> reg -> int -> (instr, error) result
val sltiu : reg -> reg -> int -> (instr, error) result
val xori : reg -> reg -> int -> (instr, error) result
val ori : reg -> reg -> int -> (instr, error) result
val andi : reg -> reg -> int -> (instr, error) result
val slli : reg -> reg -> int -> (instr, error) result
val srli : reg -> reg -> int -> (instr, error) result
val srai : reg -> reg -> int -> (instr, error) result
val add : reg -> reg -> reg -> (instr, 'a) result
val sub : reg -> reg -> reg -> (instr, 'a) result
val sll : reg -> reg -> reg -> (instr, 'a) result
val slt : reg -> reg -> reg -> (instr, 'a) result
val sltu : reg -> reg -> reg -> (instr, 'a) result
val xor : reg -> reg -> reg -> (instr, 'a) result
val srl : reg -> reg -> reg -> (instr, 'a) result
val sra : reg -> reg -> reg -> (instr, 'a) result
val or_ : reg -> reg -> reg -> (instr, 'a) result
val and_ : reg -> reg -> reg -> (instr, 'a) result
val lui : reg -> int -> (instr, error) result
val auipc : reg -> int -> (instr, error) result
val jal : reg -> string -> (instr, error) result
val jalr : reg -> reg -> int -> (instr, error) result
val beq : reg -> reg -> string -> (instr, error) result
val bne : reg -> reg -> string -> (instr, error) result
val blt : reg -> reg -> string -> (instr, error) result
val bge : reg -> reg -> string -> (instr, error) result
val bltu : reg -> reg -> string -> (instr, error) result
val bgeu : reg -> reg -> string -> (instr, error) result
val mul : reg -> reg -> reg -> (instr, 'a) result
val mulh : reg -> reg -> reg -> (instr, 'a) result
val mulhsu : reg -> reg -> reg -> (instr, 'a) result
val mulhu : reg -> reg -> reg -> (instr, 'a) result
val div : reg -> reg -> reg -> (instr, 'a) result
val divu : reg -> reg -> reg -> (instr, 'a) result
val rem : reg -> reg -> reg -> (instr, 'a) result
val remu : reg -> reg -> reg -> (instr, 'a) result
val addiw : reg -> reg -> int -> (instr, error) result
val addw : reg -> reg -> reg -> (instr, 'a) result
val subw : reg -> reg -> reg -> (instr, 'a) result
val sllw : reg -> reg -> reg -> (instr, 'a) result
val srlw : reg -> reg -> reg -> (instr, 'a) result
val sraw : reg -> reg -> reg -> (instr, 'a) result
val mv : reg -> reg -> (instr, 'a) result
val li : reg -> int -> (instr, 'a) result
val nop : (instr, 'a) result
val ret : (instr, 'a) result
