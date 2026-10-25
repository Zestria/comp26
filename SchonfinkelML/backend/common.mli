[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type reg =
  | Zero (** zero *)
  | RA (** ra *)
  | SP (** sp *)
  | A0 (** a0 *)
  | A1 (** a1 *)
  | A2 (** a2 *)
  | A3 (** a3 *)
  | A4 (** a4 *)
  | A5 (** a5 *)
  | A6 (** a6 *)
  | A7 (** a7 *)
  | T0 (** t0 *)
  | T1 (** t1 *)
  | T2 (** t2 *)
  | T3 (** t3 *)
  | T4 (** t4 *)
  | T5 (** t5 *)
  | T6 (** t6 *)
  | S0 (** s0 *)
  | S1 (** s1 *)
  | S2 (** s2 *)
  | S3 (** s3 *)
  | S4 (** s4 *)
  | S5 (** s5 *)
  | S6 (** s6 *)
  | S7 (** s7 *)
  | S8 (** s8 *)
  | S9 (** s9 *)
  | S10 (** s10 *)
  | S11 (** s11 *)

type instr =
  | LB of reg * reg * int (** lb *)
  | LH of reg * reg * int (** lh *)
  | LW of reg * reg * int (** lw *)
  | LD of reg * reg * int (** ld *)
  | LBU of reg * reg * int (** lbu *)
  | LHU of reg * reg * int (** lhu *)
  | LWU of reg * reg * int (** lwu *)
  | SB of reg * reg * int (** sb *)
  | SH of reg * reg * int (** sh *)
  | SW of reg * reg * int (** sw *)
  | SD of reg * reg * int (** sd *)
  | ADDI of reg * reg * int (** addi *)
  | SLTI of reg * reg * int (** slti *)
  | SLTIU of reg * reg * int (** sltiu *)
  | XORI of reg * reg * int (** xori *)
  | ORI of reg * reg * int (** ori *)
  | ANDI of reg * reg * int (** andi *)
  | SLLI of reg * reg * int (** slli *)
  | SRLI of reg * reg * int (** srli *)
  | SRAI of reg * reg * int (** srai *)
  | ADD of reg * reg * reg (** add *)
  | SUB of reg * reg * reg (** sub *)
  | SLL of reg * reg * reg (** sll *)
  | SLT of reg * reg * reg (** slt *)
  | SLTU of reg * reg * reg (** sltu *)
  | XOR of reg * reg * reg (** xor *)
  | SRL of reg * reg * reg (** srl *)
  | SRA of reg * reg * reg (** sra *)
  | OR of reg * reg * reg (** or *)
  | AND of reg * reg * reg (** and *)
  | LUI of reg * int (** lui *)
  | AUIPC of reg * int (** auipc *)
  | JAL of reg * string (** jal *)
  | JALR of reg * reg * int (** jalr *)
  | BEQ of reg * reg * string (** beq *)
  | BNE of reg * reg * string (** bne *)
  | BLT of reg * reg * string (** blt *)
  | BGE of reg * reg * string (** bge *)
  | BLTU of reg * reg * string (** bltu *)
  | BGEU of reg * reg * string (** bgeu *)
  | MUL of reg * reg * reg (** mul *)
  | MULH of reg * reg * reg (** mulh *)
  | MULHSU of reg * reg * reg (** mulhsu *)
  | MULHU of reg * reg * reg (** mulhu *)
  | DIV of reg * reg * reg (** div *)
  | DIVU of reg * reg * reg (** divu *)
  | REM of reg * reg * reg (** rem *)
  | REMU of reg * reg * reg (** remu *)
  | ADDIW of reg * reg * int (** addiw *)
  | ADDW of reg * reg * reg (** addw *)
  | SUBW of reg * reg * reg (** subw *)
  | SLLW of reg * reg * reg (** sllw *)
  | SRLW of reg * reg * reg (** srlw *)
  | SRAW of reg * reg * reg (** sraw *)
  | ECALL (** ecall *)
  | MV of reg * reg (** mv *)
  | LI of reg * int (** li *)
  | NOP (** nop *)
  | RET (** ret *)

type error =
  | Invalid_immediate of
      { instruction : string
      ; value : int
      ; min : int
      ; max : int
      }
  | Invalid_label of string
  | Unsupported of string

val pp_error : Format.formatter -> error -> unit

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
val ecall : (instr, 'a) result
val mv : reg -> reg -> (instr, 'a) result
val li : reg -> int -> (instr, 'a) result
val nop : (instr, 'a) result
val ret : (instr, 'a) result
