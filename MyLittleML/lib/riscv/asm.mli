[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V 64 assembly representation and emission (Task 3). *)

type instr =
  | Li of Program.Reg.t * int
  | Mov of Program.Reg.t * Program.Reg.t
  | Neg of Program.Reg.t * Program.Reg.t
  | Add of Program.Reg.t * Program.Reg.t * Program.Reg.t
  | Addi of Program.Reg.t * Program.Reg.t * int
  | Sub of Program.Reg.t * Program.Reg.t * Program.Reg.t
  | Mul of Program.Reg.t * Program.Reg.t * Program.Reg.t
  | Div of Program.Reg.t * Program.Reg.t * Program.Reg.t
  | Rem of Program.Reg.t * Program.Reg.t * Program.Reg.t
  | Slt of Program.Reg.t * Program.Reg.t * Program.Reg.t
  | Seqz of Program.Reg.t * Program.Reg.t
  | Xori of Program.Reg.t * Program.Reg.t * int
  | Beq of Program.Reg.t * Program.Reg.t * string
  | Bne of Program.Reg.t * Program.Reg.t * string
  | Blt of Program.Reg.t * Program.Reg.t * string
  | Bge of Program.Reg.t * Program.Reg.t * string
  | J of string
  | Jal of string
  | Call of string
  | Jalr of Program.Reg.t
  | La of Program.Reg.t * string
  | Ret
  | Ld of Program.Reg.t * int * Program.Reg.t
  | Sd of Program.Reg.t * int * Program.Reg.t
  | Sb of Program.Reg.t * int * Program.Reg.t
  | Ecall
  | Label of string

type t =
  { text : instr list
  ; data : (string * string) list
  }

val instr_to_string : instr -> string
val emit : t -> string
