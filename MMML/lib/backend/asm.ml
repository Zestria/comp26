[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type register =
  | Zero
  | Ra
  | Sp
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7

let register_name = function
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | T0 -> "t0"
  | T1 -> "t1"
  | T2 -> "t2"
  | T3 -> "t3"
  | T4 -> "t4"
  | T5 -> "t5"
  | T6 -> "t6"
  | A0 -> "a0"
  | A1 -> "a1"
  | A2 -> "a2"
  | A3 -> "a3"
  | A4 -> "a4"
  | A5 -> "a5"
  | A6 -> "a6"
  | A7 -> "a7"
;;

type condition =
  | Eq
  | Ne
  | Lt

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem

type instruction =
  | Label of string
  | Li of register * int64
  | La of register * string
  | Addi of register * register * int64
  | Binop of binop * register * register * register
  | Ld of register * int64 * register
  | Sd of register * int64 * register
  | Sb of register * int64 * register
  | Branch of condition * register * register * string
  | Jump of string
  | Call of string
  | Ecall
  | Ret

type function_ =
  { name : string
  ; exported : bool
  ; body : instruction list
  }

type program = function_ list
