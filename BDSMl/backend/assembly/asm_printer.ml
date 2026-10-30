[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Asm

let register_name = function
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | Gp -> "gp"
  | Tp -> "tp"
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
  | A0 -> "a0"
  | A1 -> "a1"
  | A2 -> "a2"
  | A3 -> "a3"
  | A4 -> "a4"
  | A5 -> "a5"
  | A6 -> "a6"
  | A7 -> "a7"
;;

let binary_name = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Xor -> "xor"
  | Slt -> "slt"
  | And -> "and"
  | Or -> "or"
;;

let unary_name = function
  | Neg -> "neg"
  | Seqz -> "seqz"
  | Snez -> "snez"
;;

let r = register_name

let print_instruction = function
  | Li (rd, value) -> Printf.sprintf "li %s, %Ld" (r rd) value
  | La (rd, symbol) -> Printf.sprintf "la %s, %s" (r rd) symbol
  | Mv (rd, rs) -> Printf.sprintf "mv %s, %s" (r rd) (r rs)
  | Binary (op, rd, left, right) ->
    Printf.sprintf "%s %s, %s, %s" (binary_name op) (r rd) (r left) (r right)
  | Unary (op, rd, rs) -> Printf.sprintf "%s %s, %s" (unary_name op) (r rd) (r rs)
  | Addi (rd, rs, n) -> Printf.sprintf "addi %s, %s, %d" (r rd) (r rs) n
  | Xori (rd, rs, n) -> Printf.sprintf "xori %s, %s, %d" (r rd) (r rs) n
  | Ld (rd, offset, base) -> Printf.sprintf "ld %s, %d(%s)" (r rd) offset (r base)
  | Sd (rs, offset, base) -> Printf.sprintf "sd %s, %d(%s)" (r rs) offset (r base)
  | Beqz (rs, label) -> Printf.sprintf "beqz %s, %s" (r rs) label
  | Bnez (rs, label) -> Printf.sprintf "bnez %s, %s" (r rs) label
  | Jump label -> "j " ^ label
  | Call symbol -> "call " ^ symbol
  | Ret -> "ret"
;;

let print_directive = function
  | Text -> ".text"
  | Bss -> ".bss"
  | Global symbol -> ".globl " ^ symbol
  | Align power -> Printf.sprintf ".p2align %d" power
  | Function symbol -> Printf.sprintf ".type %s, @function" symbol
  | End_function symbol -> Printf.sprintf ".size %s, .-%s" symbol symbol
  | Zero_bytes size -> Printf.sprintf ".zero %d" size
  | Non_executable_stack -> ".section .note.GNU-stack,\"\",@progbits"
;;

let print program =
  let line = function
    | Instruction op -> "\t" ^ print_instruction op
    | Directive directive -> "\t" ^ print_directive directive
    | Label label -> label ^ ":"
  in
  String.concat "\n" (List.map line program) ^ "\n"
;;
