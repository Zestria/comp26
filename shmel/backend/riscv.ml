[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type reg =
  | Zero
  | Ra
  | Sp
  | Fp
  | A of int
  | T of int
  | S of int

type label = string

type rop =
  | Add
  | Sub
  | Mul
  | Div
  | Slt
  | Xor

type iop =
  | Addi
  | Slti
  | Xori

type bcond =
  | Beq
  | Bne
  | Blt
  | Bge
  | Bgt
  | Ble

type instr =
  | R of rop * reg * reg * reg
  | I of iop * reg * reg * int
  | Li of reg * int
  | La of reg * label
  | Mv of reg * reg
  | Neg of reg * reg
  | Seqz of reg * reg
  | Ld of reg * int * reg
  | Sd of reg * int * reg
  | Branch of bcond * reg * reg * label
  | J of label
  | Call of label
  | Jalr of reg
  | Ret
  | Label of label
  | Directive of string
  | Comment of string

let fits_imm12 n = n >= -2048 && n <= 2047

let pp_reg ppf = function
  | Zero -> Format.pp_print_string ppf "zero"
  | Ra -> Format.pp_print_string ppf "ra"
  | Sp -> Format.pp_print_string ppf "sp"
  | Fp -> Format.pp_print_string ppf "s0"
  | A n -> Format.fprintf ppf "a%d" n
  | T n -> Format.fprintf ppf "t%d" n
  | S n -> Format.fprintf ppf "s%d" n
;;

let rop_name = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Slt -> "slt"
  | Xor -> "xor"
;;

let iop_name = function
  | Addi -> "addi"
  | Slti -> "slti"
  | Xori -> "xori"
;;

let bcond_name = function
  | Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Bge -> "bge"
  | Bgt -> "bgt"
  | Ble -> "ble"
;;

let pp_instr ppf = function
  | R (op, rd, rs1, rs2) ->
    Format.fprintf ppf "  %s %a, %a, %a" (rop_name op) pp_reg rd pp_reg rs1 pp_reg rs2
  | I (op, rd, rs, imm) ->
    Format.fprintf ppf "  %s %a, %a, %d" (iop_name op) pp_reg rd pp_reg rs imm
  | Li (rd, n) -> Format.fprintf ppf "  li %a, %d" pp_reg rd n
  | La (rd, l) -> Format.fprintf ppf "  la %a, %s" pp_reg rd l
  | Mv (rd, rs) -> Format.fprintf ppf "  mv %a, %a" pp_reg rd pp_reg rs
  | Neg (rd, rs) -> Format.fprintf ppf "  neg %a, %a" pp_reg rd pp_reg rs
  | Seqz (rd, rs) -> Format.fprintf ppf "  seqz %a, %a" pp_reg rd pp_reg rs
  | Ld (rd, off, base) -> Format.fprintf ppf "  ld %a, %d(%a)" pp_reg rd off pp_reg base
  | Sd (rs, off, base) -> Format.fprintf ppf "  sd %a, %d(%a)" pp_reg rs off pp_reg base
  | Branch (Beq, rs, Zero, l) -> Format.fprintf ppf "  beqz %a, %s" pp_reg rs l
  | Branch (Bne, rs, Zero, l) -> Format.fprintf ppf "  bnez %a, %s" pp_reg rs l
  | Branch (c, rs1, rs2, l) ->
    Format.fprintf ppf "  %s %a, %a, %s" (bcond_name c) pp_reg rs1 pp_reg rs2 l
  | J l -> Format.fprintf ppf "  j %s" l
  | Call l -> Format.fprintf ppf "  call %s" l
  | Jalr rs -> Format.fprintf ppf "  jalr %a" pp_reg rs
  | Ret -> Format.pp_print_string ppf "  ret"
  | Label l -> Format.fprintf ppf "%s:" l
  | Directive d -> Format.pp_print_string ppf d
  | Comment c -> Format.fprintf ppf "  # %s" c
;;

let pp_instrs ppf instrs =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '\n')
    pp_instr
    ppf
    instrs
;;
