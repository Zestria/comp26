[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type reg =
  | Zero
  | Ra
  | Sp
  | Fp
  | S of int
  | A of int
  | T of int

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Slt
  | Sltu
  | Xor
  | And
  | Or

type imm_op =
  | Addi
  | Xori
  | Andi
  | Ori
  | Slti
  | Slli
  | Srai

type branch =
  | Beq
  | Bne
  | Blt
  | Bge

type instr =
  | Li of reg * int64
  | La of reg * string
  | Mv of reg * reg
  | Binop of binop * reg * reg * reg
  | Imm of imm_op * reg * reg * int
  | Seqz of reg * reg
  | Snez of reg * reg
  | Ld of reg * int * reg
  | Sd of reg * int * reg
  | Call of string
  | Jalr of reg
  | J of string
  | Branch of branch * reg * reg * string
  | Beqz of reg * string
  | Bnez of reg * string
  | Label of string
  | Ret
  | Directive of string
  | Comment of string

(* [t6], the legalization scratch register: never allocated and never an
   operand of instruction selection. *)
let scratch = T 6
let fits_imm12 n = n >= -2048 && n <= 2047

let check_operand fn = function
  | T 6 -> raise (Invalid_argument (fn ^ ": t6 is the legalization scratch register"))
  | _ -> ()
;;

let add_imm rd rs n =
  check_operand "add_imm" rd;
  check_operand "add_imm" rs;
  if fits_imm12 n
  then [ Imm (Addi, rd, rs, n) ]
  else [ Li (scratch, Int64.of_int n); Binop (Add, rd, rs, scratch) ]
;;

let load rd off base =
  check_operand "load" rd;
  check_operand "load" base;
  if fits_imm12 off
  then [ Ld (rd, off, base) ]
  else
    [ Li (scratch, Int64.of_int off)
    ; Binop (Add, scratch, base, scratch)
    ; Ld (rd, 0, scratch)
    ]
;;

let store src off base =
  check_operand "store" src;
  check_operand "store" base;
  if fits_imm12 off
  then [ Sd (src, off, base) ]
  else
    [ Li (scratch, Int64.of_int off)
    ; Binop (Add, scratch, base, scratch)
    ; Sd (src, 0, scratch)
    ]
;;

let reg_to_string = function
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | Fp -> "s0"
  | S i -> "s" ^ string_of_int i
  | A i -> "a" ^ string_of_int i
  | T i -> "t" ^ string_of_int i
;;

let binop_to_string = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Rem -> "rem"
  | Slt -> "slt"
  | Sltu -> "sltu"
  | Xor -> "xor"
  | And -> "and"
  | Or -> "or"
;;

let imm_op_to_string = function
  | Addi -> "addi"
  | Xori -> "xori"
  | Andi -> "andi"
  | Ori -> "ori"
  | Slti -> "slti"
  | Slli -> "slli"
  | Srai -> "srai"
;;

let branch_to_string = function
  | Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Bge -> "bge"
;;

let line = function
  | Li (rd, n) -> "\tli\t" ^ reg_to_string rd ^ ", " ^ Int64.to_string n
  | La (rd, sym) -> "\tla\t" ^ reg_to_string rd ^ ", " ^ sym
  | Mv (rd, rs) -> "\tmv\t" ^ reg_to_string rd ^ ", " ^ reg_to_string rs
  | Binop (op, rd, rs1, rs2) ->
    "\t"
    ^ binop_to_string op
    ^ "\t"
    ^ reg_to_string rd
    ^ ", "
    ^ reg_to_string rs1
    ^ ", "
    ^ reg_to_string rs2
  | Imm (op, rd, rs, n) ->
    "\t"
    ^ imm_op_to_string op
    ^ "\t"
    ^ reg_to_string rd
    ^ ", "
    ^ reg_to_string rs
    ^ ", "
    ^ string_of_int n
  | Seqz (rd, rs) -> "\tseqz\t" ^ reg_to_string rd ^ ", " ^ reg_to_string rs
  | Snez (rd, rs) -> "\tsnez\t" ^ reg_to_string rd ^ ", " ^ reg_to_string rs
  | Ld (rd, off, base) ->
    "\tld\t"
    ^ reg_to_string rd
    ^ ", "
    ^ string_of_int off
    ^ "("
    ^ reg_to_string base
    ^ ")"
  | Sd (rd, off, base) ->
    "\tsd\t"
    ^ reg_to_string rd
    ^ ", "
    ^ string_of_int off
    ^ "("
    ^ reg_to_string base
    ^ ")"
  | Call sym -> "\tcall\t" ^ sym
  | Jalr r -> "\tjalr\tra, 0(" ^ reg_to_string r ^ ")"
  | Label l -> l ^ ":"
  | Ret -> "\tret"
  | Directive d -> "\t" ^ d
  | Comment c -> "\t# " ^ c
  | J l -> "\tj\t" ^ l
  | Branch (op, rs1, rs2, l) ->
    "\t"
    ^ branch_to_string op
    ^ "\t"
    ^ reg_to_string rs1
    ^ ", "
    ^ reg_to_string rs2
    ^ ", "
    ^ l
  | Beqz (r, l) -> "\tbeqz\t" ^ reg_to_string r ^ ", " ^ l
  | Bnez (r, l) -> "\tbnez\t" ^ reg_to_string r ^ ", " ^ l
;;

let to_string = function
  | [] -> ""
  | instrs -> String.concat "\n" (List.map line instrs) ^ "\n"
;;
