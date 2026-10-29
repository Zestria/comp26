[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type reg =
  | Zero
  | Ra
  | Sp
  | Gp
  | Tp
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
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7

type label = string

let label name = name

(* [Fresh.name] даёт [L__N]; точка в начале делает метку локальной для as. *)
let fresh_label () = "." ^ Fresh.name "L"

type directive =
  | Text
  | Data
  | Globl of label
  | Dword of int
  | Dword_label of label
  | Align of int
  | Asciz of string

type instr =
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Rem of reg * reg * reg
  | Addi of reg * reg * int
  | Slt of reg * reg * reg
  | Sltu of reg * reg * reg
  | Xor of reg * reg * reg
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Sll of reg * reg * reg
  | Srl of reg * reg * reg
  | Sra of reg * reg * reg
  | Neg of reg * reg
  | Seqz of reg * reg
  | Snez of reg * reg
  | Ld of reg * int * reg
  | Sd of reg * int * reg
  | Beq of reg * reg * label
  | Bne of reg * reg * label
  | Blt of reg * reg * label
  | Bge of reg * reg * label
  | Ecall
  | Li of reg * int
  | La of reg * label
  | Mv of reg * reg
  | Call of label
  | Tail of label
  | Ret
  | J of label

type item =
  | Label of label
  | Directive of directive
  | Instr of instr * string option

type program = item list

let reg_name = function
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

let pp_reg ppf reg = Format.pp_print_string ppf (reg_name reg)

(* Иначе as молча не соберёт программу или соберёт не то: ловим раньше.
   Такой иммедиат --- ошибка кодогена, поэтому assert, а не result. *)
let imm12 name imm =
  let fits = -2048 <= imm && imm <= 2047 in
  if not fits
  then Printf.eprintf "riscv: иммедиат %d в %s не влезает в 12 бит\n%!" imm name;
  assert fits;
  imm
;;

let pp_instr ppf instr =
  let p fmt = Format.fprintf ppf fmt in
  let r3 name rd rs1 rs2 = p "%s %a, %a, %a" name pp_reg rd pp_reg rs1 pp_reg rs2 in
  let r2 name rd rs = p "%s %a, %a" name pp_reg rd pp_reg rs in
  let br name rs1 rs2 target = p "%s %a, %a, %s" name pp_reg rs1 pp_reg rs2 target in
  match instr with
  | Add (rd, rs1, rs2) -> r3 "add" rd rs1 rs2
  | Sub (rd, rs1, rs2) -> r3 "sub" rd rs1 rs2
  | Mul (rd, rs1, rs2) -> r3 "mul" rd rs1 rs2
  | Div (rd, rs1, rs2) -> r3 "div" rd rs1 rs2
  | Rem (rd, rs1, rs2) -> r3 "rem" rd rs1 rs2
  | Slt (rd, rs1, rs2) -> r3 "slt" rd rs1 rs2
  | Sltu (rd, rs1, rs2) -> r3 "sltu" rd rs1 rs2
  | Xor (rd, rs1, rs2) -> r3 "xor" rd rs1 rs2
  | And (rd, rs1, rs2) -> r3 "and" rd rs1 rs2
  | Or (rd, rs1, rs2) -> r3 "or" rd rs1 rs2
  | Sll (rd, rs1, rs2) -> r3 "sll" rd rs1 rs2
  | Srl (rd, rs1, rs2) -> r3 "srl" rd rs1 rs2
  | Sra (rd, rs1, rs2) -> r3 "sra" rd rs1 rs2
  | Neg (rd, rs) -> r2 "neg" rd rs
  | Addi (rd, rs, imm) -> p "addi %a, %a, %d" pp_reg rd pp_reg rs (imm12 "addi" imm)
  | Seqz (rd, rs) -> r2 "seqz" rd rs
  | Snez (rd, rs) -> r2 "snez" rd rs
  | Mv (rd, rs) -> r2 "mv" rd rs
  | Ld (rd, offset, base) -> p "ld %a, %d(%a)" pp_reg rd (imm12 "ld" offset) pp_reg base
  | Sd (rs, offset, base) -> p "sd %a, %d(%a)" pp_reg rs (imm12 "sd" offset) pp_reg base
  | Beq (rs1, rs2, target) -> br "beq" rs1 rs2 target
  | Bne (rs1, rs2, target) -> br "bne" rs1 rs2 target
  | Blt (rs1, rs2, target) -> br "blt" rs1 rs2 target
  | Bge (rs1, rs2, target) -> br "bge" rs1 rs2 target
  | Ecall -> p "ecall"
  | Li (rd, imm) -> p "li %a, %d" pp_reg rd imm
  | La (rd, target) -> p "la %a, %s" pp_reg rd target
  | Call target -> p "call %s" target
  | Tail target -> p "tail %s" target
  | Ret -> p "ret"
  | J target -> p "j %s" target
;;

(* %S не годится: OCaml пишет \ddd десятично, а as читает восьмерично. *)
let escape text =
  String.to_seq text
  |> Seq.map (function
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | c when Char.code c < 32 || Char.code c > 126 ->
      Printf.sprintf "\\%03o" (Char.code c)
    | c -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""
;;

let pp_directive ppf = function
  | Text -> Format.fprintf ppf ".text"
  | Data -> Format.fprintf ppf ".data"
  | Globl name -> Format.fprintf ppf ".globl %s" name
  | Dword value -> Format.fprintf ppf ".dword %d" value
  | Dword_label name -> Format.fprintf ppf ".dword %s" name
  | Align power -> Format.fprintf ppf ".p2align %d" power
  | Asciz text -> Format.fprintf ppf ".asciz %s" ("\"" ^ escape text ^ "\"")
;;

let pp_item ppf = function
  | Label name -> Format.fprintf ppf "%s:" name
  | Directive directive -> Format.fprintf ppf "    %a" pp_directive directive
  | Instr (instr, None) -> Format.fprintf ppf "    %a" pp_instr instr
  | Instr (instr, Some comment) ->
    Format.fprintf ppf "    %a  # %s" pp_instr instr comment
;;

let pp_program ppf program =
  List.iter (fun item -> Format.fprintf ppf "%a@\n" pp_item item) program
;;

let rec peephole = function
  | Instr ((Mv (rd, rs) | Addi (rd, rs, 0)), _) :: rest when rd = rs -> peephole rest
  (* Загрузили слот и тут же записали обратно: запись лишняя. *)
  | (Instr (Ld (rd, k, Sp), _) as load) :: Instr (Sd (rs, k', Sp), _) :: rest
    when rd = rs && rd <> Sp && k = k' -> peephole (load :: rest)
  (* Записали слот и тут же читаем: значение уже в регистре. *)
  | (Instr (Sd (rs, k, Sp), _) as store) :: Instr (Ld (rd, k', Sp), comment) :: rest
    when k = k' && rd = rs -> peephole (store :: rest)
  | (Instr (Sd (rs, k, Sp), _) as store) :: Instr (Ld (rd, k', Sp), comment) :: rest
    when k = k' -> store :: peephole (Instr (Mv (rd, rs), comment) :: rest)
  | item :: rest -> item :: peephole rest
  | [] -> []
;;
