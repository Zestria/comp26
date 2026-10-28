[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Программа на ассемблере RISC-V RV64 и её печать в синтаксисе GNU as.

    Набор инструкций --- только то, что нужно нашему языку; кодоген (#21)
    добавляет остальное сам. Контракт с рантаймом --- ADR 0003. *)

(** Регистры по ABI-именам. *)
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

(** Метка: глобальный символ или локальная [.L…]. *)
type label = private string

(** Метка с заданным именем, например [label "vc_main"]. *)
val label : string -> label

(** Свежая локальная метка [.L…] через [Fresh]: уникальна в программе. *)
val fresh_label : unit -> label

type directive =
  | Text
  | Data
  | Globl of label
  | Dword of int
  | Asciz of string

type instr =
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Rem of reg * reg * reg
  | Addi of reg * reg * int (** иммедиат в 12 битах *)
  | Slt of reg * reg * reg
  | Sltu of reg * reg * reg
  | Xor of reg * reg * reg
  | Seqz of reg * reg
  | Snez of reg * reg
  | Ld of reg * int * reg (** [Ld (rd, offset, base)] --- [ld rd, offset(base)] *)
  | Sd of reg * int * reg (** [Sd (rs, offset, base)] --- [sd rs, offset(base)] *)
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

(** Элемент ассемблера. Комментарий к инструкции печатается после [#]. *)
type item =
  | Label of label
  | Directive of directive
  | Instr of instr * string option

type program = item list

val pp_reg : Format.formatter -> reg -> unit

(** Падает на [assert], если иммедиат [addi], [ld] или [sd] не влезает в 12 бит. *)
val pp_item : Format.formatter -> item -> unit

val pp_program : Format.formatter -> program -> unit

(** Peephole в окне из двух инструкций; метка и директива --- барьер.
    Комментарий удалённой инструкции теряется. *)
val peephole : program -> program
