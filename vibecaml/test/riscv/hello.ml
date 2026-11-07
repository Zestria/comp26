[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Программа, собранная руками на Riscv-AST, пока нет кодогенерации (#21):
   vc_main печатает 42 через рантайм и возвращает управление в _start. *)

open Vibecaml
open Riscv

let () =
  let main = label "vc_main" in
  let instr i = Instr (i, None) in
  let program =
    [ Directive Text
    ; Directive (Globl main)
    ; Label main
    ; instr (Addi (Sp, Sp, -16))
    ; Instr (Sd (Ra, 8, Sp), Some "call затрёт ra")
    ; instr (Li (A0, 42))
    ; instr (Call (label "vc_print_int"))
    ; instr (Call (label "vc_print_newline"))
    ; instr (Ld (Ra, 8, Sp))
    ; instr (Addi (Sp, Sp, 16))
    ; instr (Mv (A0, A0))
    ; instr Ret
    ]
  in
  Format.printf "%a" pp_program (peephole program)
;;
