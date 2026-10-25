[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Riscv

let ( let* ) = Result.bind

let adjust_sp delta =
  if delta = 0
  then []
  else if fits_imm12 delta
  then [ I (Addi, Sp, Sp, delta) ]
  else [ Li (T 1, delta); R (Add, Sp, Sp, T 1) ]
;;

let store_argument offset =
  if fits_imm12 offset
  then [ Sd (T 0, offset, Sp) ]
  else [ Li (T 1, offset); R (Add, T 1, Sp, T 1); Sd (T 0, 0, T 1) ]
;;

let call ~load f args =
  let argc = List.length args in
  if argc > (max_int - 15) / Closure.word_bytes
  then Error "too many arguments for a closure call"
  else (
    let bytes = ((argc * Closure.word_bytes) + 15) / 16 * 16 in
    let rec arguments offset chunks = function
      | [] -> Ok (List.concat (List.rev chunks))
      | arg :: rest ->
        let* code = load (T 0) arg in
        arguments
          (offset + Closure.word_bytes)
          ((code @ store_argument offset) :: chunks)
          rest
    in
    let* arguments = arguments 0 [] args in
    (* All arguments are saved before a0-a2 are filled for the C runtime. *)
    let* closure = load (A 0) f in
    Ok
      (adjust_sp (-bytes)
       @ arguments
       @ closure
       @ [ Li (A 1, argc); Mv (A 2, Sp); Call "shmel_apply" ]
       @ adjust_sp bytes))
;;

let hooks ~load_global = { Codegen.load_global; call }
