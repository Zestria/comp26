[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t =
  { code : Riscv.label
  ; arity : int
  }

let create ~code ~arity =
  if String.equal code ""
  then Error "closure code label must not be empty"
  else if arity <= 0
  then Error "closure arity must be positive"
  else Ok { code; arity }
;;

let code t = t.code
let arity t = t.arity
let word_bytes = 8
let code_offset = 0
let arity_offset = 8
let applied_offset = 16
let arguments_offset = 24

let argument_offset index =
  if index < 0
  then Error "closure argument index must be non-negative"
  else if index > (max_int - arguments_offset) / word_bytes
  then Error "closure layout size overflow"
  else Ok (arguments_offset + (word_bytes * index))
;;

let size_bytes ~applied = argument_offset applied

let remaining_arity t ~applied =
  if applied < 0 || applied >= t.arity
  then Error "closure must have fewer applied arguments than its arity"
  else Ok (t.arity - applied)
;;
