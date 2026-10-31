[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let word = 8

(** Byte offsets from a heap value. *)
let tuple_field i = word * (i + 1)

let closure_code = 8
let closure_arity = 16
let closure_env j = 24 + (word * j)

(** Header words. *)
let tuple_header n = Int64.shift_left (Int64.of_int n) 1

let closure_header = 1L

(** Tagged immediates. *)
let unit_value = 1L

let bool_value b = if b then 3L else 1L

(** Runtime symbols ([runtime/runtime.c]). *)
let alloc = "rt_alloc"

let apply = "rt_apply"
let compare = "rt_compare"
let print_int = "rt_print_int"
let div_by_zero = "rt_div_by_zero"

(** Exported by generated code: evaluates the program body. *)
let entry = "ml_main"
