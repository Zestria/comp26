[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let target_isa = "rv64gc"
let target_abi = "lp64d"
let word_size = 8
let stack_alignment = 16
let argument_registers = [ "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" ]
let register_argument_count = List.length argument_registers
let result_register = "a0"
let temporary_registers = [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6" ]
let caller_saved_registers = "ra" :: (argument_registers @ temporary_registers)
let callee_saved_registers =
  [ "s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11" ]
;;

let stack_pointer = "sp"
let return_address_register = "ra"
let false_value = 0L
let true_value = 1L
let unit_value = 0L

module Closure = struct
  let code_offset = 0
  let arity_offset = word_size
  let argc_offset = 2 * word_size
  let args_offset = 3 * word_size
  let header_size = args_offset
end

module Runtime = struct
  let make_closure = "bd_make_closure"
  let apply = "bd_apply"
  let print_int = "bd_print_int"
  let print_int_arity = 1
  let division_by_zero = "bd_division_by_zero"
end
