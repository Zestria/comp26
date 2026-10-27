[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Constants shared by code generation and runtime integration.
    The full contract is documented in [ABI.md]. Sizes and offsets are bytes.
    Register names are assembly spellings, independent of the future [Asm] type. *)

val target_isa : string
val target_abi : string
val word_size : int
val stack_alignment : int

(** Argument registers in source argument order. Further arguments use the stack. *)
val argument_registers : string list
val register_argument_count : int
val result_register : string

(** Temporaries available to the emitter; calls may overwrite them. *)
val temporary_registers : string list

(** Registers that a call may overwrite, including the return address. *)
val caller_saved_registers : string list

(** Registers a function must restore if it modifies them. [sp] is separate
    and must also be restored before returning. *)
val callee_saved_registers : string list
val stack_pointer : string
val return_address_register : string

(** Untagged 64-bit representations of source literals. *)
val false_value : int64
val true_value : int64
val unit_value : int64

(** Layout of a closure: three words followed by saved argument words.
    Argument [i] is at [args_offset + i * word_size]. *)
module Closure : sig
  val code_offset : int
  val arity_offset : int
  val argc_offset : int
  val args_offset : int
  val header_size : int
end

(** External symbols declared in [runtime/runtime.h]. *)
module Runtime : sig
  val make_closure : string
  val apply : string
  val print_int : string
  val print_int_arity : int
  val division_by_zero : string
end
