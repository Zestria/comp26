[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t

type location =
  | Register of Assembly.Asm.register
  | Stack of int
  | Unused

(** Linear-scan allocation for alpha-renamed parameters/locals. Only spilled
    values get stack slots; only allocated callee-saved registers are saved.
    The outgoing argument area and scratch slots remain separate. *)
val create
  :  BDSMl_frontend.Ast.pattern list
  -> BDSMl_middleend.Anf.Anf_ast.aexpr list
  -> t

val size : t -> int
(** [slot] is only defined for spilled bindings; use [location] in emitters. *)
val slot : t -> string -> int
val location : t -> string -> location
val saved_registers : t -> Assembly.Asm.register list
val spill_count : t -> int
val read : location -> Assembly.Asm.register -> unit Assembly.Asm.code
val write : location -> Assembly.Asm.register -> unit Assembly.Asm.code
val callee_slot : t -> int
val operand_slot : t -> int
val argument_offset : int -> int
val prologue : t -> BDSMl_frontend.Ast.pattern list -> unit Assembly.Asm.code
val epilogue : t -> unit Assembly.Asm.code
