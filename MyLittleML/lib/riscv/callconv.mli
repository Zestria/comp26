[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Calling convention of the MyLittleML RISC-V backend (Task 3). *)

val max_args : int
val arg_reg : int -> (Program.Reg.t, string) result
val return_reg : unit -> Program.Reg.t
val local_offset : int -> int
val spill_offset : nlocals:int -> int -> int
val frame_size : nlocals:int -> max_spill:int -> int
val static_closure_label : string -> string
val closure_size : int -> int
