[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Calling convention of the MyLittleML RISC-V backend (Task 3).

    A deliberately minimal, self-contained ABI:

    - arguments 1..{!max_args} travel in [a0]..[a5]; the return value is
      in [a0];
    - calls are [jal label] / [jalr]; [ra] and [s0] are saved by the callee
      in its own frame;
    - every function opens a frame with one [addi sp, sp, -frame]: [ra]
      lives at [0(sp)], [s0] at [8(sp)], [let]-bound locals start at
      [16(sp)] and expression spill slots live above them;
    - first-class function values are closure records [code; v1 … vk]; an
      indirect call puts the record pointer in [s0], the remaining
      arguments in [a0]... and jumps through [t0] (see {!Codegen});
    - [print_int] is not a C function: the backend emits a small assembly
      helper that formats the number on the stack and issues the [write]
      syscall directly, so linking needs no extra runtime object. *)

let max_args = 6

(** Register holding argument [i] (0-based).  @raise Invalid_arg beyond
    {!max_args}. *)
let arg_reg i =
  if i < 0 || i >= max_args
  then invalid_arg (Format.sprintf "Callconv.arg_reg: %d beyond max_args %d" i max_args);
  match i with
  | 0 -> Program.Reg.A0
  | 1 -> Program.Reg.A1
  | 2 -> Program.Reg.A2
  | 3 -> Program.Reg.A3
  | 4 -> Program.Reg.A4
  | 5 -> Program.Reg.A5
  | _ -> assert false
;;

let return_reg () = Program.Reg.A0

(** Offset of local slot [i] (0-based) from [sp] inside an open frame.
    [ra] and [s0] occupy [0(sp)] and [8(sp)]. *)
let local_offset i = 16 + (8 * i)

(** Offset of spill slot [d] (0-based) from [sp] inside an open frame,
    assuming the function uses [nlocals] local slots. *)
let spill_offset ~nlocals d = 16 + (8 * (nlocals + d))

(** Total frame size for [nlocals] local slots and [max_spill] spill slots:
    [ra] + [s0] + slots, rounded up to the 16-byte ABI stack alignment. *)
let frame_size ~nlocals ~max_spill =
  let raw = 16 + (8 * (nlocals + max_spill)) in
  (raw + 15) / 16 * 16
;;

(** Label of the [k]-capture trampoline for the static function [f]. *)
let trampoline_label f k = f ^ "_clo" ^ string_of_int k

(** Label of the static (zero-capture) closure record for [f]. *)
let static_closure_label f = f ^ "_clo0"

(** Byte size of a closure record with [k] captured arguments. *)
let closure_size k = 8 * (k + 1)
