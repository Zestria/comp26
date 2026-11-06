[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Native closure ABI: one argument in a0, result in a0, environment
    pointer in s0. A function saves ra and the entry s0 in a 16-byte-aligned
    stack frame. The saved pointer is stable across nested indirect calls.
    The lower-level IL also supports direct calls with up to six registers.
    See README-riscv.md for the full contract. *)

let max_args = 6

(** Register holding argument [i] (0-based), or a descriptive error. *)
let arg_reg = function
  | 0 -> Ok Program.Reg.A0
  | 1 -> Ok Program.Reg.A1
  | 2 -> Ok Program.Reg.A2
  | 3 -> Ok Program.Reg.A3
  | 4 -> Ok Program.Reg.A4
  | 5 -> Ok Program.Reg.A5
  | i -> Error (Printf.sprintf "argument register index %d is out of range" i)
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

(** Label of the static (zero-capture) closure record for [f]. *)
let static_closure_label f = f ^ "_clo0"

(** Byte size of a closure record with [k] captured arguments. *)
let closure_size k = 8 * (k + 1)
