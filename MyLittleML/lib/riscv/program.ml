[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Explicit-closure IL and RV64 register names. Values are untagged 64-bit
    integers or closure pointers. Arithmetic is emitted inline. Register t6
    is reserved for expansion of stack offsets and immediates beyond 12 bits. *)
module Reg = struct
  type t =
    | Zero
    | Ra
    | Sp
    | S0
    | A0
    | A1
    | A2
    | A3
    | A4
    | A5
    | A7
    | T0
    | T1
    | T2
    | T3
    | T4
    | T5
    | T6

  let name = function
    | Zero -> "zero"
    | Ra -> "ra"
    | Sp -> "sp"
    | S0 -> "s0"
    | A0 -> "a0"
    | A1 -> "a1"
    | A2 -> "a2"
    | A3 -> "a3"
    | A4 -> "a4"
    | A5 -> "a5"
    | A7 -> "a7"
    | T0 -> "t0"
    | T1 -> "t1"
    | T2 -> "t2"
    | T3 -> "t3"
    | T4 -> "t4"
    | T5 -> "t5"
    | T6 -> "t6"
  ;;
end

(** Primitive operations lowered inline to RISC-V instructions.  [Div] and
    [Mod] use the signed M-extension instructions [div]/[rem], whose
    rounding-toward-zero semantics coincide with OCaml's [/] and [mod]. *)
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

(** [MakeClosure (label, values)] allocates [code; captures...]. [Env i]
    loads capture i from the entry environment, and [Self] denotes that
    environment itself, allowing recursion through the entry environment.
    [PrintInt] prints a signed integer with no trailing newline. *)
type body =
  | Env of int
  | Self
  | MakeClosure of string * body list
  | Const of int
  | Var of string
  | BinOp of binop * body * body
  | If of body * body * body
  | App of callee * body list
  | Let of string * body * body
  | PrintInt of body

(** A call target: either a statically known function label (zero captured
    arguments), or a first-class function value (a closure record). *)
and callee =
  | Direct of string
  | Value of body

(** A machine function: its label, its parameter names (in the order they
    occupy the argument registers) and its body. *)
type function_ =
  { name : string
  ; params : string list
  ; body : body
  }

(** A complete user program: a list of function definitions, one of which
    must be named ["main"] (the entry point). *)
type program = function_ list
