[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Input language of the RISC-V backend (Task 3).

    This module owns the register type ([Reg.t]) used by the whole backend:
    it appears in [Asm.instr] and inside the code generator, so it is the
    single source of truth for "which register does this value live in".

    Public contract:

    - [program] is the input of {!Codegen.compile_program};
    - [function_] and [body] are the function/definition shapes;
    - [binop] is the set of primitive operations the generator lowers
      directly to instruction sequences — there are no runtime helpers for
      [+ - * / mod <=], per the Task 3 "no cringe" rule.

    First-class function values are closure records [code; v1 … vk]
    allocated with [malloc]; calling one goes through a per-[(code, k)]
    trampoline that reloads the captured arguments (see {!Codegen}). *)

(** Machine registers used by the backend.  The set is deliberately small:
    argument registers [a0]..[a5] (plus [a7] for syscall numbers), the
    standard [zero]/[ra]/[sp]/[s0] and temporaries [t0]..[t5]. *)
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

(** A function body: a tiny expression language over 64-bit integers.
    [App] passes all arguments at once; booleans are encoded as [1]/[0].
    [PrintInt] is the only side effect.

    [Closure (f, arity, vs)] builds a closure record for the static label
    [f] of total arity [arity] with the captured (already applied) leading
    arguments [vs]; [ClosRec (f, arity)] builds the self-referential record
    that ties the recursive knot of a [let rec] binding. *)
type body =
  | Const of int
  | Var of string
  | BinOp of binop * body * body
  | If of body * body * body
  | App of callee * body list
  | Let of string * body * body
  | Closure of string * int * body list
  | ClosRec of string * int
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
