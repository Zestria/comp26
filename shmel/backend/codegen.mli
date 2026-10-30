[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_middleend

(** Code generation for the body of a function (the output of lambda lifting,
    so the body has no [fun] and no local [let rec]).

    The calling convention is the standard one of RISC-V (psABI): the first 8
    arguments come in [a0] .. [a7], the others on the stack, at [0(s0)],
    [8(s0)], ...; the result is returned in [a0]; [t*] and [a*] are not
    preserved by calls. [s0] is the frame pointer, [ra] and the old [s0] are
    saved at [-8(s0)] and [-16(s0)]; stack slots for variables are at
    [-24(s0)], [-32(s0)], ...

    The value of every expression is computed into [a0], temporaries [t0],
    [t1] hold the operands, [t6] addresses far slots. A parameter is read
    straight from its register while the register still holds it ([a0] until
    the first computation, [a1] .. [a7] until the first call); only the
    parameters needed later are saved to slots when the function starts.

    Division by anything but a non-zero constant checks the divisor and, if it
    is zero, calls [shmel_div_by_zero], a runtime function that reports
    [Division_by_zero] like OCaml and does not return. *)

(** What the body needs from the code for calls and closures (part 2). *)
type hooks =
  { load_global : Riscv.reg -> string -> (Riscv.instr list, string) result
    (** loads a name that is neither a parameter nor a local, e.g. a top-level
        function or value, into the register; it must not change any other
        register, the parameters may still be in [a1] .. [a7] *)
  ; call :
      load:(Riscv.reg -> Anf.imm -> (Riscv.instr list, string) result)
      -> Anf.imm
      -> Anf.imm list
      -> (Riscv.instr list, string) result
    (** applies the function to the arguments, the result is in [a0]; [load]
        puts the value of a parameter, local or global into a register (it
        reads only stack slots and globals, so the argument registers can be
        filled in any order) *)
  }

(** Hooks for code without calls and global names: both are errors. *)
val no_calls : hooks

type body =
  { code : Riscv.instr list (** the result is in [a0] *)
  ; locals : int (** bytes of stack for the local slots, a multiple of 16 *)
  }

(** [compile_body hooks ~name ~params body]: [name] is used for the labels of
    branches, so they are unique in the assembly file. *)
val compile_body
  :  hooks
  -> name:string
  -> params:string list
  -> Anf.aexpr
  -> (body, string) result

(** The whole function: label [name], prologue, body and epilogue. *)
val compile_function
  :  hooks
  -> name:string
  -> params:string list
  -> Anf.aexpr
  -> (Riscv.instr list, string) result
