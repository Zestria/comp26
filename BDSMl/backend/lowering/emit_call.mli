[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Locally bound identifiers and their frame offsets. *)
type environment = (string * int) list

type context =
  { symbols : Symbols.t
  ; frame : Allocation.Frame.t
  }

(** Load an immediate. Function values allocate closures and may clobber
    caller-saved registers; local values remain in stable stack slots. *)
val value
  :  context
  -> environment
  -> Assembly.Asm.register
  -> BDSMl_middleend.Anf.Anf_ast.immexpr
  -> unit Assembly.Asm.code

(** Whether loading the immediate can invoke [bd_make_closure]. *)
val allocates : context -> environment -> BDSMl_middleend.Anf.Anf_ast.immexpr -> bool

(** Result in [a0]. Exactly saturated known functions use a direct call;
    other applications use [bd_apply]. Arguments are staged in stack slots
    before loading ABI registers, including arguments beyond [a7]. *)
val call
  :  context
  -> environment
  -> BDSMl_middleend.Anf.Anf_ast.immexpr
  -> BDSMl_middleend.Anf.Anf_ast.immexpr list
  -> unit Assembly.Asm.code
