[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Emit using allocated locations; result in [destination] (default [a0]). Branches join
    once; branch-local bindings never escape into the other branch. *)
val expression
  :  ?destination:Assembly.Asm.register
  -> Emit_call.context
  -> Emit_call.environment
  -> BDSMl_middleend.Anf.Anf_ast.aexpr
  -> unit Assembly.Asm.code
