[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Generate GNU-compatible RV64GC/lp64d assembly from alpha-renamed ANF.
    Functions must be lifted, and short-circuit operations lowered to [CIf].
    Returns diagnostics for unknown names/unsupported ANF. Generated code links
    against the contract in [runtime/runtime.h]; no CLI or runtime implementation
    is included here. Each call uses its own symbol/label state. *)
val generate : BDSMl_middleend.Anf.Anf_ast.aprogram -> (string, string) result
