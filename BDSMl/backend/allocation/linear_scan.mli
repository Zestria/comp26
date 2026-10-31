[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Conservative, deterministic linear scan over alpha-renamed structured ANF.
    Branches are numbered consecutively; outer live intervals span both arms.
    No interval splitting or path-sensitive lifetime holes are assumed. *)
type interval =
  { name : string
  ; start : int
  ; finish : int
  }

type assignment =
  | Register of Assembly.Asm.register
  | Spill of int

(** Only referenced local bindings have intervals; globals are excluded. *)
val intervals
  : BDSMl_frontend.Ast.pattern list
  -> BDSMl_middleend.Anf.Anf_ast.aexpr list
  -> interval list

(** Sort by start, expire finished intervals, and on pressure spill the interval
    ending furthest in the future. Assignments cover entire lifetimes: an evicted
    binding is placed in a spill slot from its definition, not moved mid-block.
    Spill indices start at zero. State is local to this invocation. *)
val allocate
  : registers:Assembly.Asm.register list
  -> interval list
  -> (string * assignment) list

(** Reserved callee-saved pool, disjoint from argument and emitter scratch regs. *)
val registers : Assembly.Asm.register list
