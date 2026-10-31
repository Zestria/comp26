[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Middleend

(** Every variable of a code body lives in exactly one location for its whole
    lifetime (no splitting), so branch joins need no reconciling moves.
    Registers come only from [S 1] .. [S 11]. They are callee-saved, so every
    value survives calls, allocation and runtime calls without extra code; the
    prologue saves exactly [saved]. Argument, temporary, [ra], [sp] and [s0]
    registers are never allocated.

    Graph colouring over [Liveness.interference]: variables are coloured
    greedily in definition order (entry identities first); a variable whose
    neighbours already occupy all eleven registers receives a stack slot
    instead, and slots are coloured over the same graph, so non-interfering
    spilled variables share a slot. *)

type location =
  | Reg of Riscv.reg
  | Slot of int (** index from 0; the byte offset is chosen by [Codegen] *)

type t =
  { locations : location Id.Map.t (** every key of the interference graph *)
  ; saved : Riscv.reg list (** distinct [S i] used, ascending *)
  ; slots : int (** number of distinct slots used *)
  }

val allocate : entry:Id.t list -> Lifted.expression -> t
