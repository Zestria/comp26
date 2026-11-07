[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_middleend

(** Exact liveness and interference for one code body (or the program body).
    [Lifted.validate] guarantees every binder is unique, so identities name
    variables without shadowing. Only [Anf.Var] atoms are uses; constants,
    primitives and code labels are not variables.

    Dataflow on the ANF tree, from a term's live-out set [out]:
    - [Return c]: [in = uses c out].
    - [Let (x, c, t)]: [in = uses c (live_in t out \ {x})].
    - [Sequence (c, t)]: [in = uses c (live_in t out)].
    - [Let_rec (x, r, t)]: with [o = live_in t out], [x] is live everywhere in [r]
      (the address of a statically sized definition is allocated before [r] and
      written after it); [in = live_in r (o ∪ {x}) \ {x}].
    - For computations, [uses c o]: atoms of [Atom], [Tuple], [Field], the
      callee atom of [Indirect] and the arguments of [Apply], and captures of
      [Function] are added to [o]; for [If (a, y, n)] it is
      [{a} ∪ live_in y o ∪ live_in n o] — both arms share the live-out [o]
      of the [If].

    Interference (symmetric, irreflexive):
    - Entry: the [entry] identities (environment slots, then parameters) are
      written simultaneously by the prologue, so they pairwise interfere.
    - Definition point: for [Let (x, c, t)], [x] interferes with every variable
      of [live_in t out \ {x}]. This set is exactly what is live while either arm
      of an [If] writes its result into [x], so values live through a branch
      join never share [x]'s location.
    - [Let_rec (x, r, t)]: [x] interferes with every variable live at any
      point inside [r] and with [live_in t out \ {x}].
    - Every binder inside a [Return (If ...)] arm or an [If] arm follows the
      same rules with the arm's live-out; arms of one [If] never make their
      local binders interfere with each other.
    Two variables that do not interfere are never simultaneously live on any
    path, so they may share a location. *)

type graph = Id.Set.t Id.Map.t

(** Variables live at the start of the term, given its live-out set. *)
val live_in : Lifted.expression -> Id.Set.t -> Id.Set.t

(** All binders of the body ([Let] and [Let_rec]) plus [entry]; every one is a
    key of the result (possibly with an empty neighbour set). The body's
    live-out is empty: its result is returned in [a0]. *)
val interference : entry:Id.t list -> Lifted.expression -> graph
