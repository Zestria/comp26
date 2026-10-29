[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Common fresh-name machinery shared by ANF, closure conversion and lambda lifting.

    Every middle-end pass needs to generate temporary names (`anf_t<i>`,
    `cc_f<i>`, `ll_f<i>` …) that never collide with user identifiers.  This
    module centralises that logic so the three passes no longer keep three
    independent copies of the same counter/taken-set machinery.

    Each pass seeds the generator with its own set of taken names (e.g. via
    {!Anf.collect_names} or its own {!Closure.names_aexp} /
    {!Lambda_lift.names_item} helpers). *)

module StringSet : Set.S with type elt = string

(** State of a fresh-name generator: [taken] holds names that must not be
    reused, [next] is the monotonic counter. *)
type t = {
  taken : StringSet.t;
  mutable next : int;
}

(** Create a state seeded with [taken]. *)
val make : StringSet.t -> t

(** Generate a fresh name with the given prefix (e.g. `"anf_t"`, `"cc_f"`,
    `"ll_f"`), skipping anything already in [taken]. *)
val fresh : t -> string -> string
