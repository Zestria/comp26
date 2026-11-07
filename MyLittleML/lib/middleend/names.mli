[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module StringSet : Set.S with type elt = string

type t

val make : StringSet.t -> t
val fresh : string -> (t, string) State.t
