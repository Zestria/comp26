[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
module StringSet : Set.S with type elt = string

val idents_of_expr : StringSet.t -> expr -> StringSet.t

type naming =
  { reserved : StringSet.t
  ; counter : int
  }

val naming_of : StringSet.t -> naming
val gensym : naming -> ?prefix:string -> unit -> string * naming
