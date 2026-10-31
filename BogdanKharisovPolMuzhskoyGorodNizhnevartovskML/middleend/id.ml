[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t =
  { name : string
  ; stamp : int
  }

type supply = int
type error = Exhausted

let error_to_string Exhausted = "Binding identifier space exhausted"
let initial = 0

let fresh supply name =
  if supply = max_int then Error Exhausted else Ok ({ name; stamp = supply }, supply + 1)
;;

let name id = id.name
let rename supply id = fresh supply id.name
let compare a b = Int.compare a.stamp b.stamp
let equal a b = compare a b = 0

module Ordered = struct
  type nonrec t = t

  let compare = compare
end

module Set = Set.Make (Ordered)
module Map = Map.Make (Ordered)
