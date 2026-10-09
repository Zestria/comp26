[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
module StringSet = Set.Make (String)

let rec idents_of_expr acc = function
  | Ident x -> StringSet.add x acc
  | Const _ -> acc
  | App (e1, e2) -> idents_of_expr (idents_of_expr acc e1) e2
  | Let (_, v, e1, e2) -> idents_of_expr (idents_of_expr (StringSet.add v acc) e1) e2
  | If (e1, e2, e3) -> idents_of_expr (idents_of_expr (idents_of_expr acc e1) e2) e3
  | Abstr (v, e) -> idents_of_expr (StringSet.add v acc) e
;;

(* Fresh names must not collide with names that occur in the source. *)
type naming =
  { reserved : StringSet.t
  ; counter : int
  }

let naming_of idents = { reserved = idents; counter = 0 }

let gensym (naming : naming) ?(prefix = "temp") () =
  let rec pick counter =
    let name = Printf.sprintf "%s%d" prefix counter in
    if StringSet.mem name naming.reserved then pick (counter + 1) else name, counter
  in
  let name, counter = pick (naming.counter + 1) in
  name, { naming with counter }
;;
