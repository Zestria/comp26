[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module StringSet = Set.Make (String)

type t =
  { taken : StringSet.t
  ; next : int
  }

let make taken = { taken; next = 0 }

let rec fresh prefix state =
  let name = prefix ^ string_of_int state.next in
  let next = { state with next = state.next + 1 } in
  if StringSet.mem name state.taken
  then fresh prefix next
  else name, { next with taken = StringSet.add name next.taken }
;;
