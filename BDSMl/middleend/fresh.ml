[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Names = Set.Make (String)

type t =
  { next : int
  ; reserved : Names.t
  }

let create names = { next = 0; reserved = Names.of_list names }

let rec fresh_ident base state =
  let next = state.next + 1 in
  let name = Printf.sprintf "%s_%d" base next in
  let state = { state with next } in
  if Names.mem name state.reserved
  then fresh_ident base state
  else name, { state with reserved = Names.add name state.reserved }
;;

let return value state = value, state

let bind computation continuation state =
  let value, state = computation state in
  continuation value state
;;
