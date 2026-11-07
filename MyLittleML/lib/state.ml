[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Pure state computations. Each run owns an immutable state value. *)
type ('s, 'a) t = 's -> 'a * 's

let return value state = value, state

let bind action next state =
  let value, state = action state in
  next value state
;;

let ( let* ) = bind
let get state = state, state
let put state _ = (), state
let modify f state = (), f state
let eval action state = fst (action state)

let rec map_list f = function
  | [] -> return []
  | x :: xs ->
    let* y = f x in
    let* ys = map_list f xs in
    return (y :: ys)
;;
