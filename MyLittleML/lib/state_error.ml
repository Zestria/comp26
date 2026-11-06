[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** State + explicit errors. Failures retain state so a parser may rewind
    its cursor while preserving diagnostics. No state is shared between runs. *)
type ('s, 'a, 'e) t = 's -> ('a * 's, 'e * 's) result

let return value state = Ok (value, state)
let fail error state = Error (error, state)

let bind action next state =
  match action state with
  | Ok (value, state) -> next value state
  | Error error -> Error error
;;

let ( let* ) = bind
let get state = Ok (state, state)
let put state _ = Ok ((), state)
let modify f state = Ok ((), f state)

let of_result = function
  | Ok value -> return value
  | Error error -> fail error
;;

let eval action state =
  match action state with
  | Ok (value, _) -> Ok value
  | Error (error, _) -> Error error
;;

let rec map_list f = function
  | [] -> return []
  | x :: xs ->
    let* y = f x in
    let* ys = map_list f xs in
    return (y :: ys)
;;
