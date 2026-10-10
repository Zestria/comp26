[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let comparison primitive order =
  match primitive with
  | Primitive.Equal -> Some (order = 0)
  | Not_equal -> Some (order <> 0)
  | Less -> Some (order < 0)
  | Less_equal -> Some (order <= 0)
  | Greater -> Some (order > 0)
  | Greater_equal -> Some (order >= 0)
  | Add | Subtract | Multiply | Divide | Modulo | Negate | And | Or | Not -> None
;;

let apply primitive arguments =
  let open Frontend.Constant in
  let result =
    match primitive, arguments with
    | Primitive.Not, [ Bool value ] -> Some (not value)
    | And, [ Bool left; Bool right ] -> Some (left && right)
    | Or, [ Bool left; Bool right ] -> Some (left || right)
    | _, [ Bool left; Bool right ] -> comparison primitive (Bool.compare left right)
    | _, [ Unit; Unit ] -> comparison primitive 0
    | _ -> None
  in
  Option.map (fun value -> Bool value) result
;;
