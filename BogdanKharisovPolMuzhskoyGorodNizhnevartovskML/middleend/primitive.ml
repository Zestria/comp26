[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Negate
  | Equal
  | Not_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | And
  | Or
  | Not
  | Print_int

let name = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Modulo -> "mod"
  | Negate -> "~-"
  | Equal -> "="
  | Not_equal -> "<>"
  | Less -> "<"
  | Less_equal -> "<="
  | Greater -> ">"
  | Greater_equal -> ">="
  | And -> "&&"
  | Or -> "||"
  | Not -> "not"
  | Print_int -> "print_int"
;;

let all =
  [ Add
  ; Subtract
  ; Multiply
  ; Divide
  ; Modulo
  ; Negate
  ; Equal
  ; Not_equal
  ; Less
  ; Less_equal
  ; Greater
  ; Greater_equal
  ; And
  ; Or
  ; Not
  ; Print_int
  ]
;;
