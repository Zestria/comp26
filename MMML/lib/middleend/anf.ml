[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type id =
  { stamp : int
  ; hint : string
  }

type unary =
  | Negate
  | Positive

type binary =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Equal
  | Not_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal

type builtin =
  | Unary of unary
  | Binary of binary
  | And
  | Or

type imm =
  | Constant of Ast.constant
  | Local of id
  | External of string
  | Builtin of builtin

type compound =
  | Value of imm
  | Apply of imm * imm
  | Unary_primitive of unary * imm
  | Binary_primitive of binary * imm * imm
  | Lambda of id * term
  | If of imm * term * term

and term =
  | Result of compound
  | Let of id * compound * term
  | Let_rec of id * id * term * term

type definition =
  | Definition of id * term
  | Recursive_definition of id * id * term

type program = definition list

let builtin_name = function
  | Unary Negate -> "~-"
  | Unary Positive -> "~+"
  | Binary Add -> "+"
  | Binary Subtract -> "-"
  | Binary Multiply -> "*"
  | Binary Divide -> "/"
  | Binary Equal -> "="
  | Binary Not_equal -> "<>"
  | Binary Less -> "<"
  | Binary Less_equal -> "<="
  | Binary Greater -> ">"
  | Binary Greater_equal -> ">="
  | And -> "&&"
  | Or -> "||"
;;

let builtins =
  [ Unary Negate
  ; Unary Positive
  ; Binary Add
  ; Binary Subtract
  ; Binary Multiply
  ; Binary Divide
  ; Binary Equal
  ; Binary Not_equal
  ; Binary Less
  ; Binary Less_equal
  ; Binary Greater
  ; Binary Greater_equal
  ; And
  ; Or
  ]
;;

let default_bindings = List.map (fun b -> builtin_name b, Builtin b) builtins
