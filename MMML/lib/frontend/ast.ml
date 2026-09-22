[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Parsed, untyped MiniML syntax shared by both parsers. *)

type identifier = string [@@deriving show { with_path = false }]

type constant =
  | Integer of int
  | Boolean of bool
  | Unit
[@@deriving show { with_path = false }]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving show { with_path = false }]

type pattern =
  | PWildcard
  | PVariable of identifier
[@@deriving show { with_path = false }]

type expr =
  | Constant of constant
  | Variable of identifier
  | Lambda of pattern * expr
  | Application of expr * expr
  | If_then_else of expr * expr * expr
  | Let_in of rec_flag * pattern * expr * expr
[@@deriving show { with_path = false }]

type definition = Definition of rec_flag * pattern * expr
[@@deriving show { with_path = false }]

type program = definition list [@@deriving show { with_path = false }]

let application =
  List.fold_left (fun function_ argument -> Application (function_, argument))
;;

let function_ parameters body =
  List.fold_right (fun parameter body -> Lambda (parameter, body)) parameters body
;;

let infix operator left right = application (Variable operator) [ left; right ]
let prefix operator operand = Application (Variable operator, operand)
