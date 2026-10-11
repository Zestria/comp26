[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Variable or function name. *)
type ident = string [@@deriving show { with_path = false }]

(** Binary operators. *)
type binop =
  | Plus (** Addition. *)
  | Minus (** Subtraction. *)
  | Mult (** Multiplication. *)
  | Div (** Integer division. *)
  | Equal (** Equality. *)
  | NotEqual (** Inequality. *)
  | More (** Greater than. *)
  | Less (** Less than. *)
  | EMore (** Greater than or equal. *)
  | ELess (** Less than or equal. *)
  | And (** Short-circuit conjunction. *)
  | Or (** Short-circuit disjunction. *)
[@@deriving show { with_path = false }]

(** Unary operators. *)
type unop =
  | Neg (** Arithmetic negation. *)
  | Not (** Boolean negation. *)
[@@deriving show { with_path = false }]

(** Whether a binding is recursive. *)
type rec_flag =
  | NonRec (** Non-recursive binding. *)
  | Rec (** Recursive binding. *)
[@@deriving show { with_path = false }]

(** Binding patterns. *)
type pattern =
  | PVar of ident (** Bind a variable. *)
  | PUnit (** Match unit. *)
  | PUnderscore (** Discard the value. *)
[@@deriving show { with_path = false }]

(** Source expressions. *)
type expr =
  | Unit (** Unit literal. *)
  | Int of int (** Integer literal. *)
  | Bool of bool (** Boolean literal. *)
  | Var of ident (** Variable reference. *)
  | BinOp of binop * expr * expr (** Binary operator and its operands. *)
  | UnOp of unop * expr (** Unary operator and its operand. *)
  | If of expr * expr * expr (** Condition, then branch, and else branch. *)
  | Let of rec_flag * pattern * expr * expr (** Binding flag, pattern, value, and body. *)
  | Abs of pattern * expr (** Function parameter and body. *)
  | App of expr * expr (** Function and one argument. *)
[@@deriving show { with_path = false }]

(** Top-level declarations and expressions. *)
type structure_item =
  | StrLet of rec_flag * pattern * expr (** Top-level binding. *)
  | StrEval of expr (** Top-level expression. *)
[@@deriving show { with_path = false }]

(** Source program in declaration order. *)
type program = structure_item list [@@deriving show { with_path = false }]
