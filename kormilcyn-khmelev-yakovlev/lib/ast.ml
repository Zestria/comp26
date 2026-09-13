[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string [@@deriving show { with_path = false }]

type binop =
  | Add (** '+' *)
  | Sub (** '-' *)
  | Mul (** '*' *)
  | Div (** '/' *)
  | Eq (** '=' *)
  | Lt (** '<' *)
  | Gt (** '>' *)
  | Le (** '<=' *)
  | Ge (** '>=' *)
[@@deriving show { with_path = false }]

type expr =
  | Var of name
  | Abs of name * expr
  | App of expr * expr
  | Const of int
  | BinOp of binop * expr * expr
  | If of expr * expr * expr
  | Let of name * expr * expr
  | LetRec of name * name * expr * expr
[@@deriving show { with_path = false }]
