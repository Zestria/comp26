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
  | Neg of expr
  | BinOp of binop * expr * expr
  | If of expr * expr * expr
  | Let of name * expr * expr
  | LetRec of name * name * expr * expr
[@@deriving show { with_path = false }]

(** A top-level declaration. The name [_] may be bound, by [let] or [fun], but
    is never used as a variable. *)
type decl =
  | DLet of name * expr (** [let x = e]; [let f x = e] is [let f = fun x -> e] *)
  | DLetRec of name * name * expr (** [let rec f x = e] *)
  | DExpr of expr (** an expression at the beginning of the program or after [;;] *)
[@@deriving show { with_path = false }]

(** A program is a sequence of declarations, e.g.
    [let rec fac n = ... let main = print_int (fac 5)]. A program that is a
    single expression is [[DExpr e]]. *)
type program = decl list [@@deriving show { with_path = false }]
