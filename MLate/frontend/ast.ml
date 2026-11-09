[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type rec_flag =
  | Rec
  | Nonrec

type binop =
  | Plus (** [+] *)
  | Minus (** [-] *)
  | Times (** [*] *)
  | Lt (** [<] *)

type name = string

type expr =
  | Int of int
  | Var of name
  | Fun of name * expr (** [fun x -> body]. Multiple parameters use nested [Fun] nodes. *)
  | App of expr * expr (** [f arg]. Application chains associate to the left. *)
  | Binop of binop * expr * expr (** [left op right] *)
  | If of expr * expr * expr (** [if c then t else e] *)
  | Let of binding * expr (** [let [rec] x = value in body] *)

and binding =
  { flag : rec_flag
  ; name : name
  ; value : expr
  }

(** A sequence of top-level bindings in source order. *)
type program = binding list
