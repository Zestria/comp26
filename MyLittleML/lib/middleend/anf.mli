[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Frontend.Ast

type rec_flag =
  | Rec
  | Nonrec
[@@deriving show]

type atom =
  | AInt of int
  | ABool of bool
  | AVar of string
[@@deriving show]

type cexp =
  | CAtom of atom
  | CBinop of A.bop * atom * atom
  | CNeg of atom
  | CApp of atom * atom
  | CIf of atom * aexp * aexp
  | CFun of string list * aexp
[@@deriving show]

and aexp =
  | ACexp of cexp
  | ALet of rec_flag * string * aexp * aexp
[@@deriving show]

type item =
  | ADecl of rec_flag * string * aexp
  | AExp of aexp
[@@deriving show]

type aprogram = item list [@@deriving show]

module StringSet : Set.S with type elt = string

val is_cexp_rhs : aexp -> bool
val anf_prog : A.prog -> aprogram
val to_string : aprogram -> string
