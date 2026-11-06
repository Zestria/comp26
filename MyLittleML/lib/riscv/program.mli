[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Input language of the RISC-V backend (Task 3).  See the [.ml] for the
    full documentation. *)

module Reg : sig
  type t =
    | Zero
    | Ra
    | Sp
    | S0
    | A0
    | A1
    | A2
    | A3
    | A4
    | A5
    | A7
    | T0
    | T1
    | T2
    | T3
    | T4
    | T5
    | T6

  val name : t -> string
end

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type body =
  | Env of int
  | Self
  | MakeClosure of string * body list
  | Const of int
  | Var of string
  | BinOp of binop * body * body
  | If of body * body * body
  | App of callee * body list
  | Let of string * body * body
  | PrintInt of body

and callee =
  | Direct of string
  | Value of body

type function_ =
  { name : string
  ; params : string list
  ; body : body
  }

type program = function_ list
