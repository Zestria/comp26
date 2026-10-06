[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type token =
  | TUnit
  | TInt of int
  | TIdent of string
  | TTrue
  | TFalse
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TEqual
  | TNotEqual
  | TMore
  | TLess
  | TEMore
  | TELess
  | TAnd
  | TOr
  | TNot
  | TLParen
  | TRParen
  | TUnderscore
  | TSemisemi (* ;; *)
  | TEOF
  | TIF
  | TThen
  | TElse
  | TLet
  | TRec
  | TIn
  | TFun
  | TArrow (* -> *)
