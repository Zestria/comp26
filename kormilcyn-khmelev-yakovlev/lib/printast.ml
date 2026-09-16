[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Format

let rec pp fmt = function
  | Var s -> fprintf fmt "%s" s
  | App (l, r) -> fprintf fmt "(%a %a)" pp l pp r
  | Abs (x, body) -> fprintf fmt "(fun %s -> %a)" x pp body
  | Const n when n < 0 -> fprintf fmt "(%d)" n
  | Const n -> fprintf fmt "%d" n
  | Neg e -> fprintf fmt "(-%a)" pp e
  | If (c, t, e) -> fprintf fmt "(if %a then %a else %a)" pp c pp t pp e
  | Let (n, e1, e2) -> fprintf fmt "(let %s = %a in %a)" n pp e1 pp e2
  | LetRec (f, p, b, inb) -> fprintf fmt "(let rec %s %s = %a in %a)" f p pp b pp inb
  | BinOp (op, l, r) ->
    let s =
      match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Eq -> "="
      | Lt -> "<"
      | Gt -> ">"
      | Le -> "<="
      | Ge -> ">="
    in
    fprintf fmt "(%a %s %a)" pp l s pp r
;;

let show term =
  let buf = Buffer.create 256 in
  let fmt = formatter_of_buffer buf in
  pp fmt term;
  pp_print_flush fmt ();
  Buffer.contents buf
;;
