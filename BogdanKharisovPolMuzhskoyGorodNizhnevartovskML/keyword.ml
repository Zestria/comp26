[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t =
  | Else
  | False
  | Fun
  | If
  | In
  | Let
  | Rec
  | Then
  | True
  | Mod

let to_string = function
  | Else -> "else"
  | False -> "false"
  | Fun -> "fun"
  | If -> "if"
  | In -> "in"
  | Let -> "let"
  | Rec -> "rec"
  | Then -> "then"
  | True -> "true"
  | Mod -> "mod"
;;

let of_string text =
  List.find_opt
    (fun keyword -> String.equal text (to_string keyword))
    [ Else; False; Fun; If; In; Let; Rec; Then; True; Mod ]
;;
