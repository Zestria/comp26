[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
open SchonfinkelML_frontend.Lexer

type error = SchonfinkelML_frontend.Lexer.error

let parse_expr (src : string) : (expr, unit) result =
  try
    match
      Parser_gen.program SchonfinkelML_frontend.Lexer.token (Lexing.from_string src)
    with
    | [ StrEval e ] -> Ok e
    | _ -> Error ()
  with
  | Parser_gen.Error -> Error ()
  | SchonfinkelML_frontend.Lexer.Lexing_error e -> Error ()
;;
