[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let parse parser source =
  let lexbuf = Lexing.from_string source in
  try Ok (parser Lexer.token lexbuf) with
  | Lexer.Error message -> Error message
  | Grammar.Error -> Error "Syntax error"
;;

let parse_expression = parse Grammar.expression
let parse_program = parse Grammar.program
