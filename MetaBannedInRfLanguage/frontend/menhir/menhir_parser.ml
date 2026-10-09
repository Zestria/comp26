[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let position_to_string (position : Lexing.position) =
  Printf.sprintf
    "line %d, column %d"
    position.pos_lnum
    (position.pos_cnum - position.pos_bol + 1)
;;

let parse_structure input =
  let lexbuf = Lexing.from_string input in
  try Ok (Menhir_grammar.structure Menhir_lexer.token lexbuf) with
  | Menhir_lexer.Error message ->
    Error (Printf.sprintf "%s: %s" (position_to_string lexbuf.lex_start_p) message)
  | Menhir_grammar.Error ->
    Error
      (Printf.sprintf
         "%s: syntax error at %S"
         (position_to_string lexbuf.lex_start_p)
         (Lexing.lexeme lexbuf))
;;
