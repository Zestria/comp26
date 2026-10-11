[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let parse source =
  let lexbuf = Lexing.from_string source in
  let source : (Tokens.token, LR_parser.Lexer.error) result Seq.t =
    LR_parser.Lexer.tokenize_lazy_from_lexbuf lexbuf
  in
  let tokens = ref source in
  let next_token _ =
    match !tokens () with
    | Seq.Cons (Ok token, rest) ->
      tokens := rest;
      token
    | Seq.Cons (Error (`lexer_error message), _) ->
      raise (LR_parser.Lexer.Error message)
    | Seq.Nil -> Tokens.TEOF
  in
  try Ok (LR_parser.Parser.program next_token lexbuf) with
  | LR_parser.Lexer.Error message -> Error message
  | LR_parser.Parser.Error ->
    let position = Lexing.lexeme_start_p lexbuf in
    Error
      (Printf.sprintf
         "Line %d, column %d: Syntax error"
         position.Lexing.pos_lnum
         (position.Lexing.pos_cnum - position.Lexing.pos_bol + 1))
;;
