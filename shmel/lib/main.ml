[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let parse_with_menhir s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let parse_with_menhir_result s =
  let lexbuf = Lexing.from_string s in
  let error_at message =
    let pos = lexbuf.Lexing.lex_curr_p in
    Result.Error
      (`Parsing_error
          (Printf.sprintf
             "%s at line %d, column %d"
             message
             pos.Lexing.pos_lnum
             (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)))
  in
  match Parser.prog Lexer.read lexbuf with
  | ast -> Result.Ok ast
  | exception Parser.Error -> error_at "syntax error"
  | exception Failure message -> error_at message
;;

let parse_with_comb = Parser_comb.parse
let parse_with_comb_extended = Parser_comb.parse_extended
