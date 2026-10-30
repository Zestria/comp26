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

let parse_menhir_result parser s =
  let lexbuf = Lexing.from_string s in
  (* Points to the token where parsing stopped; columns start from 1. *)
  let error_at message =
    let pos = lexbuf.Lexing.lex_start_p in
    Result.Error
      (`Parsing_error
          (Printf.sprintf
             "%s at line %d, column %d"
             message
             pos.Lexing.pos_lnum
             (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)))
  in
  match parser Lexer.read lexbuf with
  | ast -> Result.Ok ast
  | exception Parser.Error -> error_at "syntax error"
  | exception Failure message -> error_at message
;;

let parse_with_menhir_result = parse_menhir_result Parser.prog
let parse_with_comb = Parser_comb.parse
let parse_with_comb_extended = Parser_comb.parse_extended
let parse_program_with_comb = Parser_comb.parse_program
let parse_program_with_comb_extended = Parser_comb.parse_program_extended
let parse_program_with_menhir = parse_menhir_result Parser.program
