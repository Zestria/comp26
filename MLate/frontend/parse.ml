[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error message -> Format.pp_print_string ppf message
;;

let error lexbuf message =
  let position = Lexing.lexeme_start_p lexbuf in
  let column = position.pos_cnum - position.pos_bol + 1 in
  Error
    (`Parsing_error
        (Printf.sprintf "line %d, column %d: %s" position.pos_lnum column message))
;;

let parse source =
  let lexbuf = Lexing.from_string source in
  try Ok (Parser.program Lexer.token lexbuf) with
  | Lexer.Error message -> error lexbuf message
  | Parser.Error ->
    let token = Lexing.lexeme lexbuf in
    let message =
      if token = ""
      then "unexpected end of input"
      else Printf.sprintf "unexpected token %S" token
    in
    error lexbuf message
;;
