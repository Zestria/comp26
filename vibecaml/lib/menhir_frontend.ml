[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let location { Lexing.pos_lnum; pos_bol; pos_cnum; _ } =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)
;;

let run entry source =
  let lexbuf = Lexing.from_string source in
  match entry Menhir_lexer.token lexbuf with
  | tree -> Ok tree
  | exception Menhir_lexer.Error (position, message) ->
    Error (Printf.sprintf "%s: %s" (location position) message)
  | exception Menhir_parser.Error ->
    let position = location (Lexing.lexeme_start_p lexbuf) in
    (match Lexing.lexeme lexbuf with
     | "" -> Error (position ^ ": неожиданный конец ввода")
     | lexeme -> Error (Printf.sprintf "%s: синтаксическая ошибка у %S" position lexeme))
;;

let parse_structure source = run Menhir_parser.structure_eof source
let parse_expr source = run Menhir_parser.expression_eof source
