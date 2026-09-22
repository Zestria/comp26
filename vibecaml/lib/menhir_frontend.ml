[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let location { Lexing.pos_lnum; pos_bol; pos_cnum; _ } =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
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
     | "" -> Error (Printf.sprintf "%s: %s" position Lexicon.end_of_input_error)
     (* Модуль [min_int] лексер отдаёт токеном: он допустим после минуса.
        Если парсер споткнулся о него, это переполнение, как у rd-парсера. *)
     | lexeme
       when Option.is_none (int_of_string_opt lexeme) && String.for_all is_digit lexeme ->
       Error (Printf.sprintf "%s: %s" position (Lexicon.int_overflow_error lexeme))
     | lexeme -> Error (Printf.sprintf "%s: синтаксическая ошибка у %S" position lexeme))
;;

let parse_structure source = run Menhir_parser.structure_eof source
let parse_expr source = run Menhir_parser.expression_eof source
