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
     | "" -> Error (Printf.sprintf "%s: %s" position Lexicon.end_of_input_error)
     (* Модуль [min_int] лексер отдаёт токеном: он допустим после минуса.
        Если парсер споткнулся о него, это переполнение, как у rd-парсера. *)
     | lexeme
       when Option.is_none (int_of_string_opt lexeme)
            && String.for_all Lexicon.is_digit lexeme ->
       Error (Printf.sprintf "%s: %s" position (Lexicon.int_overflow_error lexeme))
     | lexeme -> Error (Printf.sprintf "%s: синтаксическая ошибка у %S" position lexeme))
;;

(* Menhir не поддерживает любые расширения, поэтому при любом из флагов возвращаем ошибку. *)
let check_extensions extensions =
  let enabled =
    List.find_opt
      snd
      [ "--ext-glued-keywords", extensions.Parser_intf.glued_keywords
      ; "--ext-operator-declarations", extensions.Parser_intf.operator_declarations
      ]
  in
  match enabled with
  | None -> Ok ()
  | Some (flag, _) ->
    Error (Printf.sprintf "расширение %s не поддерживается в menhir-парсере" flag)
;;

let with_extensions extensions entry source =
  match check_extensions extensions with
  | Error _ as error -> error
  | Ok () -> run entry source
;;

let parse_structure extensions source =
  with_extensions extensions Menhir_parser.structure_eof source
;;

let parse_expr extensions source =
  with_extensions extensions Menhir_parser.expression_eof source
;;
