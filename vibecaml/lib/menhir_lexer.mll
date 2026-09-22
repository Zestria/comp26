(* Шапка с лицензией внутри фигурных скобок: ocamllex копирует её в
   сгенерированный .ml, где её ищет Zanuda. *)

{
[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Menhir_parser

exception Error of Lexing.position * string

let error lexbuf format =
  Printf.ksprintf
    (fun message -> raise (Error (Lexing.lexeme_start_p lexbuf, message)))
    format
;;

let keyword = function
  | "let" -> Some LET
  | "rec" -> Some REC
  | "and" -> Some AND
  | "in" -> Some IN
  | "fun" -> Some FUN
  | "if" -> Some IF
  | "then" -> Some THEN
  | "else" -> Some ELSE
  | "true" -> Some TRUE
  | "false" -> Some FALSE
  | _ -> None
;;

(* Класс оператора определяет его приоритет в грамматике. Операторы вне
   подмножества отсекаются раньше, по [Lexicon.operators]. *)
let operator = function
  | "=" -> EQUAL
  | "->" -> MINUSGREATER
  | "+" -> PLUS
  | "-" -> MINUS
  | "||" -> BARBAR
  | "&&" -> AMPERAMPER
  | ("<>" | "<" | "<=" | ">" | ">=") as op -> INFIXOP0 op
  | ("+." | "-.") as op -> INFIXOP2 op
  | ("**" | "lsl" | "lsr" | "asr") as op -> INFIXOP4 op
  | op -> INFIXOP3 op
;;
}

let newline = '\r'? '\n'
let blank = [' ' '\t' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal = ['0'-'9']+

rule token = parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | blank+ { token lexbuf }
  | "(*" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; token lexbuf }
  | decimal as literal
    { match int_of_string_opt literal with
      | Some value -> INT value
      | None ->
        (* У [min_int] нет положительной записи: проверяем, не он ли это. *)
        (match int_of_string_opt ("-" ^ literal) with
         | Some value when value = min_int -> INT_MIN_ABS
         | Some _ | None -> error lexbuf "%s" (Lexicon.int_overflow_error literal)) }
  (* [1x], [1L], [0x1F], [1_000] и [1.5] для OCaml --- один литерал, а не
     число и что-то следом. *)
  | decimal (identchar | '.')+ as literal
    { error lexbuf "%s" (Lexicon.literal_error literal) }
  | "_" { UNDERSCORE }
  | lowercase identchar* as name
    { match keyword name with
      | Some token -> token
      | None when List.mem name Lexicon.reserved ->
        error lexbuf "%s" (Lexicon.reserved_error name)
      | None when List.mem name Lexicon.operators -> operator name
      | None -> LIDENT name }
  | uppercase identchar* as name
    { error lexbuf "%s" (Lexicon.constructor_error name) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  (* Оператор читается целиком, как в OCaml: [+-] --- один неизвестный
     оператор, а не [+] и [-]. *)
  | symbolchar+ as op
    { if String.equal op "->" || List.mem op Lexicon.operators
      then operator op
      else error lexbuf "%s" (Lexicon.operator_error op) }
  | eof { EOF }
  | _ as symbol { error lexbuf "%s" (Lexicon.unexpected_char_error symbol) }

(* Комментарии вложенные, как в OCaml. [start] --- начало самого внешнего: о
   незакрытом комментарии сообщаем там, где он открыт. *)
and comment start = parse
  | "(*" { comment start lexbuf; comment start lexbuf }
  | "*)" { () }
  | newline { Lexing.new_line lexbuf; comment start lexbuf }
  | eof { raise (Error (start, Lexicon.unclosed_comment_error)) }
  | _ { comment start lexbuf }
