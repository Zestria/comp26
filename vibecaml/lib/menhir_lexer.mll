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

(* Ключевые слова OCaml, которых нет в подмножестве. Зарезервированы, чтобы
   наш вход всегда оставался корректным OCaml. *)
let reserved =
  [ "as"; "assert"; "class"; "constraint"; "do"; "done"; "downto"; "effect"
  ; "exception"; "external"; "for"; "function"; "functor"; "include"; "inherit"
  ; "initializer"; "lazy"; "match"; "method"; "module"; "mutable"; "new"
  ; "nonrec"; "object"; "of"; "open"; "private"; "sig"; "struct"; "to"; "try"
  ; "type"; "val"; "virtual"; "when"; "while"; "with"
  ]
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
  | "begin" -> Some BEGIN
  | "end" -> Some END
  | ("mod" | "land" | "lor" | "lxor") as op -> Some (INFIXOP3 op)
  | ("lsl" | "lsr" | "asr") as op -> Some (INFIXOP4 op)
  | "or" -> Some (BARBAR "or")
  | _ -> None
;;
}

let newline = '\r'? '\n'
let blank = [' ' '\t' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolchar_or_hash = symbolchar | '#'
let decimal = ['0'-'9'] ['0'-'9' '_']*
let hex = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F' '_']*
let octal = '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let binary = '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let integer = decimal | hex | octal | binary

rule token = parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | blank+ { token lexbuf }
  | "(*" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; token lexbuf }
  | integer as literal
    { match int_of_string_opt literal with
      | Some value -> INT value
      | None ->
        (* У [min_int] нет положительной записи: проверяем, не он ли это. *)
        (match int_of_string_opt ("-" ^ literal) with
         | Some value when value = min_int -> INT_MIN_ABS
         | Some _ | None -> error lexbuf "целое %s не помещается в int" literal) }
  | integer ['l' 'L' 'n'] as literal
    { error lexbuf "литерал %s вне подмножества: только int" literal }
  | ['0'-'9'] ['0'-'9' '_']* ('.' ['0'-'9' '_']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
    as literal
    { error lexbuf "вещественное число %s вне подмножества" literal }
  | "_" { UNDERSCORE }
  | lowercase identchar* as name
    { match keyword name with
      | Some token -> token
      | None when List.mem name reserved ->
        error lexbuf "ключевое слово %s вне подмножества" name
      | None -> LIDENT name }
  | uppercase identchar* as name
    { error lexbuf "конструктор или модуль %s вне подмножества" name }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ";;" { SEMISEMI }
  | "->" { MINUSGREATER }
  | "=" { EQUAL }
  | ":=" { COLONEQUAL }
  | "+" { PLUS }
  | "+." { PLUSDOT }
  | "-" { MINUS }
  | "-." { MINUSDOT }
  | "!" { BANG }
  | "||" { BARBAR "||" }
  | "&&" { AMPERAMPER "&&" }
  | "&" { AMPERAMPER "&" }
  | "!=" { INFIXOP0 "!=" }
  (* Одиночные [|], [<-], [::] и прочие в OCaml --- синтаксис, а не операторы.
     Правила выше классов операторов: при равной длине ocamllex берёт первое. *)
  | ("|" | "<-" | "::" | ":" | "." | "#" | "~" | "?") as symbol
    { error lexbuf "символ %s вне подмножества" symbol }
  | "!" symbolchar_or_hash+ as op { PREFIXOP op }
  | ['~' '?'] symbolchar_or_hash+ as op { PREFIXOP op }
  | ['=' '<' '>' '|' '&' '$'] symbolchar* as op { INFIXOP0 op }
  | ['@' '^'] symbolchar* as op { INFIXOP1 op }
  | ['+' '-'] symbolchar* as op { INFIXOP2 op }
  | "**" symbolchar* as op { INFIXOP4 op }
  | ['*' '/' '%'] symbolchar* as op { INFIXOP3 op }
  | '#' symbolchar_or_hash+ as op { HASHOP op }
  | eof { EOF }
  | _ as symbol { error lexbuf "неожиданный символ %C" symbol }

(* Комментарии вложенные, как в OCaml. [start] --- начало самого внешнего: о
   незакрытом комментарии сообщаем там, где он открыт. *)
and comment start = parse
  | "(*" { comment start lexbuf; comment start lexbuf }
  | "*)" { () }
  | newline { Lexing.new_line lexbuf; comment start lexbuf }
  | eof { raise (Error (start, "незакрытый комментарий")) }
  | _ { comment start lexbuf }
