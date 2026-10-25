{
[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Menhir_grammar

exception Error of string

let keywords =
  [ "let", LET
  ; "rec", REC
  ; "and", AND
  ; "in", IN
  ; "if", IF
  ; "then", THEN
  ; "else", ELSE
  ; "match", MATCH
  ; "with", WITH
  ; "function", FUNCTION
  ; "fun", FUN
  ; "true", TRUE
  ; "false", FALSE
  ; "not", NOT
  ]
;;
}

let digit = ['0'-'9']
let ident_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let newline = '\r' | '\n' | "\r\n"
let blank = [' ' '\t' '\012']

rule token = parse
  | blank+ { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | digit+ as n ['A'-'Z' 'a'-'z' '_' '\'']
    { raise (Error ("there is no separator after the integer " ^ n)) }
  | digit+ as n
    { match int_of_string_opt n with
      | Some n -> INT n
      | None -> raise (Error ("integer literal is too large: " ^ n)) }
  | "_" { UNDERSCORE }
  | ['a'-'z' '_'] ident_char* as name
    { match List.assoc_opt name keywords with
      | Some keyword -> keyword
      | None -> IDENT name }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ";;" { SEMISEMI }
  | "->" { ARROW }
  | "||" { OROR }
  | "&&" { ANDAND }
  | "|" { BAR }
  | "<>" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<" { LT }
  | ">" { GT }
  | "=" { EQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "~+" { UPLUS }
  | "~-" { UMINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | eof { EOF }
  | _ as c { raise (Error (Printf.sprintf "unexpected character %C" c)) }
