{
[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
}

let blank = [' ' '\t' '\r']+
let digit = ['0'-'9']
let const = digit+
let id = ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*

rule read =
  parse
  | blank { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "<=" { LE}
  | "<" { LT}
  | ">=" { GE}
  | ">" { GT}
  | "*" { TIMES }
  | "/" { DIVISION }
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "rec" {REC}
  | "->" { ARROW }
  | ";;" { SEMISEMI }
  | "and" { raise (Failure "let ... and ... is not supported") }
  | id { ID (Lexing.lexeme lexbuf) }
  | const { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
