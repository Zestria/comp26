{
[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser

exception Error of string

let keyword_or_identifier = function
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  | "fun" -> FUN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | name -> IDENT name
}

let digit = ['0'-'9']
let initial = ['a'-'z']
let identifier_letter = ['a'-'z' 'A'-'Z' '_']
let identifier_char = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | initial identifier_char* as name { keyword_or_identifier name }
  | digit+ identifier_letter identifier_char* {
      raise (Error "integer literal cannot be followed by an identifier character")
    }
  | digit+ as digits {
      match int_of_string_opt digits with
      | Some value -> INT value
      | None -> raise (Error "integer literal is out of range")
    }
  | "->" { ARROW }
  | '=' { EQUAL }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '<' { LT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ as c { raise (Error (Printf.sprintf "unexpected character %C" c)) }
