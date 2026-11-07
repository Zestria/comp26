{
(* The header is inside the ocamllex header section: only the { ... } block is
   copied into the generated lexer.ml. *)
[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser

let error_message token =
  if String.length token > 0 && String.for_all (function '0' .. '9' -> true | _ -> false) token
     && Option.is_none (int_of_string_opt token)
  then Some "integer literal is out of range"
  else if String.length token = 1
          && not (String.contains "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+-*/=<>(); \t\r\n" token.[0])
  then Some ("Unexpected character: " ^ token)
  else None
}

let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+

let digit = ['0'-'9']
let int = digit+

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* Non-nested comments. *)
let comment = "(*" ([^ '*'] | '*' [^ ')'])* "*)"

rule read = parse
  | white        { read lexbuf }
  | newline      { Lexing.new_line lexbuf; read lexbuf }
  | comment      { read lexbuf }
  | ";;"         { SEMI }          (* top-level phrase separator *)
  | int          { match int_of_string_opt (Lexing.lexeme lexbuf) with Some n -> INT n | None -> INVALID }
  | "true"       { TRUE }
  | "false"      { FALSE }
  | "let"        { LET }
  | "rec"        { REC }
  | "in"         { IN }
  | "fun"        { FUN }
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }
  | "mod"        { MOD }
  | "->"         { ARROW }
  | "="          { EQ }
  | "<>"         { NEQ }
  | "<="         { LE }
  | ">="         { GE }
  | "<"          { LT }
  | ">"          { GT }
  | "+"          { PLUS }
  | "-"          { MINUS }
  | "*"          { STAR }
  | "/"          { SLASH }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | id           { ID (Lexing.lexeme lexbuf) }
  | eof          { EOF }
  | _            { INVALID }