{
  [@@@ocaml.text "/*"]

  (** Copyright 2023-2025, Kakadu and contributors *)

  (** SPDX-License-Identifier: LGPL-3.0-or-later *)

  [@@@ocaml.text "/*"]

  (* Use `ocamllex frontend/lexer.mll` to get a preview of generated .ml file *)

  open Common
  open Lexing

  let error lexbuf msg =
    let pos = lexbuf.lex_start_p in
    Printf.eprintf "Lexing error at line %d, column %d: %s\n"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg;
    exit 1

  let lex_string lexbuf s =
    let len = String.length s in
    let buf = Buffer.create len in
    let rec loop i =
      if i >= len - 1 then Buffer.contents buf
      else
        match s.[i] with
        | '\\' ->
            let next = i + 1 in
            if next >= len - 1 then
              error lexbuf "unexpected end of string after escape"
            else
              (match s.[next] with
               | '"'  -> Buffer.add_char buf '"'
               | 'n'  -> Buffer.add_char buf '\n'
               | 't'  -> Buffer.add_char buf '\t'
               | 'r'  -> Buffer.add_char buf '\r'
               | '\\' -> Buffer.add_char buf '\\'
               | c    -> error lexbuf (Printf.sprintf "unknown escape '\\%c'" c));
              loop (next + 1)
        | c ->
            Buffer.add_char buf c;
            loop (i + 1)
    in
    loop 1
}

let digit       = ['0'-'9']
let lowercase   = ['a'-'z']
let uppercase   = ['A'-'Z']
let letter      = lowercase | uppercase
let ident_char  = letter | digit | '_' | '\''
let identifier  = letter ident_char*
let int_lit     = digit+
let float_lit   = digit+ '.' digit+ (['e' 'E'] ['+' '-']? digit+)?
let string_char = [^ '"' '\\']
let escape_seq  = '\\' ( '"' | 'n' | 't' | 'r' | '\\' )
let string_body = (string_char | escape_seq)*
let newline     = '\n' | "\r\n"
let whitespace  = (' ' | '\t' | newline)+

rule token = parse
  | whitespace        { token lexbuf }
  | "(*"              { comment 0 lexbuf; token lexbuf }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | "let"             { LET }
  | "rec"             { REC }
  | "in"              { IN }
  | "fun"             { FUN }
  | "->"              { RIGHTARROW }
  | identifier as id  { IDENT id }
  | int_lit as num    { INT (int_of_string num) }
  (* | float_lit as f    { FLOAT (float_of_string f) } *)
  (* | '"' string_body '"' as s *)
                      (* { STRING (lex_string lexbuf s) } *)
  (* | "true"            { BOOL true } *)
  (* | "false"           { BOOL false } *)
  | "()"              { UNIT }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '='               { EQ }
  | '<'               { LT }
  | '>'               { GT }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | eof               { EOF }
  | _ as c            { error lexbuf (Printf.sprintf "unexpected character '%c'" c) }

and comment depth = parse
  | "(*"      { comment (depth + 1) lexbuf }
  | "*)"      { if depth = 0 then () else comment (depth - 1) lexbuf }
  | '\n'      { comment depth lexbuf }
  | eof       { error lexbuf "unclosed comment" }
  | _         { comment depth lexbuf }

{
  let token_stream channel =
    let lexbuf = from_channel channel in
    let rec next () =
      match token lexbuf with
      | EOF -> []
      | tok -> tok :: next ()
    in
    next ()
}
