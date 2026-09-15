{
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lexer

type error = [ `lexer_error of string ]

exception Error of string

let error position message =
  raise
    (Error
       (Printf.sprintf
          "%d:%d: %s"
          position.Lexing.pos_lnum
          (position.Lexing.pos_cnum - position.Lexing.pos_bol + 1)
          message))

let integer lexbuf literal =
  match int_of_string_opt literal with
  | Some value -> TInt value
  | None ->
    error (Lexing.lexeme_start_p lexbuf)
      (Printf.sprintf "integer literal '%s' out of range" literal)
}

let digit = ['0'-'9']
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_char = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let newline = '\r' '\n' | '\n' | '\r'

rule token = parse
  | [' ' '\t' '\012']+ { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { comment (Lexing.lexeme_start_p lexbuf) 1 lexbuf; token lexbuf }
  | "let" { TLet }
  | "rec" { TRec }
  | "in" { TIn }
  | "fun" { TFun }
  | "if" { TIF }
  | "then" { TThen }
  | "else" { TElse }
  | "true" { TTrue }
  | "false" { TFalse }
  | "not" { TNot }
  | '_' { TUnderscore }
  | ident_start ident_char* as name { TIdent name }
  | digit+ as literal { integer lexbuf literal }
  | "->" { TArrow }
  | '+' { TPlus }
  | '-' { TMinus }
  | '*' { TStar }
  | '/' { TSlash }
  | '=' { TEqual }
  | "<>" { TNotEqual }
  | '>' { TMore }
  | '<' { TLess }
  | ">=" { TEMore }
  | "<=" { TELess }
  | "&&" { TAnd }
  | "||" { TOr }
  | "()" { TUnit }
  | '(' { TLParen }
  | ')' { TRParen }
  | ";;" { TSemisemi }
  | '&' { error (Lexing.lexeme_start_p lexbuf) "unexpected '&' (expected '&&')" }
  | '|' { error (Lexing.lexeme_start_p lexbuf) "unexpected '|' (expected '||')" }
  | eof { TEOF }
  | _ as character
      { error (Lexing.lexeme_start_p lexbuf)
          (Printf.sprintf "unexpected character '%c'" character) }

and comment start depth = parse
  | "(*" { comment start (depth + 1) lexbuf }
  | "*)" { if depth > 1 then comment start (depth - 1) lexbuf }
  | newline { Lexing.new_line lexbuf; comment start depth lexbuf }
  | eof { error start "unterminated comment" }
  | _ { comment start depth lexbuf }

{
let tokenize source =
  let lexbuf = Lexing.from_string source in
  let rec collect acc =
    match token lexbuf with
    | TEOF -> Ok (List.rev (TEOF :: acc))
    | next -> collect (next :: acc)
  in
  try collect [] with
  | Error message -> Result.Error (`lexer_error message)
}
