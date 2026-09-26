{
(* SPDX-License-Identifier: LGPL-3.0-or-later *)

open Grammar

exception Error of string

let checked print_error = function
  | Ok value -> value
  | Error error -> raise (Error (print_error error))

let operator text =
  if text = "->" then ARROW
  else
    let name = checked Name.error_to_string (Name.of_operator text) in
    match text with
    | "=" -> EQUAL name
    | "-" -> MINUS name
    | _ ->
      match Name.operator_kind name with
      | Some Prefix -> PREFIX name
      | Some Hash -> HASHOP name
      | Some Comparison -> INFIX0 name
      | Some Concatenation -> INFIX1 name
      | Some Additive -> INFIX2 name
      | Some Multiplicative -> INFIX3 name
      | Some Power -> INFIX4 name
      | Some Conjunction -> AND name
      | Some Disjunction -> OR name
      | Some Assignment -> ASSIGN name
      | None -> raise (Error (Name.error_to_string (Name.Invalid_operator text)))

let word text =
  match Keyword.of_string text with
  | Some True -> TRUE
  | Some False -> FALSE
  | Some Let -> LET
  | Some Rec -> REC
  | Some In -> IN
  | Some Fun -> FUN
  | Some If -> IF
  | Some Then -> THEN
  | Some Else -> ELSE
  | Some Mod -> operator text
  | None ->
    match text with
    | "_" -> UNDERSCORE
    | text when text.[0] >= '0' && text.[0] <= '9' ->
      INT (checked Integer_literal.error_to_string (Integer_literal.of_string text))
    | text -> IDENT (checked Name.error_to_string (Name.of_identifier text))
}

let identifier_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let identifier_start = ['a'-'z' 'A'-'Z' '_']
let symbol_char = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbol = ['!' '~' '?' '#'] (symbol_char | '#')*
           | ['=' '<' '>' '|' '&' '$' '@' '^' '+' '-' '*' '/' '%'] symbol_char*
let whitespace = [' ' '\t' '\n' '\r' '\012']
let digit = ['0'-'9']
let octal = ['0'-'7']
let character_escape = '\\' (['\\' '"' '\'' 'n' 't' 'b' 'r' ' ']
                            | digit digit digit
                            | 'o' ['0'-'3'] octal octal)
let character = "''" | '\'' (character_escape | '\r'* '\n' | [^ '\\' '\'' '\n' '\r']) '\''
let delimiter = ['a'-'z' '_']*
let extension_name = identifier_start identifier_char*
let extension = '%' '%'? extension_name ('.' extension_name)* [' ' '\t' '\012']*

rule token = parse
  | whitespace+ { token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }
  | identifier_char+ as text { word text }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | ";;" { SEMISEMI }
  | ":=" as text { operator text }
  | symbol as text { operator text }
  | eof { EOF }
  | _ as c { raise (Error (Printf.sprintf "Unexpected character: %C" c)) }

and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | '"' { string_in_comment lexbuf; comment lexbuf }
  | character { comment lexbuf }
  | '{' extension? (delimiter as tag) '|' { quoted_contents tag lexbuf; comment lexbuf }
  | identifier_start identifier_char* { comment lexbuf }
  | eof { raise (Error "Unclosed comment") }
  | _ { comment lexbuf }

and string_in_comment = parse
  | '"' { () }
  | '\\' _ { string_in_comment lexbuf }
  | eof { raise (Error "Unclosed string in comment") }
  | _ { string_in_comment lexbuf }

and quoted_contents tag = parse
  | '|' (delimiter as closing) '}'
      { if closing <> tag then quoted_contents tag lexbuf }
  | eof { raise (Error "Unclosed quoted string in comment") }
  | _ { quoted_contents tag lexbuf }
