{
open Parser
}

(* TODO: functions *)
let white = [' ' '\t']+
let digit = ['0'-'9']
let const = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+ (*TODO: ID like in OCaml*)

rule read =
  parse
  | white { read lexbuf }
  | "<=" { LE}
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | const { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
