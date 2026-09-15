{
open Parser
}

(* TODO: functions *)
let white = [' ' '\t']+
let digit = ['0'-'9']
let const = '-'? digit+
let id = ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*

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
  | "fun" { FUN }
  | "->" { ARROW }
  | id { ID (Lexing.lexeme lexbuf) }
  | const { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
