{
open Parser

exception SyntaxError of string
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
  | ";;"         { read lexbuf }   (* optional top-level separator, skipped *)
  | int          { INT (int_of_string (Lexing.lexeme lexbuf)) }
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
  | _            { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }