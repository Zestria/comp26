type token =
  | Unit
  | TInt of int
  | Idebt of string
  | True
  | False
  | Plus
  | Minus
  | Star
  | Slash
  | Equal
  | NotEqual
  | More
  | Less
  | EMore
  | ELess
  | And
  | Or
  | Not
  | LPAREN
  | RPAREN
  | UNDERSCORE
  | SEMISEMI (* ;; *)
  | EOF
  | IF
  | THEN
  | ELSE
  | LET
  | REC
  | IN
  | FUN
  | ARROW (* -> *)

type error = [ `lexer_error of string ]

val tokenize : string -> token list
