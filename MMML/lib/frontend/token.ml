(** Tokens produced by the MiniML lexer. Each supported operator has an
    explicit constructor, so extending the language requires an explicit
    change to this type. *)

type t =
  | INT of (int[@gen QCheck.Gen.nat_small])
  | IDENT of
      (string
      [@gen
        QCheck.Gen.map (fun value -> "value_" ^ string_of_int value) QCheck.Gen.nat_small])
  | LET
  | REC
  | AND
  | IN
  | FUN
  | FUNCTION
  | IF
  | THEN
  | ELSE
  | MATCH
  | WITH
  | TRUE
  | FALSE
  | UNDERSCORE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | COMMA
  | SEMICOLON
  | BAR
  | ARROW
  | EQUAL
  | NOT_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | PLUS
  | MINUS
  | TILDE_PLUS
  | TILDE_MINUS
  | STAR
  | SLASH
  | AND_AND
  | OR_OR
  | CONS
  | EOF
[@@deriving eq, show { with_path = false }, qcheck]

(** A token paired with its range in the original input. *)
type located =
  { token : t (** The recognized token. *)
  ; span : Location.span (** Its half-open source range. *)
  }
[@@deriving eq, show { with_path = false }]

(** [keyword_or_identifier text] recognizes reserved words and otherwise
    returns [IDENT text]. *)
let keyword_or_identifier = function
  | "let" -> LET
  | "rec" -> REC
  | "and" -> AND
  | "in" -> IN
  | "fun" -> FUN
  | "function" -> FUNCTION
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "match" -> MATCH
  | "with" -> WITH
  | "true" -> TRUE
  | "false" -> FALSE
  | "_" -> UNDERSCORE
  | identifier -> IDENT identifier
;;

(** [operator_of_lexeme text] returns the token of an explicitly supported
    operator. An unknown spelling produces [None]. *)
let operator_of_lexeme = function
  | "=" -> Some EQUAL
  | "<>" -> Some NOT_EQUAL
  | "<" -> Some LESS
  | "<=" -> Some LESS_EQUAL
  | ">" -> Some GREATER
  | ">=" -> Some GREATER_EQUAL
  | "+" -> Some PLUS
  | "-" -> Some MINUS
  | "~+" -> Some TILDE_PLUS
  | "~-" -> Some TILDE_MINUS
  | "*" -> Some STAR
  | "/" -> Some SLASH
  | "&&" -> Some AND_AND
  | "||" -> Some OR_OR
  | "::" -> Some CONS
  | "|" -> Some BAR
  | _ -> None
;;

(** Return the canonical source spelling of a token. [EOF] has an empty
    spelling. This function is also used by lexer property tests. *)
let to_lexeme = function
  | INT value -> string_of_int value
  | IDENT identifier -> identifier
  | LET -> "let"
  | REC -> "rec"
  | AND -> "and"
  | IN -> "in"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | MATCH -> "match"
  | WITH -> "with"
  | TRUE -> "true"
  | FALSE -> "false"
  | UNDERSCORE -> "_"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | COMMA -> ","
  | SEMICOLON -> ";"
  | BAR -> "|"
  | ARROW -> "->"
  | EQUAL -> "="
  | NOT_EQUAL -> "<>"
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | PLUS -> "+"
  | MINUS -> "-"
  | TILDE_PLUS -> "~+"
  | TILDE_MINUS -> "~-"
  | STAR -> "*"
  | SLASH -> "/"
  | AND_AND -> "&&"
  | OR_OR -> "||"
  | CONS -> "::"
  | EOF -> ""
;;
