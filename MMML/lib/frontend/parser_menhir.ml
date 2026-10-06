(** Entry points for the bottom-up MiniML parser. *)

exception Frontend_error of Error_monad.error

let reserved_identifier = function
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "effect"
  | "end"
  | "exception"
  | "external"
  | "for"
  | "functor"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "sig"
  | "struct"
  | "to"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while" -> true
  | _ -> false
;;

let next_token lexbuf =
  match Lexer.next_token lexbuf with
  | Error error -> raise (Frontend_error error)
  | Ok { token = IDENT name; span }
    when reserved_identifier name || (name.[0] >= 'A' && name.[0] <= 'Z') ->
    raise
      (Frontend_error
         { location = span.start
         ; message = Printf.sprintf "invalid value identifier %S" name
         })
  | Ok located -> located.token
;;

let parse entry lexbuf =
  try Ok (entry next_token lexbuf) with
  | Frontend_error error -> Error error
  | Parser.Error ->
    let location = Location.of_lexing_position (Lexing.lexeme_start_p lexbuf) in
    let message =
      match Lexing.lexeme lexbuf with
      | "" -> "unexpected end of input"
      | text -> Printf.sprintf "unexpected token %S" text
    in
    Error_monad.error location message
;;

let parse_program_lexbuf lexbuf = parse Parser.program lexbuf
let parse_expression_lexbuf lexbuf = parse Parser.expression lexbuf
let parse_program source = parse_program_lexbuf (Lexing.from_string source)
let parse_expression source = parse_expression_lexbuf (Lexing.from_string source)
