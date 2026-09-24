open Frontend.Common
open Frontend.Lexer

type error = Frontend.Lexer.error

let parse_expr (src : string) : (expr, unit) result =
  try
    match Parser_gen.program Frontend.Lexer.token (Lexing.from_string src) with
    | [ StrEval e ] -> Ok e
    | _ -> Error ()
  with
  | Parser_gen.Error -> Error ()
  | Frontend.Lexer.Lexing_error e -> Error ()
;;
