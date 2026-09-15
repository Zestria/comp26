let parse source =
  let lexbuf = Lexing.from_string source in
  try Ok (Parser.program Lexer_LR.token lexbuf) with
  | Lexer_LR.Error message -> Error message
  | Parser.Error ->
    let position = Lexing.lexeme_start_p lexbuf in
    Error
      (Printf.sprintf
         "Line %d, column %d: Syntax error"
         position.Lexing.pos_lnum
         (position.Lexing.pos_cnum - position.Lexing.pos_bol + 1))
;;
