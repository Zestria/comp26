(** Unified parsing entry point for MyLittleML (Task 1).

    Dispatches to one of the two syntax analyzers:

    - {!Parser_topdown}: the top-down (recursive-descent, Angstrom-based)
      parser, the default;
    - {!Parser} (with {!Lexer}): the bottom-up (menhir + ocamllex) parser.

    Both parse the same OCaml subset into the same {!Ast.prog} and report
    syntax errors as ["line:col: message"] on the ['Error] side.  Sticky
    keywords ([--sticky]) are only supported by the top-down parser, which
    is why {!opts} carries the flag here. *)

type parser_kind = Topdown | Bottomup

type opts = { sticky : bool }

let error_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line = p.Lexing.pos_lnum in
  let col = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Format.sprintf "%d:%d" line col
;;

let parse_bottomup source =
  let lexbuf = Lexing.from_string source in
  match Parser.prog Lexer.read lexbuf with
  | prog -> Ok prog
  | exception Lexer.SyntaxError msg ->
    Error (Format.sprintf "%s: %s" (error_position lexbuf) msg)
  | exception Parser.Error ->
    Error (Format.sprintf "%s: syntax error" (error_position lexbuf))
;;

let parse kind opts source =
  match kind with
  | Topdown -> Parser_topdown.parse_prog ~sticky:opts.sticky source
  | Bottomup ->
    if opts.sticky
    then Error "sticky mode is not supported by the bottom-up parser"
    else parse_bottomup source
;;