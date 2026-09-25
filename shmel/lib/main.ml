open Ast

let parse_with_menhir s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let parse_with_comb s = Parser_comb.parse s
let parse_with_comb_extended s = Parser_comb.parse_extended s
