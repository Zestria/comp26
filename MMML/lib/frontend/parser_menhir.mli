(** Bottom-up parsing through Menhir and ocamllex. Each entry point consumes
    the complete input and returns positioned lexical or syntax errors.

    Value identifiers start with a lowercase ASCII letter or underscore;
    reserved OCaml words cannot be used as names. Control expressions used as
    operator operands, tuple components or list elements require parentheses,
    as do signed constant function parameters. Integer literal limits are
    inherited from {!Lexer}. *)

val parse_program : string -> Ast.program Error_monad.t
val parse_expression : string -> Ast.expr Error_monad.t

(** Streaming entry points. The supplied buffer is consumed sequentially. *)
val parse_program_lexbuf : Lexing.lexbuf -> Ast.program Error_monad.t

val parse_expression_lexbuf : Lexing.lexbuf -> Ast.expr Error_monad.t
