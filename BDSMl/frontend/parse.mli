(** Parse a source string, returning lexical and syntax errors as [Error] messages. *)
val parse : string -> (Ast.program, string) result
