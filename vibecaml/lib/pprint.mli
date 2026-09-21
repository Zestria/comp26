(** Печать AST в синтаксис подмножества OCaml. *)

val pp_expression : Format.formatter -> Ast.expression -> unit
val pp_structure : Format.formatter -> Ast.structure -> unit

(** Получить строковое представление выражения. *)
val string_of_expression : Ast.expression -> string

val string_of_structure : Ast.structure -> string
