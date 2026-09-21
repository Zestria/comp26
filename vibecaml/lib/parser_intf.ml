(** Общий контракт для нисходящего и Menhir-парсеров. *)

module type S = sig
  (** Разобрать последовательность объявлений верхнего уровня. *)
  val parse_structure : string -> (Ast.structure, string) result

  (** Разобрать одно выражение. *)
  val parse_expr : string -> (Ast.expression, string) result
end
