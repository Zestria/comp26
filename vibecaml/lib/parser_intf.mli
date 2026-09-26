[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Общий контракт для нисходящего и Menhir-парсеров. *)

(** Расширения синтаксиса, по умолчанию выключены. Парсер сам решает, что поддерживает: включённое
    расширение, которое он не реализует, --- [Error]. *)
type extensions =
  { glued_keywords : bool
    (** идентификаторы, слипшиеся с ключевыми словами, [--ext-glued-keywords] *)
  ; operator_declarations : bool
    (** переопределение арности и ассоциативности операторов,
        [--ext-operator-declarations] *)
  }

module type S = sig
  (** Разобрать последовательность объявлений верхнего уровня. *)
  val parse_structure : extensions -> string -> (Ast.structure, string) result

  (** Разобрать одно выражение. *)
  val parse_expr : extensions -> string -> (Ast.expression, string) result
end
