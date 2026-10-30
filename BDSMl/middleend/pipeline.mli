[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

(** Стадия, после которой компилятор останавливается и печатает результат. *)
type stage =
  | Parsing
  | Alpha
  | Closure_conv
  | Lambda_lift
  | Anf

(** [run ~stop_after program] прогоняет [program] через проходы по порядку
    (alpha, closure conversion, lambda lifting, ANF) до стадии [stop_after]
    включительно и возвращает её результат в виде текста.

    AST печатается через [Printer], ANF --- через [Anf.Anf_pp].
    Перед closure conversion нормализует рекурсивные инициализаторы.
    Возвращает [Error] для неподдерживаемых рекурсивных значений или
    если программа нарушает требования входа ANF. *)
val run : stop_after:stage -> Ast.program -> (string, string) result

(** [to_anf program] выполняет alpha conversion, нормализацию рекурсивных
    инициализаторов, closure conversion, lambda lifting и ANF conversion.
    Возвращает структуру ANF без печати для передачи в backend.
    Использует тот же конвейер, что [run ~stop_after:Anf].
    Возвращает [Error] для неподдерживаемых рекурсивных значений или
    при нарушении требований входа ANF. *)
val to_anf : Ast.program -> (Anf.Anf_ast.aprogram, string) result
