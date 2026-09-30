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

    AST печатается через [Printer], ANF --- через [Anf.Anf_pp]. *)
val run : stop_after:stage -> Ast.program -> string
