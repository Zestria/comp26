[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend
open Anf_ast

(** Распрямляет цепочку применений:
    [App (App (App (f, a), b), c)] ~> [(f, [a; b; c])]. *)
let flatten_app (e : Ast.expr) : Ast.expr * Ast.expr list =
  failwith "Anf_conv.flatten_app: not implemented"
;;

(** Отделяет параметры функции от тела:
    [Abs (p1, Abs (p2, body))] ~> [([p1; p2], body)].
    Для не-функции возвращает [([], e)]. *)
let split_params (e : Ast.expr) : Ast.pattern list * Ast.expr =
  failwith "Anf_conv.split_params: not implemented"
;;

(** Переводит [e] и передаёт в [k] атом с его результатом.
    Если [e] не атом, заводит для него новое имя через [Fresh]. *)
let rec to_imm (e : Ast.expr) (k : immexpr -> aexpr) : aexpr =
  failwith "Anf_conv.to_imm: not implemented"

(** [to_imm] для списка: переводит выражения слева направо и передаёт
    в [k] список атомов. *)
and to_imm_list (es : Ast.expr list) (k : immexpr list -> aexpr) : aexpr =
  failwith "Anf_conv.to_imm_list: not implemented"

(** Переводит [e] и передаёт в [k] последнее действие, не заводя для него имя. *)
and to_cexpr (e : Ast.expr) (k : cexpr -> aexpr) : aexpr =
  failwith "Anf_conv.to_cexpr: not implemented"

(** Переводит [e] целиком: [to_cexpr e (fun c -> ACExpr c)]. *)
and to_aexpr (e : Ast.expr) : aexpr = failwith "Anf_conv.to_aexpr: not implemented"

(** Переводит одно определение верхнего уровня. *)
let anf_decl (item : Ast.structure_item) : decl =
  failwith "Anf_conv.anf_decl: not implemented"
;;

(** Переводит всю программу. Не забыть [Fresh.reset ()] в начале. *)
let anf (program : Ast.program) : aprogram = failwith "Anf_conv.anf: not implemented"
