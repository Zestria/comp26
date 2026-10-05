[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend
open BDSMl_frontend.Ast
open Anf_ast

let flatten_app (e : Ast.expr) : Ast.expr * Ast.expr list =
  let rec go e acc =
    match e with
    | Ast.App (e1, e2) -> go e1 (e2 :: acc)
    | _ -> e, acc
  in
  go e []
;;

(** Отделяет параметры функции от тела:
    [Abs (p1, Abs (p2, body))] ~> [([p1; p2], body)].
    Для не-функции возвращает [([], e)]. *)
let split_params (e : Ast.expr) : Ast.pattern list * Ast.expr =
  let rec go acc e =
    match e with
    | Ast.Abs (p, e) -> go (p :: acc) e
    | _ -> List.rev acc, e
  in
  go [] e
;;

(** Переводит [e] и передаёт в [k] атом с его результатом.
    Если [e] не атом, заводит для него новое имя через [Fresh]. *)
let rec to_imm (e : Ast.expr) (k : immexpr -> aexpr) : aexpr =
  match e with
  | Ast.Int n -> k (ImmInt n)
  | Ast.Bool b -> k (ImmBool b)
  | Ast.Unit -> k ImmUnit
  | Ast.Var x -> k (ImmId x)
  | _ ->
    to_cexpr e (fun ce ->
      let name = Fresh.fresh_ident "a" in
      ALet (Ast.PVar name, ce, k (ImmId name)))

(** [to_imm] для списка: переводит выражения слева направо и передаёт
    в [k] список атомов. *)
and to_imm_list (es : Ast.expr list) (k : immexpr list -> aexpr) : aexpr =
  match es with
  | [] -> k []
  | e :: tl -> to_imm e (fun imm -> to_imm_list tl (fun imms -> k (imm :: imms)))

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
