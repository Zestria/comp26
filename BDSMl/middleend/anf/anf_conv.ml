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
    to_cexpr e (function
      | CImm imm -> k imm
      | ce ->
        let name = Fresh.fresh_ident "a" in
        ALet (Ast.PVar name, ce, k (ImmId name)))

(** [to_imm] для списка: переводит выражения слева направо и передаёт
    в [k] список атомов. *)
and to_imm_list (es : Ast.expr list) (k : immexpr list -> aexpr) : aexpr =
  match es with
  | [] -> k []
  | e :: tl -> to_imm e (fun imm -> to_imm_list tl (fun imms -> k (imm :: imms)))

and to_cexpr (e : Ast.expr) (k : cexpr -> aexpr) : aexpr =
  match e with
  | Ast.Unit | Ast.Int _ | Ast.Bool _ | Ast.Var _ -> to_imm e (fun imm -> k (CImm imm))
  | Ast.BinOp (And, a, b) -> to_cexpr (Ast.If (a, b, Ast.Bool false)) k
  | Ast.BinOp (Or, a, b) -> to_cexpr (Ast.If (a, Ast.Bool true, b)) k
  | Ast.BinOp (op, e1, e2) ->
    to_imm e1 (fun a -> to_imm e2 (fun b -> k (CBinOp (op, a, b))))
  | Ast.UnOp (op, e1) -> to_imm e1 (fun a -> k (CUnOp (op, a)))
  | Ast.App _ ->
    let f, args = flatten_app e in
    to_imm f (fun f_imm -> to_imm_list args (fun args_imm -> k (CApp (f_imm, args_imm))))
  | Ast.If (c, e1, e2) ->
    to_imm c (fun c_imm -> k (CIf (c_imm, to_aexpr e1, to_aexpr e2)))
  | Ast.Let (Rec, _, _, _) ->
    failwith "to_cexpr: локальный let rec должен быть убран до ANF"
  | Ast.Let (NonRec, p, e1, e2) -> to_cexpr e1 (fun ce -> ALet (p, ce, to_cexpr e2 k))
  | Ast.Abs _ -> failwith "to_cexpr: лямбда должна быть убрана до ANF"

(** Переводит [e] целиком: [to_cexpr e (fun c -> ACExpr c)]. *)
and to_aexpr (e : Ast.expr) : aexpr = to_cexpr e (fun ce -> ACExpr ce)

(** Переводит одно определение верхнего уровня. *)
let anf_decl (item : Ast.structure_item) : decl =
  match item with
  | StrEval e -> AEval (to_aexpr e)
  | StrLet (r, p, e) ->
    let params, body = split_params e in
    ADecl (r, p, params, to_aexpr body)
;;

(** Переводит всю программу. Не забыть [Fresh.reset ()] в начале. *)
let anf (program : Ast.program) : aprogram =
  Fresh.reset ();
  let rec go prog acc =
    match prog with
    | p :: tl -> go tl (anf_decl p :: acc)
    | _ -> List.rev acc
  in
  go program []
;;
