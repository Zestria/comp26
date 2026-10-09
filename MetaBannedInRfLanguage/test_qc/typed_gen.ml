[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Gen = QCheck.Gen

type ty =
  | TInt
  | TBool
  | TUnit
  | TFun of ty * ty
  | TTuple of ty * ty * ty list

type ctx = (string * ty) list

let rec gen_type n : ty Gen.t =
  if n <= 0
  then Gen.oneof [ Gen.return TInt; Gen.return TBool; Gen.return TUnit ]
  else
    Gen.oneof
      [ Gen.return TInt
      ; Gen.return TBool
      ; Gen.return TUnit
      ; Gen.map2 (fun a b -> TFun (a, b)) (gen_type (n / 2)) (gen_type (n / 2))
      ; Gen.map3
          (fun a b c -> TTuple (a, b, c))
          (gen_type (n / 2))
          (gen_type (n / 2))
          (Gen.list_size (Gen.int_range 0 1) (gen_type (n / 2)))
      ]
;;

let rec default_expr = function
  | TInt -> Ast.EConst (Ast.CInt 0)
  | TBool -> Ast.EConst (Ast.CBool false)
  | TUnit -> Ast.EConst Ast.CUnit
  | TFun (_, t2) -> Ast.EFun (Ast.PAny, default_expr t2)
  | TTuple (t1, t2, rest) -> Ast.ETuple (default_expr t1, default_expr t2, List.map default_expr rest)
;;

let vars_of_type (ctx : ctx) ty =
  List.filter_map (fun (name, t) -> if t = ty then Some name else None) ctx
;;

let funs_returning (ctx : ctx) ty =
  List.filter_map
    (fun (name, t) ->
       match t with
       | TFun (targ, tret) when tret = ty -> Some (name, targ)
       | _ -> None)
    ctx
;;

let fresh_name (ctx : ctx) = Printf.sprintf "v%d" (List.length ctx)

let rec gen_expr (ctx : ctx) ty n : Ast.expression Gen.t =
  fun st ->
  let leaves =
    match ty with
    | TInt -> [ Gen.map (fun i -> Ast.EConst (Ast.CInt i)) (Gen.int_range 0 20) ]
    | TBool -> [ Gen.map (fun b -> Ast.EConst (Ast.CBool b)) Gen.bool ]
    | TUnit -> [ Gen.return (Ast.EConst Ast.CUnit) ]
    | TFun _ | TTuple _ -> []
  in
  let vars = List.map (fun name -> Gen.return (Ast.EIdent name)) (vars_of_type ctx ty) in
  let applications =
    List.map
      (fun (name, targ) ->
         Gen.map (fun arg -> Ast.EApply (Ast.EIdent name, arg)) (gen_expr ctx targ (n / 2)))
      (funs_returning ctx ty)
  in
  let if_gen =
    Gen.map3
      (fun cond e1 e2 -> Ast.EIf (cond, e1, Some e2))
      (gen_expr ctx TBool (n / 2))
      (gen_expr ctx ty (n / 2))
      (gen_expr ctx ty (n / 2))
  in
  let let_gen : Ast.expression Gen.t =
    fun st ->
    let t_bound = gen_type 1 st in
    let name = fresh_name ctx in
    let bound_expr = gen_expr ctx t_bound (n / 2) st in
    let body = gen_expr ((name, t_bound) :: ctx) ty (n / 2) st in
    Ast.ELet (Ast.NonRec, { Ast.vb_pat = Ast.PVar name; vb_expr = bound_expr }, [], body)
  in
  let type_specific =
    match ty with
    | TInt ->
      [ Gen.map3
          (fun op a b -> Ast.EApply (Ast.EApply (Ast.EIdent op, a), b))
          (Gen.oneof_list [ "+"; "-"; "*" ])
          (gen_expr ctx TInt (n / 2))
          (gen_expr ctx TInt (n / 2))
      ]
    | TBool ->
      [ Gen.map3
          (fun op a b -> Ast.EApply (Ast.EApply (Ast.EIdent op, a), b))
          (Gen.oneof_list [ "<"; "<="; ">"; ">="; "="; "<>" ])
          (gen_expr ctx TInt (n / 2))
          (gen_expr ctx TInt (n / 2))
      ; Gen.map3
          (fun op a b -> Ast.EApply (Ast.EApply (Ast.EIdent op, a), b))
          (Gen.oneof_list [ "&&"; "||" ])
          (gen_expr ctx TBool (n / 2))
          (gen_expr ctx TBool (n / 2))
      ; Gen.map (fun b -> Ast.EApply (Ast.EIdent "not", b)) (gen_expr ctx TBool (n / 2))
      ]
    | TUnit -> []
    | TFun (t1, t2) ->
      let param = fresh_name ctx in
      let plain =
        Gen.map (fun body -> Ast.EFun (Ast.PVar param, body)) (gen_expr ((param, t1) :: ctx) t2 (n / 2))
      in
      let recursive : Ast.expression Gen.t =
        fun st ->
        let rec_name = fresh_name ctx in
        let rec_ctx = (rec_name, TFun (t1, t2)) :: ctx in
        let param = fresh_name rec_ctx in
        let body = gen_expr ((param, t1) :: rec_ctx) t2 (n / 2) st in
        Ast.ELet
          ( Ast.Rec
          , { Ast.vb_pat = Ast.PVar rec_name; vb_expr = Ast.EFun (Ast.PVar param, body) }
          , []
          , Ast.EIdent rec_name )
      in
      [ plain; recursive ]
    | TTuple (t1, t2, rest) ->
      [ Gen.map3
          (fun e1 e2 erest -> Ast.ETuple (e1, e2, erest))
          (gen_expr ctx t1 (n / 2))
          (gen_expr ctx t2 (n / 2))
          (Gen.flatten_list (List.map (fun t -> gen_expr ctx t (n / 2)) rest))
      ]
  in
  let candidates =
    leaves @ vars @ if (n <= 0) then [] else applications @ [ if_gen; let_gen ] @ type_specific
  in
  match candidates with
  | [] -> default_expr ty
  | _ -> Gen.oneof candidates st
;;

let gen_structure_item (ctx : ctx) name ty n : Ast.structure_item Gen.t =
  fun st ->
  match ty with
  | TFun (t1, t2) when Gen.bool st ->
    let rec_ctx = (name, ty) :: ctx in
    let param = fresh_name rec_ctx in
    let body = gen_expr ((param, t1) :: rec_ctx) t2 n st in
    Ast.SValue (Ast.Rec, { Ast.vb_pat = Ast.PVar name; vb_expr = Ast.EFun (Ast.PVar param, body) }, [])
  | _ ->
    let expr = gen_expr ctx ty n st in
    Ast.SValue (Ast.NonRec, { Ast.vb_pat = Ast.PVar name; vb_expr = expr }, [])
;;

let gen_structure : Ast.structure Gen.t =
  fun st ->
  let count = Gen.int_range 1 5 st in
  let rec go (ctx : ctx) i n =
    if n <= 0
    then []
    else (
      let ty = gen_type 2 st in
      let name = Printf.sprintf "top%d" i in
      let item = gen_structure_item ctx name ty 6 st in
      item :: go ((name, ty) :: ctx) (i + 1) (n - 1))
  in
  go [] 0 count
;;

let arb_structure = QCheck.make ~print:Ast.show_structure gen_structure
