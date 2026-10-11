[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend
open BDSMl_frontend.Ast
open Anf_ast
module Names = Set.Make (String)

let ( let* ) = Result.bind

let rec source_names names = function
  | Ast.Unit | Ast.Int _ | Ast.Bool _ -> names
  | Ast.Var name -> Names.add name names
  | Ast.BinOp (_, left, right) | Ast.App (left, right) ->
    source_names (source_names names left) right
  | Ast.UnOp (_, operand) -> source_names names operand
  | Ast.If (condition, if_true, if_false) ->
    source_names (source_names (source_names names condition) if_true) if_false
  | Ast.Let (_, pattern, value, body) ->
    source_names (source_names (pattern_name names pattern) value) body
  | Ast.Abs (pattern, body) -> source_names (pattern_name names pattern) body

and pattern_name names = function
  | Ast.PVar name -> Names.add name names
  | Ast.PUnit | Ast.PUnderscore -> names
;;

let reserved = ref Names.empty

let fresh_anf () =
  let rec choose () =
    let name = Fresh.fresh_ident "a" in
    if Names.mem name !reserved then choose () else name
  in
  let name = choose () in
  reserved := Names.add name !reserved;
  name
;;

let flatten_app (e : Ast.expr) : Ast.expr * Ast.expr list =
  let rec go e acc =
    match e with
    | Ast.App (e1, e2) -> go e1 (e2 :: acc)
    | _ -> e, acc
  in
  go e []
;;

let split_params (e : Ast.expr) : Ast.pattern list * Ast.expr =
  let rec go acc e =
    match e with
    | Ast.Abs (p, e) -> go (p :: acc) e
    | _ -> List.rev acc, e
  in
  go [] e
;;

let rec to_imm (e : Ast.expr) (k : immexpr -> (aexpr, string) result) : (aexpr, string) result =
  match e with
  | Ast.Int n -> k (ImmInt n)
  | Ast.Bool b -> k (ImmBool b)
  | Ast.Unit -> k ImmUnit
  | Ast.Var x -> k (ImmId x)
  | _ ->
    to_cexpr e (function
      | CImm imm -> k imm
      | ce ->
        let name = fresh_anf () in
        let* body = k (ImmId name) in
        Ok (ALet (Ast.PVar name, ce, body)))

and to_imm_list (es : Ast.expr list) (k : immexpr list -> (aexpr, string) result) : (aexpr, string) result =
  match es with
  | [] -> k []
  | e :: tl -> to_imm_list tl (fun imms -> to_imm e (fun imm -> k (imm :: imms)))

and to_cexpr (e : Ast.expr) (k : cexpr -> (aexpr, string) result) : (aexpr, string) result =
  match e with
  | Ast.Unit | Ast.Int _ | Ast.Bool _ | Ast.Var _ -> to_imm e (fun imm -> k (CImm imm))
  | Ast.BinOp (And, a, b) -> to_cexpr (Ast.If (a, b, Ast.Bool false)) k
  | Ast.BinOp (Or, a, b) -> to_cexpr (Ast.If (a, Ast.Bool true, b)) k
  | Ast.BinOp (op, e1, e2) ->
    to_imm e2 (fun b -> to_imm e1 (fun a -> k (CBinOp (op, a, b))))
  | Ast.UnOp (op, e1) -> to_imm e1 (fun a -> k (CUnOp (op, a)))
  | Ast.App _ ->
    let f, args = flatten_app e in
    to_imm_list args (fun args_imm -> to_imm f (fun f_imm -> k (CApp (f_imm, args_imm))))
  | Ast.If (c, e1, e2) ->
    to_imm c (fun c_imm ->
      let* if_true = to_aexpr e1 in
      let* if_false = to_aexpr e2 in
      k (CIf (c_imm, if_true, if_false)))
  | Ast.Let (Rec, _, _, _) ->
    Error "ANF: локальный let rec должен быть убран до ANF"
  | Ast.Let (NonRec, p, e1, e2) -> to_cexpr e1 (fun ce ->
      let* body = to_cexpr e2 k in
      Ok (ALet (p, ce, body)))
  | Ast.Abs _ -> Error "ANF: лямбда должна быть убрана до ANF"

and to_aexpr (e : Ast.expr) : (aexpr, string) result =
  to_cexpr e (fun ce -> Ok (ACExpr ce))

let substitute_imm name replacement = function
  | ImmId current when current = name -> replacement
  | imm -> imm
;;

let rec bound_in_aexpr name = function
  | ACExpr expression -> bound_in_cexpr name expression
  | ALet (pattern, expression, body) ->
    (match pattern with
     | PVar bound when bound = name -> true
     | _ -> bound_in_cexpr name expression || bound_in_aexpr name body)

and bound_in_cexpr name = function
  | CIf (_, if_true, if_false) ->
    bound_in_aexpr name if_true || bound_in_aexpr name if_false
  | CImm _ | CBinOp _ | CUnOp _ | CApp _ -> false
;;

let rec substitute_aexpr name replacement = function
  | ACExpr expression -> ACExpr (substitute_cexpr name replacement expression)
  | ALet (pattern, expression, body) ->
    let expression = substitute_cexpr name replacement expression in
    let body =
      match pattern with
      | PVar bound when bound = name -> body
      | _ -> substitute_aexpr name replacement body
    in
    ALet (pattern, expression, body)

and substitute_cexpr name replacement = function
  | CImm imm -> CImm (substitute_imm name replacement imm)
  | CBinOp (operator, left, right) ->
    CBinOp
      ( operator
      , substitute_imm name replacement left
      , substitute_imm name replacement right )
  | CUnOp (operator, operand) -> CUnOp (operator, substitute_imm name replacement operand)
  | CApp (function_, arguments) ->
    CApp
      ( substitute_imm name replacement function_
      , List.map (substitute_imm name replacement) arguments )
  | CIf (condition, if_true, if_false) ->
    CIf
      ( substitute_imm name replacement condition
      , substitute_aexpr name replacement if_true
      , substitute_aexpr name replacement if_false )
;;

let rec compact_aexpr = function
  | ACExpr expression -> ACExpr (compact_cexpr expression)
  | ALet (pattern, expression, body) ->
    let expression = compact_cexpr expression in
    let body = compact_aexpr body in
    (match pattern, expression, body with
     | PVar name, CImm replacement, _
       when match replacement with
            | ImmId source -> not (bound_in_aexpr source body)
            | ImmInt _ | ImmBool _ | ImmUnit -> true ->
       compact_aexpr (substitute_aexpr name replacement body)
     | PUnderscore, CImm _, _ | PUnit, CImm ImmUnit, _ -> body
     | ( PVar name
       , CApp (function_, arguments)
       , ACExpr (CApp (ImmId applied, more_arguments)) )
       when name = applied -> ACExpr (CApp (function_, arguments @ more_arguments))
     | PVar name, expression, ACExpr (CImm (ImmId result)) when name = result ->
       ACExpr expression
     | _ -> ALet (pattern, expression, body))

and compact_cexpr = function
  | CIf (condition, if_true, if_false) ->
    CIf (condition, compact_aexpr if_true, compact_aexpr if_false)
  | expression -> expression
;;

let anf_decl : Ast.structure_item -> (decl, string) result = function
  | StrEval e ->
    let* body = to_aexpr e in
    Ok (AEval (compact_aexpr body))
  | StrLet (r, p, e) ->
    let params, body = split_params e in
    let* body = to_aexpr body in
    Ok (ADecl (r, p, params, compact_aexpr body))
;;

let anf (program : Ast.program) : (aprogram, string) result =
  Fresh.reset ();
  reserved
  := List.fold_left
       (fun names -> function
          | StrEval expression -> source_names names expression
          | StrLet (_, pattern, expression) ->
            source_names (pattern_name names pattern) expression)
       Names.empty
       program;
  let rec go prog acc =
    match prog with
    | p :: tl ->
      let* declaration = anf_decl p in
      go tl (declaration :: acc)
    | [] -> Ok (List.rev acc)
  in
  go program []
;;
