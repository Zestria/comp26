[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast

let ( let* ) = Result.bind

let uses name expression =
  Free_vars.String_set.mem name (Free_vars.free_vars expression)
;;

(* Alpha conversion precedes this pass, so moving a binding cannot capture
   another variable. Initializers stay outside the function and run once. *)
let rec split_initializer name = function
  | Let (flag, pattern, value, body) when not (uses name value) ->
    let* prefixes, body = split_initializer name body in
    Ok ((flag, pattern, value) :: prefixes, body)
  | Abs _ as value -> Ok ([], (Rec, value))
  | value when not (uses name value) -> Ok ([], (NonRec, value))
  | _ ->
    Error (Printf.sprintf "Recursive initializer for %s is not supported" name)
;;

let rec expression = function
  | (Unit | Int _ | Bool _ | Var _) as value -> Ok value
  | BinOp (operator, left, right) ->
    let* left = expression left in
    let* right = expression right in
    Ok (BinOp (operator, left, right))
  | UnOp (operator, operand) ->
    let* operand = expression operand in
    Ok (UnOp (operator, operand))
  | If (condition, if_true, if_false) ->
    let* condition = expression condition in
    let* if_true = expression if_true in
    let* if_false = expression if_false in
    Ok (If (condition, if_true, if_false))
  | App (function_, argument) ->
    let* function_ = expression function_ in
    let* argument = expression argument in
    Ok (App (function_, argument))
  | Abs (parameter, body) ->
    let* body = expression body in
    Ok (Abs (parameter, body))
  | Let (Rec, PVar name, value, body) ->
    let* value = expression value in
    let* body = expression body in
    let* prefixes, (flag, value) = split_initializer name value in
    Ok (List.fold_right
          (fun (flag, pattern, value) body -> Let (flag, pattern, value, body))
          prefixes (Let (flag, PVar name, value, body)))
  | Let (NonRec, pattern, value, body) ->
    let* value = expression value in
    let* body = expression body in
    Ok (Let (NonRec, pattern, value, body))
  | Let (Rec, (PUnit | PUnderscore), _, _) ->
    Error "A recursive binding must bind a variable"
;;

let rec run = function
  | [] -> Ok []
  | StrEval value :: rest ->
    let* value = expression value in
    let* rest = run rest in
    Ok (StrEval value :: rest)
  | StrLet (Rec, PVar name, value) :: rest ->
    let* value = expression value in
    let* prefixes, (flag, value) = split_initializer name value in
    let* rest = run rest in
    let prefixes = List.map (fun (flag, pattern, value) -> StrLet (flag, pattern, value)) prefixes in
    Ok (prefixes @ (StrLet (flag, PVar name, value) :: rest))
  | StrLet (NonRec, pattern, value) :: rest ->
    let* value = expression value in
    let* rest = run rest in
    Ok (StrLet (NonRec, pattern, value) :: rest)
  | StrLet (Rec, (PUnit | PUnderscore), _) :: _ ->
    Error "A recursive binding must bind a variable"
;;
