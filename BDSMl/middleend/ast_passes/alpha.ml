[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast

module Names = Set.Make (String)
module Environment = Map.Make (String)

let add_pattern names = function
  | PVar name -> Names.add name names
  | PUnit | PUnderscore -> names
;;

let rec expression_names names = function
  | Unit | Int _ | Bool _ -> names
  | Var name -> Names.add name names
  | BinOp (_, left, right) | App (left, right) ->
    expression_names (expression_names names left) right
  | UnOp (_, operand) -> expression_names names operand
  | If (condition, if_true, if_false) ->
    expression_names
      (expression_names (expression_names names condition) if_true)
      if_false
  | Let (_, pattern, value, body) ->
    expression_names (expression_names (add_pattern names pattern) value) body
  | Abs (pattern, body) -> expression_names (add_pattern names pattern) body
;;

let program_names program =
  List.fold_left
    (fun names -> function
      | StrLet (_, pattern, value) -> expression_names (add_pattern names pattern) value
      | StrEval expression -> expression_names names expression)
    Names.empty
    program
;;

let alpha program =
  let ( let* ) = Fresh.bind in
  let return = Fresh.return in
  let rename_pattern environment = function
    | PVar name ->
      let* renamed = Fresh.fresh_ident name in
      return (PVar renamed, Environment.add name renamed environment)
    | (PUnit | PUnderscore) as pattern -> return (pattern, environment)
  in
  let rec rename_expression environment = function
    | (Unit | Int _ | Bool _) as expression -> return expression
    | Var name ->
      return (Var (Option.value (Environment.find_opt name environment) ~default:name))
    | BinOp (operator, left, right) ->
      let* right = rename_expression environment right in
      let* left = rename_expression environment left in
      return (BinOp (operator, left, right))
    | UnOp (operator, operand) ->
      let* operand = rename_expression environment operand in
      return (UnOp (operator, operand))
    | If (condition, if_true, if_false) ->
      let* if_false = rename_expression environment if_false in
      let* if_true = rename_expression environment if_true in
      let* condition = rename_expression environment condition in
      return (If (condition, if_true, if_false))
    | App (function_, argument) ->
      let* argument = rename_expression environment argument in
      let* function_ = rename_expression environment function_ in
      return (App (function_, argument))
    | Abs (pattern, body) ->
      let* pattern, body_environment = rename_pattern environment pattern in
      let* body = rename_expression body_environment body in
      return (Abs (pattern, body))
    | Let (rec_flag, pattern, value, body) ->
      let* pattern, body_environment = rename_pattern environment pattern in
      let value_environment =
        match rec_flag with
        | Rec -> body_environment
        | NonRec -> environment
      in
      let* body = rename_expression body_environment body in
      let* value = rename_expression value_environment value in
      return (Let (rec_flag, pattern, value, body))
  in
  let rec rename_program environment = function
    | [] -> return []
    | StrEval expression :: rest ->
      let* rest = rename_program environment rest in
      let* expression = rename_expression environment expression in
      return (StrEval expression :: rest)
    | StrLet (rec_flag, pattern, value) :: rest ->
      let* pattern, next_environment = rename_pattern environment pattern in
      let value_environment =
        match rec_flag with
        | Rec -> next_environment
        | NonRec -> environment
      in
      let* rest = rename_program next_environment rest in
      let* value = rename_expression value_environment value in
      return (StrLet (rec_flag, pattern, value) :: rest)
  in
  let state = Fresh.create (Names.elements (program_names program)) in
  fst (rename_program Environment.empty program state)
;;
