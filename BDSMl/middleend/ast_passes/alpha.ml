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
  Fresh.reset ();
  let used = ref (program_names program) in
  let fresh name =
    let rec choose () =
      let candidate = Fresh.fresh_ident name in
      if Names.mem candidate !used then choose () else candidate
    in
    let candidate = choose () in
    used := Names.add candidate !used;
    candidate
  in
  let rename_pattern environment = function
    | PVar name ->
      let renamed = fresh name in
      PVar renamed, Environment.add name renamed environment
    | (PUnit | PUnderscore) as pattern -> pattern, environment
  in
  let rec rename_expression environment = function
    | (Unit | Int _ | Bool _) as expression -> expression
    | Var name ->
      Var (Option.value (Environment.find_opt name environment) ~default:name)
    | BinOp (operator, left, right) ->
      BinOp
        ( operator
        , rename_expression environment left
        , rename_expression environment right )
    | UnOp (operator, operand) -> UnOp (operator, rename_expression environment operand)
    | If (condition, if_true, if_false) ->
      If
        ( rename_expression environment condition
        , rename_expression environment if_true
        , rename_expression environment if_false )
    | App (function_, argument) ->
      App
        (rename_expression environment function_, rename_expression environment argument)
    | Abs (pattern, body) ->
      let pattern, body_environment = rename_pattern environment pattern in
      Abs (pattern, rename_expression body_environment body)
    | Let (rec_flag, pattern, value, body) ->
      let pattern, body_environment = rename_pattern environment pattern in
      let value_environment =
        match rec_flag with
        | Rec -> body_environment
        | NonRec -> environment
      in
      Let
        ( rec_flag
        , pattern
        , rename_expression value_environment value
        , rename_expression body_environment body )
  in
  let rec rename_program environment = function
    | [] -> []
    | StrEval expression :: rest ->
      StrEval (rename_expression environment expression) :: rename_program environment rest
    | StrLet (rec_flag, pattern, value) :: rest ->
      let pattern, next_environment = rename_pattern environment pattern in
      let value_environment =
        match rec_flag with
        | Rec -> next_environment
        | NonRec -> environment
      in
      StrLet (rec_flag, pattern, rename_expression value_environment value)
      :: rename_program next_environment rest
  in
  rename_program Environment.empty program
;;
