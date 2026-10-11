[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast

module Names = Free_vars.String_set

let bound_name = function
  | PVar name -> Some name
  | PUnit | PUnderscore -> None
;;

let rec all_names = function
  | Unit | Int _ | Bool _ -> Names.empty
  | Var name -> Names.singleton name
  | BinOp (_, left, right) | App (left, right) ->
    Names.union (all_names left) (all_names right)
  | UnOp (_, operand) -> all_names operand
  | If (condition, if_true, if_false) ->
    Names.union
      (all_names condition)
      (Names.union (all_names if_true) (all_names if_false))
  | Let (_, pattern, value, body) ->
    let names = Names.union (all_names value) (all_names body) in
    (match bound_name pattern with
     | None -> names
     | Some name -> Names.add name names)
  | Abs (pattern, body) ->
    let names = all_names body in
    (match bound_name pattern with
     | None -> names
     | Some name -> Names.add name names)
;;

let program_names program =
  List.fold_left
    (fun names -> function
      | StrEval expression -> Names.union names (all_names expression)
      | StrLet (_, pattern, value) ->
        let names = Names.union names (all_names value) in
        (match bound_name pattern with
         | None -> names
         | Some name -> Names.add name names))
    Names.empty
    program
;;

let split_abstractions expression =
  let rec go parameters = function
    | Abs (parameter, body) -> go (parameter :: parameters) body
    | body -> List.rev parameters, body
  in
  go [] expression
;;

let abstractions parameters body =
  List.fold_right (fun parameter body -> Abs (parameter, body)) parameters body
;;

let apply function_ arguments =
  List.fold_left (fun function_ argument -> App (function_, argument)) function_ arguments
;;

(* Alpha conversion normally precedes this pass. Respect binders here as well:
   closure conversion can introduce a parameter with an existing name. *)
let rec replace_free old_name new_name expression =
  let replace = replace_free old_name new_name in
  match expression with
  | Unit | Int _ | Bool _ -> expression
  | Var name -> Var (if name = old_name then new_name else name)
  | BinOp (operator, left, right) -> BinOp (operator, replace left, replace right)
  | UnOp (operator, operand) -> UnOp (operator, replace operand)
  | If (condition, if_true, if_false) ->
    If (replace condition, replace if_true, replace if_false)
  | App (function_, argument) -> App (replace function_, replace argument)
  | Abs (pattern, body) ->
    Abs
      ( pattern
      , if bound_name pattern = Some old_name then body else replace body )
  | Let (rec_flag, pattern, value, body) ->
    let shadows = bound_name pattern = Some old_name in
    let value = if shadows && rec_flag = Rec then value else replace value in
    Let (rec_flag, pattern, value, if shadows then body else replace body)
;;

let builtin_names =
  Names.of_list
    [ "print_int"; "print_endline"; "print_string"; "string_of_int"; "int_of_string" ]
;;

let lambda_lift program =
  let ( let* ) = Fresh.bind in
  let return = Fresh.return in
  let rec lift_expression available = function
    | (Unit | Int _ | Bool _ | Var _) as expression -> return (expression, [])
    | BinOp (operator, left, right) ->
      let* left, left_lifts = lift_expression available left in
      let* right, right_lifts = lift_expression available right in
      return (BinOp (operator, left, right), left_lifts @ right_lifts)
    | UnOp (operator, operand) ->
      let* operand, lifts = lift_expression available operand in
      return (UnOp (operator, operand), lifts)
    | If (condition, if_true, if_false) ->
      let* condition, condition_lifts = lift_expression available condition in
      let* if_true, true_lifts = lift_expression available if_true in
      let* if_false, false_lifts = lift_expression available if_false in
      return (If (condition, if_true, if_false), condition_lifts @ true_lifts @ false_lifts)
    | App (function_, argument) ->
      let* function_, function_lifts = lift_expression available function_ in
      let* argument, argument_lifts = lift_expression available argument in
      return (App (function_, argument), function_lifts @ argument_lifts)
    | Abs _ as abstraction -> lift_abstraction available None abstraction
    | Let (Rec, PVar name, (Abs _ as value), body) ->
      let* value, value_lifts = lift_abstraction available (Some name) value in
      let* body, body_lifts = lift_expression available body in
      return (Let (NonRec, PVar name, value, body), value_lifts @ body_lifts)
    | Let (rec_flag, pattern, value, body) ->
      let* value, value_lifts = lift_expression available value in
      let* body, body_lifts = lift_expression available body in
      return (Let (rec_flag, pattern, value, body), value_lifts @ body_lifts)

  and lift_abstraction available recursive_name abstraction =
    let* lifted_name = Fresh.fresh_ident "lifted" in
    let abstraction =
      match recursive_name with
      | None -> abstraction
      | Some name -> replace_free name lifted_name abstraction
    in
    let captures =
      Free_vars.free_vars abstraction
      |> Names.remove lifted_name
      |> Names.filter (fun name -> not (Names.mem name available))
      |> Names.elements
    in
    let parameters, body = split_abstractions abstraction in
    let* body, nested_lifts = lift_expression available body in
    let lifted_value =
      abstractions (List.map (fun name -> PVar name) captures @ parameters) body
    in
    let rec_flag = match recursive_name with None -> NonRec | Some _ -> Rec in
    let declaration = StrLet (rec_flag, PVar lifted_name, lifted_value) in
    return
      (apply (Var lifted_name) (List.map (fun name -> Var name) captures),
       nested_lifts @ [ declaration ])
  in
  let rec lift_program available = function
    | [] -> return []
    | StrEval expression :: rest ->
      let* expression, lifts = lift_expression available expression in
      let* rest = lift_program available rest in
      return (lifts @ (StrEval expression :: rest))
    | StrLet (rec_flag, pattern, value) :: rest ->
      let* value, lifts =
        match value with
        | Abs _ ->
          let parameters, body = split_abstractions value in
          let* body, lifts = lift_expression available body in
          return (abstractions parameters body, lifts)
        | _ -> lift_expression available value
      in
      let next_available =
        match bound_name pattern with
        | None -> available
        | Some name -> Names.add name available
      in
      let* rest = lift_program next_available rest in
      return (lifts @ (StrLet (rec_flag, pattern, value) :: rest))
  in
  let state = Fresh.create (Names.elements (program_names program)) in
  fst (lift_program builtin_names program state)
;;
