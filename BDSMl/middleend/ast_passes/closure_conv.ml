[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend
open Ast

module String_map = Map.Make (String)
module String_set = Free_vars.String_set

let pattern_name = function
  | PVar name -> Some name
  | PUnit | PUnderscore -> None
;;

let remove_pattern environment pattern =
  match pattern_name pattern with
  | Some name -> String_map.remove name environment
  | None -> environment
;;

let remove_names environment names =
  List.fold_left (fun environment name -> String_map.remove name environment) environment names
;;

let add_pattern names pattern =
  match pattern_name pattern with
  | Some name -> String_set.add name names
  | None -> names
;;

let add_names names added =
  List.fold_left (fun names name -> String_set.add name names) names added
;;

let split_abstractions expression =
  let rec loop parameters = function
    | Abs (parameter, body) -> loop (parameter :: parameters) body
    | body -> List.rev parameters, body
  in
  loop [] expression
;;

let make_abstractions parameters body =
  List.fold_right (fun parameter body -> Abs (parameter, body)) parameters body
;;

let apply function_ arguments =
  List.fold_left (fun function_ argument -> App (function_, argument)) function_ arguments
;;

let bound_names patterns = List.filter_map pattern_name patterns

let builtin_names =
  [ "print_int"; "print_endline"; "print_string"; "string_of_int"; "int_of_string" ]
;;

let globals_of_program program =
  List.fold_left
    (fun globals -> function
      | StrLet (_, PVar name, _) -> String_set.add name globals
      | StrLet (_, (PUnit | PUnderscore), _) | StrEval _ -> globals)
    (String_set.of_list builtin_names)
    program
;;

(** [recursive_functions] maps a local recursive function to the names of its
    explicit environment parameters.  Looking up such a function constructs
    its closure by applying those parameters. *)
let rec convert_expression globals locals recursive_functions expression =
  let convert = convert_expression globals locals recursive_functions in
  match expression with
  | Unit | Int _ | Bool _ -> expression
  | Var name -> convert_variable recursive_functions name
  | BinOp (operator, left, right) -> BinOp (operator, convert left, convert right)
  | UnOp (operator, operand) -> UnOp (operator, convert operand)
  | If (condition, if_true, if_false) ->
    If (convert condition, convert if_true, convert if_false)
  | App (function_, argument) -> App (convert function_, convert argument)
  | Abs _ -> convert_abstraction globals locals recursive_functions expression
  | Let (Rec, PVar name, value, body) ->
    convert_recursive_let globals locals recursive_functions name value body
  | Let (rec_flag, pattern, value, body) ->
    let value_locals =
      match rec_flag with
      | Rec -> add_pattern locals pattern
      | NonRec -> locals
    in
    let value_environment =
      match rec_flag with
      | Rec -> remove_pattern recursive_functions pattern
      | NonRec -> recursive_functions
    in
    let converted_value =
      convert_expression globals value_locals value_environment value
    in
    let body_environment = remove_pattern recursive_functions pattern in
    let body_locals = add_pattern locals pattern in
    Let
      ( rec_flag
      , pattern
      , converted_value
      , convert_expression globals body_locals body_environment body )

and convert_variable recursive_functions name =
  match String_map.find_opt name recursive_functions with
  | None -> Var name
  | Some captured ->
    let arguments =
      List.map (convert_variable recursive_functions) captured
    in
    apply (Var name) arguments

and captured_variables globals locals expression =
  Free_vars.free_vars expression
  |> String_set.filter (fun name ->
    String_set.mem name locals || not (String_set.mem name globals))
  |> String_set.elements

and convert_abstraction globals locals recursive_functions expression =
  let parameters, body = split_abstractions expression in
  let captured = captured_variables globals locals expression in
  let shadowed = captured @ bound_names parameters in
  let body_environment = remove_names recursive_functions shadowed in
  let body_locals = add_names locals shadowed in
  let converted_body =
    convert_expression globals body_locals body_environment body
  in
  let function_ =
    make_abstractions
      (List.map (fun name -> PVar name) captured @ parameters)
      converted_body
  in
  let captured_values =
    List.map (convert_variable recursive_functions) captured
  in
  apply function_ captured_values

and convert_recursive_let globals locals recursive_functions name value body =
  match value with
  | Abs _ ->
    let parameters, function_body = split_abstractions value in
    let captured =
      captured_variables globals locals value
      |> List.filter (fun variable -> variable <> name)
    in
    let function_environment =
      remove_names recursive_functions (captured @ bound_names parameters)
      |> String_map.add name captured
    in
    let function_locals =
      add_names locals ((name :: captured) @ bound_names parameters)
    in
    let converted_function_body =
      convert_expression globals function_locals function_environment function_body
    in
    let converted_value =
      make_abstractions
        (List.map (fun variable -> PVar variable) captured @ parameters)
        converted_function_body
    in
    let body_environment = String_map.add name captured recursive_functions in
    let body_locals = String_set.add name locals in
    Let
      ( Rec
      , PVar name
      , converted_value
      , convert_expression globals body_locals body_environment body )
  | _ ->
    let value_environment = String_map.remove name recursive_functions in
    let body_environment = String_map.remove name recursive_functions in
    let recursive_locals = String_set.add name locals in
    Let
      ( Rec
      , PVar name
      , convert_expression globals recursive_locals value_environment value
      , convert_expression globals recursive_locals body_environment body )
;;

let convert_top_level_value globals value =
  match value with
  | Abs _ ->
    let parameters, body = split_abstractions value in
    let locals = add_names String_set.empty (bound_names parameters) in
    make_abstractions
      parameters
      (convert_expression globals locals String_map.empty body)
  | _ -> convert_expression globals String_set.empty String_map.empty value
;;

let closure_conv program =
  let globals = globals_of_program program in
  List.map
    (function
      | StrEval expression ->
        StrEval
          (convert_expression globals String_set.empty String_map.empty expression)
      | StrLet (rec_flag, pattern, value) ->
        StrLet (rec_flag, pattern, convert_top_level_value globals value))
    program
;;
