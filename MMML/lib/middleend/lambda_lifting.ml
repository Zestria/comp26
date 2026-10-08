open Ast

let pattern_variables = function
  | PWildcard -> []
  | PVariable name -> [ name ]
;;

let counter = ref 0

let fresh () =
  incr counter;
  "lambda" ^ string_of_int !counter
;;

let rec free_variables bound = function
  | Constant _ -> []
  | Variable name -> if List.mem name bound then [] else [ name ]
  | Lambda (pattern, body) -> free_variables (pattern_variables pattern @ bound) body
  | Application (function_, argument) ->
    free_variables bound function_ @ free_variables bound argument
  | If_then_else (condition, if_true, if_false) ->
    free_variables bound condition
    @ free_variables bound if_true
    @ free_variables bound if_false
  | Let_in (flag, pattern, value, body) ->
    let bound_value =
      match flag with
      | Nonrecursive -> bound
      | Recursive -> pattern_variables pattern @ bound
    in
    free_variables bound_value value
    @ free_variables (pattern_variables pattern @ bound) body
;;

let flatten_lambdas value =
  let rec loop parameters = function
    | Lambda (pattern, body) -> loop (pattern :: parameters) body
    | body -> List.rev parameters, body
  in
  loop [] value
;;

let captured bound own_names value =
  free_variables own_names value
  |> List.sort_uniq String.compare
  |> List.filter (fun name -> List.mem name bound)
;;

let build_definition own_parameters own_names bound value definitions =
  let free = captured bound own_names value in
  match free with
  | [] -> None, definitions
  | _ :: _ ->
    let name = fresh () in
    let free_patterns = List.map (fun variable -> PVariable variable) free in
    let free_arguments = List.map (fun variable -> Variable variable) free in
    let definition =
      Definition
        (Nonrecursive, PVariable name, function_ (free_patterns @ own_parameters) value)
    in
    Some (application (Variable name) free_arguments), definitions @ [ definition ]
;;

let rec transform bound expr =
  match expr with
  | Constant _ | Variable _ -> expr, []
  | Lambda (pattern, body) -> lift bound pattern body
  | Application (function_, argument) ->
    let function_, left = transform bound function_ in
    let argument, right = transform bound argument in
    Application (function_, argument), left @ right
  | If_then_else (condition, if_true, if_false) ->
    let condition, a = transform bound condition in
    let if_true, b = transform bound if_true in
    let if_false, c = transform bound if_false in
    If_then_else (condition, if_true, if_false), a @ b @ c
  | Let_in (flag, pattern, value, body) ->
    let original = value in
    let bound_value =
      match flag with
      | Nonrecursive -> bound
      | Recursive -> pattern_variables pattern @ bound
    in
    let value, value_defs = transform bound_value value in
    let body, body_defs = transform (pattern_variables pattern @ bound) body in
    (match flag, original with
     | Nonrecursive, (Constant _ | Variable _ | Application _ | If_then_else _ | Let_in _)
       ->
       (match build_definition [] [] bound value value_defs with
        | None, definitions ->
          Let_in (flag, pattern, value, body), definitions @ body_defs
        | Some site, definitions ->
          Let_in (flag, pattern, site, body), definitions @ body_defs)
     | _ -> Let_in (flag, pattern, value, body), value_defs @ body_defs)

and lift bound pattern body =
  let body, inner = transform (pattern_variables pattern @ bound) body in
  match build_definition [ pattern ] (pattern_variables pattern) bound body inner with
  | None, definitions -> Lambda (pattern, body), definitions
  | Some site, definitions -> site, definitions
;;

let transform_definition (Definition (flag, pattern, value)) =
  let parameters, body = flatten_lambdas value in
  let bound =
    (match flag with
     | Nonrecursive -> []
     | Recursive -> pattern_variables pattern)
    @ List.concat_map pattern_variables parameters
  in
  let body, lifted = transform bound body in
  lifted @ [ Definition (flag, pattern, function_ parameters body) ]
;;

let lift_program program =
  counter := 0;
  List.concat_map transform_definition program
;;
