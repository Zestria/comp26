module Names = Set.Make (String)
module Scope = Map.Make (String)
module Ir = Closure_ir

type problem =
  | Unbound_identifier of string
  | Recursive_wildcard
  | Recursive_rhs_not_lambda of string

type error =
  { path : string list
  ; problem : problem
  }

let ( let* ) = Result.bind
let fail path problem = Error { path = List.rev path; problem }

let remove_pattern names = function
  | Ast.PWildcard -> names
  | Ast.PVariable name -> Names.remove name names
;;

let bind_pattern scope = function
  | Ast.PWildcard -> scope
  | Ast.PVariable name -> Scope.add name (Ir.Local name) scope
;;

let rec free_names = function
  | Ast.Constant _ -> Names.empty
  | Ast.Variable name -> Names.singleton name
  | Ast.Lambda (parameter, body) -> remove_pattern (free_names body) parameter
  | Ast.Application (callee, argument) ->
    Names.union (free_names callee) (free_names argument)
  | Ast.If_then_else (condition, yes, no) ->
    Names.union (free_names condition) (Names.union (free_names yes) (free_names no))
  | Ast.Let_in (flag, pattern, rhs, body) ->
    let rhs_names = free_names rhs in
    let body_names = remove_pattern (free_names body) pattern in
    let rhs_names =
      match flag with
      | Ast.Nonrecursive -> rhs_names
      | Ast.Recursive -> remove_pattern rhs_names pattern
    in
    Names.union rhs_names body_names
;;

let resolve scope path name =
  match Scope.find_opt name scope with
  | Some reference -> Ok reference
  | None -> fail path (Unbound_identifier name)
;;

let rec expression scope path = function
  | Ast.Constant value -> Ok (Ir.Constant value)
  | Ast.Variable name ->
    let* reference = resolve scope path name in
    Ok (Ir.Reference reference)
  | Ast.Lambda (parameter, body) -> closure scope path None parameter body
  | Ast.Application (callee, argument) ->
    let* callee = expression scope ("callee" :: path) callee in
    let* argument = expression scope ("argument" :: path) argument in
    Ok (Ir.Call (callee, argument))
  | Ast.If_then_else (condition, yes, no) ->
    let* condition = expression scope ("if.condition" :: path) condition in
    let* yes = expression scope ("if.then" :: path) yes in
    let* no = expression scope ("if.else" :: path) no in
    Ok (Ir.If (condition, yes, no))
  | Ast.Let_in (flag, pattern, rhs, body) ->
    let* rhs = binding_rhs scope ("let.rhs" :: path) flag pattern rhs in
    let* body = expression (bind_pattern scope pattern) ("let.body" :: path) body in
    Ok (Ir.Let (pattern, rhs, body))

and binding_rhs scope path flag pattern rhs =
  match flag, pattern, rhs with
  | Ast.Nonrecursive, _, rhs -> expression scope path rhs
  | Ast.Recursive, Ast.PWildcard, _ -> fail path Recursive_wildcard
  | Ast.Recursive, Ast.PVariable name, Ast.Lambda (parameter, body) ->
    closure scope path (Some name) parameter body
  | Ast.Recursive, Ast.PVariable name, _ -> fail path (Recursive_rhs_not_lambda name)

and closure scope path self_name parameter body =
  let names = remove_pattern (free_names body) parameter in
  let names =
    match self_name with
    | None -> names
    | Some name -> Names.remove name names
  in
  (* Rebuild the scope at a code boundary: nothing may retain an outer Local,
     Env_slot or Self implicitly. Nested captures are resolved in this scope. *)
  let rec capture_layout names index code_scope labels captures =
    match names with
    | [] -> Ok (code_scope, List.rev labels, List.rev captures)
    | name :: rest ->
      let* reference = resolve scope ("lambda.capture" :: path) name in
      (match reference with
       | Ir.External _ ->
         capture_layout rest index (Scope.add name reference code_scope) labels captures
       | Ir.Local _ | Ir.Env_slot _ | Ir.Self ->
         capture_layout
           rest
           (index + 1)
           (Scope.add name (Ir.Env_slot index) code_scope)
           (name :: labels)
           (reference :: captures))
  in
  let* code_scope, capture_names, captures =
    capture_layout (Names.elements names) 0 Scope.empty [] []
  in
  let code_scope =
    match self_name with
    | None -> code_scope
    | Some name -> Scope.add name Ir.Self code_scope
  in
  let* body =
    expression (bind_pattern code_scope parameter) ("lambda.body" :: path) body
  in
  Ok (Ir.Make_closure ({ parameter; capture_names; body }, captures))
;;

let convert ~externals definitions =
  let externals = List.sort_uniq String.compare externals in
  let initial_scope =
    List.fold_left
      (fun scope name -> Scope.add name (Ir.External name) scope)
      Scope.empty
      externals
  in
  let rec loop index scope reversed = function
    | [] -> Ok { Ir.externals; definitions = List.rev reversed }
    | Ast.Definition (flag, pattern, rhs) :: rest ->
      let path = [ Printf.sprintf "definition[%d]" index ] in
      let* rhs = binding_rhs scope path flag pattern rhs in
      loop
        (index + 1)
        (bind_pattern scope pattern)
        (Ir.Definition (pattern, rhs) :: reversed)
        rest
  in
  loop 0 initial_scope [] definitions
;;

let pp_error formatter { path; problem } =
  Format.fprintf formatter "%s: " (String.concat "/" path);
  match problem with
  | Unbound_identifier name -> Format.fprintf formatter "unbound identifier %S" name
  | Recursive_wildcard ->
    Format.pp_print_string formatter "recursive binding requires a function name"
  | Recursive_rhs_not_lambda name ->
    Format.fprintf formatter "recursive binding %S requires a lambda RHS" name
;;
