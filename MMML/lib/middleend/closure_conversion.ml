module Names = Set.Make (String)
module Scope = Map.Make (String)

type problem =
  | Unbound_identifier of string
  | Recursive_wildcard
  | Recursive_rhs_not_lambda of string

type error =
  { path : string list
  ; problem : problem
  }

type binding =
  | External of string
  | Value of Ast.expr

let ( let* ) = Result.bind
let fail path problem = Error { path = List.rev path; problem }

let remove_pattern names = function
  | Ast.PWildcard -> names
  | Ast.PVariable name -> Names.remove name names
;;

let bind_pattern scope = function
  | Ast.PWildcard -> scope
  | Ast.PVariable name -> Scope.add name (Value (Ast.Variable name)) scope
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
    let rhs = free_names rhs in
    let rhs =
      match flag with
      | Ast.Nonrecursive -> rhs
      | Ast.Recursive -> remove_pattern rhs pattern
    in
    Names.union rhs (remove_pattern (free_names body) pattern)
;;

let resolve scope path name =
  match Scope.find_opt name scope with
  | Some reference -> Ok reference
  | None -> fail path (Unbound_identifier name)
;;

let value = function
  | External name -> Ast.Variable name
  | Value expression -> expression
;;

(* Only consecutive lambdas form a parameter group; never cross a let or if. *)
let rec parameters reversed = function
  | Ast.Lambda (parameter, body) -> parameters (parameter :: reversed) body
  | body -> List.rev reversed, body
;;

let fresh_supply ~externals definitions =
  let used = ref (Names.of_list externals) in
  let reserve name = used := Names.add name !used in
  let pattern = function
    | Ast.PWildcard -> ()
    | Ast.PVariable name -> reserve name
  in
  let rec expression = function
    | Ast.Constant _ -> ()
    | Ast.Variable name -> reserve name
    | Ast.Lambda (p, body) ->
      pattern p;
      expression body
    | Ast.Application (fn, arg) ->
      expression fn;
      expression arg
    | Ast.If_then_else (c, yes, no) ->
      expression c;
      expression yes;
      expression no
    | Ast.Let_in (_, p, rhs, body) ->
      pattern p;
      expression rhs;
      expression body
  in
  List.iter
    (fun (Ast.Definition (_, p, rhs)) ->
       pattern p;
       expression rhs)
    definitions;
  let counter = ref 0 in
  fun prefix ->
    let rec find () =
      let name = "cc_" ^ prefix ^ "_" ^ string_of_int !counter in
      incr counter;
      if Names.mem name !used
      then find ()
      else (
        reserve name;
        name)
    in
    find ()
;;

let convert ~externals definitions =
  let fresh = fresh_supply ~externals definitions in
  let rec expression scope path = function
    | Ast.Constant value -> Ok (Ast.Constant value)
    | Ast.Variable name ->
      let* reference = resolve scope path name in
      Ok (value reference)
    | Ast.Lambda (parameter, body) ->
      let* _, code, captures = closure scope path None parameter body in
      Ok (Ast.application code captures)
    | Ast.Application (callee, argument) ->
      let* callee = expression scope ("callee" :: path) callee in
      let* argument = expression scope ("argument" :: path) argument in
      Ok (Ast.Application (callee, argument))
    | Ast.If_then_else (condition, yes, no) ->
      let* condition = expression scope ("if.condition" :: path) condition in
      let* yes = expression scope ("if.then" :: path) yes in
      let* no = expression scope ("if.else" :: path) no in
      Ok (Ast.If_then_else (condition, yes, no))
    | Ast.Let_in (Ast.Nonrecursive, pattern, rhs, body) ->
      let* rhs = expression scope ("let.rhs" :: path) rhs in
      let* body = expression (bind_pattern scope pattern) ("let.body" :: path) body in
      Ok (Ast.Let_in (Ast.Nonrecursive, pattern, rhs, body))
    | Ast.Let_in (Ast.Recursive, pattern, rhs, body) ->
      let* name, raw, code, captures = recursive scope ("let.rhs" :: path) pattern rhs in
      let* body = expression (bind_pattern scope pattern) ("let.body" :: path) body in
      let body =
        if captures = []
        then body
        else
          Ast.Let_in
            ( Ast.Nonrecursive
            , Ast.PVariable name
            , Ast.application (Ast.Variable raw) captures
            , body )
      in
      Ok (Ast.Let_in (Ast.Recursive, Ast.PVariable raw, code, body))
  and recursive scope path pattern rhs =
    match pattern, rhs with
    | Ast.PWildcard, _ -> fail path Recursive_wildcard
    | Ast.PVariable name, Ast.Lambda (parameter, body) ->
      let* raw, code, captures = closure scope path (Some name) parameter body in
      (match raw with
       | Some raw -> Ok (name, raw, code, captures)
       | None -> assert false)
    | Ast.PVariable name, _ -> fail path (Recursive_rhs_not_lambda name)
  and closure scope path self parameter body =
    let params, body = parameters [ parameter ] body in
    let names = List.fold_left remove_pattern (free_names body) params in
    let names =
      match self with
      | None -> names
      | Some name -> Names.remove name names
    in
    let rec layout remaining inner reversed_formals reversed_values =
      match remaining with
      | [] -> Ok (inner, List.rev reversed_formals, List.rev reversed_values)
      | name :: rest ->
        let* reference = resolve scope ("lambda.capture" :: path) name in
        (match reference with
         | External _ ->
           layout rest (Scope.add name reference inner) reversed_formals reversed_values
         | Value captured ->
           let formal = fresh "capture" in
           layout
             rest
             (Scope.add name (Value (Ast.Variable formal)) inner)
             (Ast.PVariable formal :: reversed_formals)
             (captured :: reversed_values))
    in
    let* inner, formals, captures = layout (Names.elements names) Scope.empty [] [] in
    let raw =
      Option.map (fun name -> if captures = [] then name else fresh "code") self
    in
    let inner =
      match self, raw with
      | Some name, Some raw ->
        let args =
          List.map
            (function
              | Ast.PVariable name -> Ast.Variable name
              | Ast.PWildcard -> assert false)
            formals
        in
        Scope.add name (Value (Ast.application (Ast.Variable raw) args)) inner
      | _ -> inner
    in
    let inner = List.fold_left bind_pattern inner params in
    let* body = expression inner ("lambda.body" :: path) body in
    Ok (raw, Ast.function_ (formals @ params) body, captures)
  in
  let initial_scope =
    List.fold_left
      (fun scope name -> Scope.add name (External name) scope)
      Scope.empty
      externals
  in
  let rec loop index scope reversed = function
    | [] -> Ok (List.rev reversed)
    | Ast.Definition (flag, pattern, rhs) :: rest ->
      let path = [ Printf.sprintf "definition[%d]" index ] in
      let* items =
        match flag with
        | Ast.Nonrecursive ->
          let* rhs = expression scope path rhs in
          Ok [ Ast.Definition (flag, pattern, rhs) ]
        | Ast.Recursive ->
          let* name, raw, code, captures = recursive scope path pattern rhs in
          let definition = Ast.Definition (Ast.Recursive, Ast.PVariable raw, code) in
          if captures = []
          then Ok [ definition ]
          else
            Ok
              [ definition
              ; Ast.Definition
                  ( Ast.Nonrecursive
                  , Ast.PVariable name
                  , Ast.application (Ast.Variable raw) captures )
              ]
      in
      loop (index + 1) (bind_pattern scope pattern) (List.rev_append items reversed) rest
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
