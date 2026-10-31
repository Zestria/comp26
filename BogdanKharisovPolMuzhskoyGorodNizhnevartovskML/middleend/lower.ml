[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend

type error =
  | Unbound_name of Name.t
  | Duplicate_pattern_name of Name.t
  | Invalid_recursive_pattern
  | Identifier_error of Id.error

let error_to_string = function
  | Identifier_error error -> Id.error_to_string error
  | Unbound_name name -> "Unbound value " ^ Name.to_string name
  | Duplicate_pattern_name name ->
    "Variable " ^ Name.to_string name ^ " is bound more than once in this pattern"
  | Invalid_recursive_pattern -> "Only variables are allowed on the left of let rec"
;;

module Names = Map.Make (String)
module Name_set = Set.Make (String)

type reference =
  | Local of Id.t
  | Builtin of Primitive.t

let initial_environment =
  List.fold_left
    (fun environment primitive ->
       Names.add (Primitive.name primitive) (Builtin primitive) environment)
    Names.empty
    Primitive.all
;;

type bound_pattern =
  | Bind of Id.t
  | Ignore
  | Destructure of bound_pattern list

let return value supply = Ok (value, supply)
let fail error _supply = Error error

let ( let* ) action continuation supply =
  Result.bind (action supply) (fun (value, supply) -> continuation value supply)
;;

let fresh name supply =
  Result.map_error (fun error -> Identifier_error error) (Id.fresh supply name)
;;

let rec map_list f = function
  | [] -> return []
  | x :: xs ->
    let* y = f x in
    let* ys = map_list f xs in
    return (y :: ys)
;;

let bind_pattern environment pattern =
  let rec bind environment names = function
    | Ast.PVar name ->
      let spelling = Name.to_string name in
      if Name_set.mem spelling names
      then fail (Duplicate_pattern_name name)
      else
        let* id = fresh spelling in
        return
          (Names.add spelling (Local id) environment, Name_set.add spelling names, Bind id)
    | PWildcard | PUnit -> return (environment, names, Ignore)
    | PTuple (a, b, rest) ->
      let rec elements environment names = function
        | [] -> return (environment, names, [])
        | pattern :: rest ->
          let* environment, names, pattern = bind environment names pattern in
          let* environment, names, rest = elements environment names rest in
          return (environment, names, pattern :: rest)
      in
      let* environment, names, patterns = elements environment names (a :: b :: rest) in
      return (environment, names, Destructure patterns)
  in
  let* environment, _, pattern = bind environment Name_set.empty pattern in
  return (environment, pattern)
;;

let rec compile_pattern pattern value body =
  match pattern with
  | Bind id -> return (Lambda.Let (id, value, body))
  | Ignore -> return (Lambda.Sequence (value, body))
  | Destructure patterns ->
    let* tuple = fresh "tuple" in
    let fields = List.mapi (fun index pattern -> index, pattern) patterns in
    let* body =
      List.fold_right
        (fun (index, pattern) body ->
           let* body = body in
           match pattern with
           | Ignore -> return body
           | Bind _ | Destructure _ ->
             compile_pattern pattern (Lambda.Field (Var tuple, index)) body)
        fields
        (return body)
    in
    return (Lambda.Let (tuple, value, body))
;;

let rec lower environment = function
  | Ast.Constant constant -> return (Lambda.Constant constant)
  | Var name ->
    (match Names.find_opt (Name.to_string name) environment with
     | None -> fail (Unbound_name name)
     | Some (Local id) -> return (Lambda.Var id)
     | Some (Builtin primitive) -> return (Lambda.Primitive primitive))
  | Tuple (a, b, rest) ->
    let* a = lower environment a in
    let* b = lower environment b in
    let* rest = map_list (lower environment) rest in
    return (Lambda.Tuple (a, b, rest))
  | Fun { parameter; parameters; body } ->
    let bind_parameter environment pattern =
      let* environment, pattern = bind_pattern environment pattern in
      let* id =
        match pattern with
        | Bind id -> return id
        | Ignore | Destructure _ -> fresh "arg"
      in
      return (environment, (id, pattern))
    in
    let rec bind_parameters environment = function
      | [] -> return (environment, [])
      | pattern :: rest ->
        let* environment, parameter = bind_parameter environment pattern in
        let* environment, rest = bind_parameters environment rest in
        return (environment, parameter :: rest)
    in
    let* environment, (parameter, pattern) = bind_parameter environment parameter in
    let* environment, parameters = bind_parameters environment parameters in
    let* body = lower environment body in
    let* body =
      List.fold_right
        (fun (id, pattern) body ->
           let* body = body in
           match pattern with
           | Bind _ | Ignore -> return body
           | Destructure _ -> compile_pattern pattern (Lambda.Var id) body)
        ((parameter, pattern) :: parameters)
        (return body)
    in
    return (Lambda.Fun { parameter; parameters = List.map fst parameters; body })
  | Apply (f, a, rest) ->
    let* f = lower environment f in
    let* a = lower environment a in
    let* rest = map_list (lower environment) rest in
    let finish short_circuit = function
      | [] -> return short_circuit
      | a :: rest -> return (Lambda.Apply (short_circuit, a, rest))
    in
    (match f, rest with
     | Lambda.Primitive And, b :: rest ->
       finish (Lambda.If (a, b, Constant (Bool false))) rest
     | Lambda.Primitive Or, b :: rest ->
       finish (Lambda.If (a, Constant (Bool true), b)) rest
     | _ -> return (Lambda.Apply (f, a, rest)))
  | Let (binding, body) -> lower_binding environment binding (fun env -> lower env body)
  | If (condition, yes, no) ->
    let* condition = lower environment condition in
    let* yes = lower environment yes in
    let* no =
      match no with
      | None -> return (Lambda.Constant Unit)
      | Some no -> lower environment no
    in
    return (Lambda.If (condition, yes, no))

and lower_binding environment { Ast.recursion; pattern; expression } continuation =
  match recursion with
  | Nonrecursive ->
    let* value = lower environment expression in
    let* environment, pattern = bind_pattern environment pattern in
    let* body = continuation environment in
    compile_pattern pattern value body
  | Recursive ->
    (match pattern with
     | PVar name ->
       let* id = fresh (Name.to_string name) in
       let environment = Names.add (Name.to_string name) (Local id) environment in
       let* value = lower environment expression in
       let* body = continuation environment in
       return (Lambda.Let_rec (id, value, body))
     | PWildcard | PUnit | PTuple _ -> fail Invalid_recursive_pattern)
;;

let expression supply expression = lower initial_environment expression supply

let program supply program =
  let rec items environment = function
    | [] -> return (Lambda.Constant Unit)
    | Ast.Definition binding :: rest ->
      lower_binding environment binding (fun environment -> items environment rest)
    | Evaluation expression :: rest ->
      let* expression = lower environment expression in
      let* rest = items environment rest in
      return (Lambda.Sequence (expression, rest))
  in
  items initial_environment program supply
;;
