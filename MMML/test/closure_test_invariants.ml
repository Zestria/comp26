(** Independent closed-group check. It follows lexical scopes directly and
    shares no free-variable or substitution code with the converter. *)
module Scope = Map.Make (String)

exception Invalid of string

let bind scope = function
  | Ast.PWildcard -> scope
  | Ast.PVariable name -> Scope.add name false scope
;;

let rec params reversed = function
  | Ast.Lambda (p, body) -> params (p :: reversed) body
  | body -> List.rev reversed, body
;;

let check ~externals definitions =
  let rec expr scope = function
    | Ast.Constant _ -> ()
    | Ast.Variable name ->
      if not (Scope.mem name scope)
      then raise (Invalid ("unbound in closed group: " ^ name))
    | Ast.Lambda (p, body) -> group scope None p body
    | Ast.Application (fn, arg) ->
      expr scope fn;
      expr scope arg
    | Ast.If_then_else (c, yes, no) ->
      expr scope c;
      expr scope yes;
      expr scope no
    | Ast.Let_in (flag, p, rhs, body) ->
      binding scope flag p rhs;
      expr (bind scope p) body
  and group outer self parameter body =
    let parameters, body = params [ parameter ] body in
    let scope = Scope.filter (fun _ external_ -> external_) outer in
    let scope =
      match self with
      | None -> scope
      | Some p -> bind scope p
    in
    expr (List.fold_left bind scope parameters) body
  and binding scope flag p rhs =
    match flag, p, rhs with
    | Ast.Nonrecursive, _, rhs -> expr scope rhs
    | Ast.Recursive, Ast.PVariable _, Ast.Lambda (parameter, body) ->
      group scope (Some p) parameter body
    | Ast.Recursive, _, _ -> raise (Invalid "invalid recursive binding")
  in
  let initial =
    List.fold_left (fun scope name -> Scope.add name true scope) Scope.empty externals
  in
  try
    ignore
      (List.fold_left
         (fun scope (Ast.Definition (flag, p, rhs)) ->
            binding scope flag p rhs;
            bind scope p)
         initial
         definitions);
    Ok ()
  with
  | Invalid error -> Error error
;;
