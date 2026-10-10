[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module SMap = Map.Make (String)

type env = Value.value SMap.t

let ( let* ) = Result.bind

let default_fuel = 20_000
let fuel = ref default_fuel

let make_closure f =
  Value.VClosure
    (fun args ->
       if !fuel <= 0
       then Error "fuel exhausted (likely non-terminating recursion)"
       else (
         decr fuel;
         f args))
;;

let constant_value = function
  | Ast.CInt n -> Value.VInt n
  | Ast.CBool b -> Value.VBool b
  | Ast.CUnit -> Value.VUnit
;;

let rec match_pattern env pat v =
  match pat, v with
  | Ast.PAny, _ -> Some env
  | Ast.PVar name, _ -> Some (SMap.add name v env)
  | Ast.PConstant c, v -> if Value.equal (constant_value c) v then Some env else None
  | Ast.PTuple (p1, p2, rest), Value.VTuple (v1, v2, vrest) ->
    (match match_pattern env p1 v1 with
     | None -> None
     | Some env ->
       (match match_pattern env p2 v2 with
        | None -> None
        | Some env -> match_pattern_list env rest vrest))
  | Ast.PTuple _, _ -> None

and match_pattern_list env pats vs =
  match pats, vs with
  | [], [] -> Some env
  | p :: ps, v :: vs ->
    (match match_pattern env p v with
     | None -> None
     | Some env -> match_pattern_list env ps vs)
  | _ -> None
;;

let rec collect_args expr args =
  match expr with
  | Ast.EApply (fn, arg) -> collect_args fn (arg :: args)
  | fn -> fn, args
;;

let rec eval_expr env expr =
  match expr with
  | Ast.EConst c -> Ok (constant_value c)
  | Ast.EIdent name ->
    (match SMap.find_opt name env with
     | Some v -> Ok v
     | None when Value.is_primitive name ->
       Ok (Value.VClosure (fun args -> Value.apply_primitive name args))
     | None -> Error (Printf.sprintf "unbound variable %S" name))
  | Ast.EFun (pat, body) ->
    Ok
      (make_closure
         (function
           | [ v ] ->
             (match match_pattern env pat v with
              | Some env -> eval_expr env body
              | None -> Error "pattern match failure in function argument")
           | _ -> Error "arity mismatch: function expects exactly one argument"))
  | Ast.EApply _ ->
    let fn_expr, arg_exprs = collect_args expr [] in
    let* args = eval_list env arg_exprs in
    (match fn_expr with
     | Ast.EIdent name when (not (SMap.mem name env)) && Value.is_primitive name ->
       Value.apply_primitive name args
     | _ ->
       let* fn_val = eval_expr env fn_expr in
       apply_curried fn_val args)
  | Ast.ETuple (first, second, rest) ->
    let* first = eval_expr env first in
    let* second = eval_expr env second in
    let* rest = eval_list env rest in
    Ok (Value.VTuple (first, second, rest))
  | Ast.EIf (condition, then_branch, else_branch) ->
    let* condition = eval_expr env condition in
    (match condition with
     | Value.VBool true -> eval_expr env then_branch
     | Value.VBool false ->
       (match else_branch with
        | Some else_branch -> eval_expr env else_branch
        | None -> Ok Value.VUnit)
     | _ -> Error "if-condition is not a bool")
  | Ast.ELet (NonRec, first, rest, body) ->
    let* env = bind_nonrec env (first :: rest) in
    eval_expr env body
  | Ast.ELet (Rec, first, rest, body) ->
    let* env = bind_rec env (first :: rest) in
    eval_expr env body
  | Ast.EFunction (first, rest) ->
    Ok
      (make_closure
         (function
           | [ v ] -> eval_cases env (first :: rest) v
           | _ -> Error "arity mismatch: function expects exactly one argument"))
  | Ast.EMatch (subject, first, rest) ->
    let* v = eval_expr env subject in
    eval_cases env (first :: rest) v

and eval_list env exprs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | expr :: rest ->
      let* v = eval_expr env expr in
      go (v :: acc) rest
  in
  go [] exprs

and apply_curried fn_value args =
  match args with
  | [] -> Ok fn_value
  | arg :: rest ->
    (match fn_value with
     | Value.VClosure f ->
       let* result = f [ arg ] in
       apply_curried result rest
     | _ -> Error "cannot apply a non-function value")

and eval_cases env cases v =
  match cases with
  | [] -> Error "match failure: no case matched"
  | { Ast.case_pat; case_expr } :: rest ->
    (match match_pattern env case_pat v with
     | Some env -> eval_expr env case_expr
     | None -> eval_cases env rest v)

and bind_nonrec outer_env bindings =
  let rec go env = function
    | [] -> Ok env
    | { Ast.vb_pat; vb_expr } :: rest ->
      let* v = eval_expr outer_env vb_expr in
      (match match_pattern env vb_pat v with
       | Some env -> go env rest
       | None -> Error "pattern match failure in let binding")
  in
  go outer_env bindings

and names_of_rec_bindings bindings =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | { Ast.vb_pat = Ast.PVar name; vb_expr = (Ast.EFun _ | Ast.EFunction _) } :: rest ->
      go (name :: acc) rest
    | { Ast.vb_pat = Ast.PVar _; _ } :: _ ->
      Error "let rec: the right-hand side must be a function"
    | _ :: _ -> Error "let rec: only variable patterns are supported"
  in
  go [] bindings

and bind_rec env bindings =
  let* names = names_of_rec_bindings bindings in
  let exprs = List.map (fun { Ast.vb_expr; _ } -> vb_expr) bindings in
  let env_ref = ref env in
  let wrapped =
    List.map
      (fun vb_expr ->
         make_closure (fun args ->
           let* fn_value = eval_expr !env_ref vb_expr in
           apply_curried fn_value args))
      exprs
  in
  let env = List.fold_left2 (fun acc name v -> SMap.add name v acc) env names wrapped in
  env_ref := env;
  Ok env
;;

let eval_item env = function
  | Ast.SEval expr ->
    let* _ = eval_expr env expr in
    Ok env
  | Ast.SValue (NonRec, first, rest) -> bind_nonrec env (first :: rest)
  | Ast.SValue (Rec, first, rest) -> bind_rec env (first :: rest)
;;

let eval_structure structure =
  fuel := default_fuel;
  let rec go env = function
    | [] -> Ok env
    | item :: rest ->
      let* env = eval_item env item in
      go env rest
  in
  go SMap.empty structure
;;
