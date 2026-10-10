[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module SMap = Map.Make (String)

type env = Value.value SMap.t

let ( let* ) = Result.bind
let default_fuel = 20_000
let out_of_fuel = Error "fuel exhausted (likely non-terminating recursion)"
let with_fuel fuel result = Result.map (fun value -> value, fuel) result

(* Every call of a closure costs one unit of fuel. *)
let make_closure f =
  Value.VClosure (fun args fuel -> if fuel <= 0 then out_of_fuel else f args (fuel - 1))
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

let rec eval_expr env expr fuel =
  match expr with
  | Ast.EConst c -> Ok (constant_value c, fuel)
  | Ast.EIdent name ->
    (match SMap.find_opt name env with
     | Some v -> Ok (v, fuel)
     | None when Value.is_primitive name ->
       Ok
         ( Value.VClosure
             (fun args fuel -> with_fuel fuel (Value.apply_primitive name args))
         , fuel )
     | None -> Error (Printf.sprintf "unbound variable %S" name))
  | Ast.EFun (pat, body) ->
    Ok
      ( make_closure (fun args fuel ->
          match args with
          | [ v ] ->
            (match match_pattern env pat v with
             | Some env -> eval_expr env body fuel
             | None -> Error "pattern match failure in function argument")
          | _ -> Error "arity mismatch: function expects exactly one argument")
      , fuel )
  | Ast.EApply _ ->
    let fn_expr, arg_exprs = collect_args expr [] in
    let* args, fuel = eval_list env arg_exprs fuel in
    (match fn_expr with
     | Ast.EIdent name when (not (SMap.mem name env)) && Value.is_primitive name ->
       with_fuel fuel (Value.apply_primitive name args)
     | _ ->
       let* fn_val, fuel = eval_expr env fn_expr fuel in
       apply_curried fn_val args fuel)
  | Ast.ETuple (first, second, rest) ->
    let* first, fuel = eval_expr env first fuel in
    let* second, fuel = eval_expr env second fuel in
    let* rest, fuel = eval_list env rest fuel in
    Ok (Value.VTuple (first, second, rest), fuel)
  | Ast.EIf (condition, then_branch, else_branch) ->
    let* condition, fuel = eval_expr env condition fuel in
    (match condition with
     | Value.VBool true -> eval_expr env then_branch fuel
     | Value.VBool false ->
       (match else_branch with
        | Some else_branch -> eval_expr env else_branch fuel
        | None -> Ok (Value.VUnit, fuel))
     | _ -> Error "if-condition is not a bool")
  | Ast.ELet (NonRec, first, rest, body) ->
    let* env, fuel = bind_nonrec env (first :: rest) fuel in
    eval_expr env body fuel
  | Ast.ELet (Rec, first, rest, body) ->
    let* env = bind_rec env (first :: rest) in
    eval_expr env body fuel
  | Ast.EFunction (first, rest) ->
    Ok
      ( make_closure (fun args fuel ->
          match args with
          | [ v ] -> eval_cases env (first :: rest) v fuel
          | _ -> Error "arity mismatch: function expects exactly one argument")
      , fuel )
  | Ast.EMatch (subject, first, rest) ->
    let* v, fuel = eval_expr env subject fuel in
    eval_cases env (first :: rest) v fuel

and eval_list env exprs fuel =
  let rec go acc fuel = function
    | [] -> Ok (List.rev acc, fuel)
    | expr :: rest ->
      let* v, fuel = eval_expr env expr fuel in
      go (v :: acc) fuel rest
  in
  go [] fuel exprs

and apply_curried fn_value args fuel =
  match args with
  | [] -> Ok (fn_value, fuel)
  | arg :: rest ->
    (match fn_value with
     | Value.VClosure f ->
       let* result, fuel = f [ arg ] fuel in
       apply_curried result rest fuel
     | _ -> Error "cannot apply a non-function value")

and eval_cases env cases v fuel =
  match cases with
  | [] -> Error "match failure: no case matched"
  | { Ast.case_pat; case_expr } :: rest ->
    (match match_pattern env case_pat v with
     | Some env -> eval_expr env case_expr fuel
     | None -> eval_cases env rest v fuel)

and bind_nonrec outer_env bindings fuel =
  let rec go env fuel = function
    | [] -> Ok (env, fuel)
    | { Ast.vb_pat; vb_expr } :: rest ->
      let* v, fuel = eval_expr outer_env vb_expr fuel in
      (match match_pattern env vb_pat v with
       | Some env -> go env fuel rest
       | None -> Error "pattern match failure in let binding")
  in
  go outer_env fuel bindings

and names_of_rec_bindings bindings =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | { Ast.vb_pat = Ast.PVar name; vb_expr = Ast.EFun _ | Ast.EFunction _ } :: rest ->
      go (name :: acc) rest
    | { Ast.vb_pat = Ast.PVar _; _ } :: _ ->
      Error "let rec: the right-hand side must be a function"
    | _ :: _ -> Error "let rec: only variable patterns are supported"
  in
  go [] bindings

(* The closures of the group find each other through [rec_env], which builds
   the same environment again on every call instead of patching a reference. *)
and bind_rec env bindings =
  let* names = names_of_rec_bindings bindings in
  let exprs = List.map (fun { Ast.vb_expr; _ } -> vb_expr) bindings in
  let rec rec_env () =
    List.fold_left2
      (fun acc name vb_expr -> SMap.add name (closure vb_expr) acc)
      env
      names
      exprs
  and closure vb_expr =
    make_closure (fun args fuel ->
      let* fn_value, fuel = eval_expr (rec_env ()) vb_expr fuel in
      apply_curried fn_value args fuel)
  in
  Ok (rec_env ())
;;

let eval_item env item fuel =
  match item with
  | Ast.SEval expr ->
    let* _, fuel = eval_expr env expr fuel in
    Ok (env, fuel)
  | Ast.SValue (NonRec, first, rest) -> bind_nonrec env (first :: rest) fuel
  | Ast.SValue (Rec, first, rest) ->
    let* env = bind_rec env (first :: rest) in
    Ok (env, fuel)
;;

let eval_structure structure =
  let rec go env fuel = function
    | [] -> Ok env
    | item :: rest ->
      let* env, fuel = eval_item env item fuel in
      go env fuel rest
  in
  go SMap.empty default_fuel structure
;;
