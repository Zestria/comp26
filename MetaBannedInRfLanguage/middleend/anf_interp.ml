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

let eval_imm env = function
  | Anf.ImmInt n -> Ok (Value.VInt n)
  | Anf.ImmBool b -> Ok (Value.VBool b)
  | Anf.ImmUnit -> Ok Value.VUnit
  | Anf.ImmVar name ->
    (match SMap.find_opt name env with
     | Some v -> Ok v
     | None when Value.is_primitive name ->
       Ok
         (Value.VClosure
            (fun args fuel -> with_fuel fuel (Value.apply_primitive name args)))
     | None -> Error (Printf.sprintf "unbound variable %S" name))
;;

let eval_imms env imms =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | imm :: rest ->
      let* v = eval_imm env imm in
      go (v :: acc) rest
  in
  go [] imms
;;

let apply_value fn_value args fuel =
  match fn_value with
  | Value.VClosure f -> f args fuel
  | _ -> Error "cannot apply a non-function value"
;;

let rec take n list =
  if n <= 0
  then []
  else (
    match list with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs)
;;

let rec drop n list =
  if n <= 0
  then list
  else (
    match list with
    | [] -> []
    | _ :: xs -> drop (n - 1) xs)
;;

let rec eval_cexpr env cexpr fuel =
  match cexpr with
  | Anf.CImm imm -> with_fuel fuel (eval_imm env imm)
  | Anf.CApp (fn, args) ->
    let* fn_value = eval_imm env fn in
    let* args = eval_imms env args in
    apply_value fn_value args fuel
  | Anf.CTuple (first, second, rest) ->
    let* first = eval_imm env first in
    let* second = eval_imm env second in
    let* rest = eval_imms env rest in
    Ok (Value.VTuple (first, second, rest), fuel)
  | Anf.CIte (cond, then_branch, else_branch) ->
    let* cond = eval_imm env cond in
    (match cond with
     | Value.VBool true -> eval_aexpr env then_branch fuel
     | Value.VBool false -> eval_aexpr env else_branch fuel
     | _ -> Error "if-condition is not a bool")
  | Anf.CFun (params, body) -> Ok (Value.VClosure (apply_closure env params body), fuel)
  | Anf.CMatchFailure -> Error "match failure: no case matched"

and eval_aexpr env aexpr fuel =
  match aexpr with
  | Anf.ACExpr cexpr -> eval_cexpr env cexpr fuel
  | Anf.ALet (name, value, body) ->
    let* v, fuel = eval_cexpr env value fuel in
    eval_aexpr (SMap.add name v env) body fuel
  | Anf.ALetTuple (names, subject, body) ->
    let* v = eval_imm env subject in
    (match v with
     | Value.VTuple (first, second, rest) ->
       let values = first :: second :: rest in
       (match List.compare_lengths names values with
        | 0 ->
          let env =
            List.fold_left2
              (fun acc name v ->
                 match name with
                 | Some name -> SMap.add name v acc
                 | None -> acc)
              env
              names
              values
          in
          eval_aexpr env body fuel
        | _ -> Error "tuple arity mismatch")
     | _ -> Error "expected a tuple")
  | Anf.ALetRec (bindings, body) -> eval_aexpr (make_rec_env env bindings) body fuel

and apply_closure env params body args fuel =
  if fuel <= 0
  then out_of_fuel
  else (
    let fuel = fuel - 1 in
    let nparams = List.length params in
    let nargs = List.length args in
    if nargs = nparams
    then (
      let env = List.fold_left2 (fun acc p v -> SMap.add p v acc) env params args in
      eval_aexpr env body fuel)
    else if nargs < nparams
    then (
      let given = take nargs params in
      let remaining = drop nargs params in
      let env = List.fold_left2 (fun acc p v -> SMap.add p v acc) env given args in
      Ok (Value.VClosure (apply_closure env remaining body), fuel))
    else (
      let these = take nparams args in
      let rest = drop nparams args in
      let env = List.fold_left2 (fun acc p v -> SMap.add p v acc) env params these in
      let* fn_value, fuel = eval_aexpr env body fuel in
      apply_value fn_value rest fuel))

(* The closures of the group find each other through [rec_env], which builds
   the same environment again on every call instead of patching a reference. *)
and make_rec_env env bindings =
  let rec rec_env () =
    List.fold_left
      (fun acc (name, cexpr) -> SMap.add name (closure cexpr) acc)
      env
      bindings
  and closure cexpr =
    make_closure (fun args fuel ->
      let* fn_value, fuel = eval_cexpr (rec_env ()) cexpr fuel in
      apply_value fn_value args fuel)
  in
  rec_env ()
;;

let eval_item env item fuel =
  match item with
  | Anf.AValue (name, body) ->
    let* v, fuel = eval_aexpr env body fuel in
    Ok (SMap.add name v env, fuel)
  | Anf.ARec bindings -> Ok (make_rec_env env bindings, fuel)
  | Anf.AEval body ->
    let* _, fuel = eval_aexpr env body fuel in
    Ok (env, fuel)
;;

let eval_program program =
  let rec go env fuel = function
    | [] -> Ok env
    | item :: rest ->
      let* env, fuel = eval_item env item fuel in
      go env fuel rest
  in
  go SMap.empty default_fuel program
;;
