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

let eval_imm env = function
  | Anf.ImmInt n -> Ok (Value.VInt n)
  | Anf.ImmBool b -> Ok (Value.VBool b)
  | Anf.ImmUnit -> Ok Value.VUnit
  | Anf.ImmVar name ->
    (match SMap.find_opt name env with
     | Some v -> Ok v
     | None when Value.is_primitive name ->
       Ok (Value.VClosure (fun args -> Value.apply_primitive name args))
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

let apply_value fn_value args =
  match fn_value with
  | Value.VClosure f -> f args
  | _ -> Error "cannot apply a non-function value"
;;

let rec eval_cexpr env = function
  | Anf.CImm imm -> eval_imm env imm
  | Anf.CApp (fn, args) ->
    let* fn_value = eval_imm env fn in
    let* args = eval_imms env args in
    apply_value fn_value args
  | Anf.CTuple (first, second, rest) ->
    let* first = eval_imm env first in
    let* second = eval_imm env second in
    let* rest = eval_imms env rest in
    Ok (Value.VTuple (first, second, rest))
  | Anf.CIte (cond, then_branch, else_branch) ->
    let* cond = eval_imm env cond in
    (match cond with
     | Value.VBool true -> eval_aexpr env then_branch
     | Value.VBool false -> eval_aexpr env else_branch
     | _ -> Error "if-condition is not a bool")
  | Anf.CFun (params, body) ->
    Ok
      (make_closure (fun args ->
         match List.compare_lengths params args with
         | 0 ->
           let env = List.fold_left2 (fun acc p v -> SMap.add p v acc) env params args in
           eval_aexpr env body
         | _ -> Error "arity mismatch"))
  | Anf.CMatchFailure -> Error "match failure: no case matched"

and eval_aexpr env = function
  | Anf.ACExpr cexpr -> eval_cexpr env cexpr
  | Anf.ALet (name, value, body) ->
    let* v = eval_cexpr env value in
    eval_aexpr (SMap.add name v env) body
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
          eval_aexpr env body
        | _ -> Error "tuple arity mismatch")
     | _ -> Error "expected a tuple")
  | Anf.ALetRec (bindings, body) -> eval_aexpr (make_rec_env env bindings) body

and make_rec_env env bindings =
  let env_ref = ref env in
  let names = List.map fst bindings in
  let wrapped =
    List.map
      (fun (_, cexpr) ->
         make_closure (fun args ->
           let* fn_value = eval_cexpr !env_ref cexpr in
           apply_value fn_value args))
      bindings
  in
  let env = List.fold_left2 (fun acc name v -> SMap.add name v acc) env names wrapped in
  env_ref := env;
  env
;;

let eval_item env = function
  | Anf.AValue (name, body) ->
    let ( let* ) = Result.bind in
    let* v = eval_aexpr env body in
    Ok (SMap.add name v env)
  | Anf.ARec bindings -> Ok (make_rec_env env bindings)
  | Anf.AEval body ->
    let ( let* ) = Result.bind in
    let* _ = eval_aexpr env body in
    Ok env
;;

let eval_program program =
  fuel := default_fuel;
  let rec go env = function
    | [] -> Ok env
    | item :: rest ->
      let* env = eval_item env item in
      go env rest
  in
  go SMap.empty program
;;
