[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib
open Closure_ir
module Env = Map.Make (String)

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VBuiltin
  | VClosure of closure

and closure =
  { code : code
  ; fields : value array
  ; applied : value list
  }

type outcome =
  { output : string
  ; result : (value, Eval.error) result
  }

let ( let* ) = Result.bind
let type_error message = Error (Eval.Type_error message)

let immediate env = function
  | ImmInt n -> Ok (VInt n)
  | ImmVar x ->
    (match Env.find_opt x env with
     | Some value -> Ok value
     | None -> Error (Eval.Unbound_variable x))
;;

let rec immediates env = function
  | [] -> Ok []
  | i :: rest ->
    let* value = immediate env i in
    let* values = immediates env rest in
    Ok (value :: values)
;;

let binop op a b =
  match op, a, b with
  | Ast.Add, VInt a, VInt b -> Ok (VInt (a + b))
  | Ast.Sub, VInt a, VInt b -> Ok (VInt (a - b))
  | Ast.Mul, VInt a, VInt b -> Ok (VInt (a * b))
  | Ast.Div, VInt _, VInt 0 -> Error Eval.Division_by_zero
  | Ast.Div, VInt a, VInt b -> Ok (VInt (a / b))
  | Ast.Eq, VInt a, VInt b -> Ok (VBool (a = b))
  | Ast.Lt, VInt a, VInt b -> Ok (VBool (a < b))
  | Ast.Gt, VInt a, VInt b -> Ok (VBool (a > b))
  | Ast.Le, VInt a, VInt b -> Ok (VBool (a <= b))
  | Ast.Ge, VInt a, VInt b -> Ok (VBool (a >= b))
  | _ -> type_error "arithmetic or comparison on a non-integer"
;;

let condition env = function
  | CondImm i -> immediate env i
  | CondCmp (op, a, b) ->
    let* a = immediate env a in
    let* b = immediate env b in
    binop op a b
;;

let rec expression out env = function
  | AExpr c -> complex out env c
  | ALet (x, c, rest) ->
    let* value = complex out env c in
    expression out (Env.add x value env) rest

and complex out env = function
  | CImm i -> immediate env i
  | CNeg i ->
    let* v = immediate env i in
    (match v with
     | VInt n -> Ok (VInt (-n))
     | _ -> type_error "negation of a non-integer")
  | CBinOp (op, a, b) ->
    let* a = immediate env a in
    let* b = immediate env b in
    binop op a b
  | CIf (c, t, e) ->
    let* c = condition env c in
    (match c with
     | VBool b -> expression out env (if b then t else e)
     | _ -> type_error "condition is not a boolean")
  | MakeClosure (code, fields) ->
    let* fields = immediates env fields in
    Ok (VClosure { code; fields = Array.of_list fields; applied = [] })
  | GetEnv (self, i) ->
    let* self = immediate env (ImmVar self) in
    (match self with
     | VClosure c when i >= 0 && i < Array.length c.fields -> Ok c.fields.(i)
     | _ -> type_error "invalid environment access")
  | ApplyClosure (f, args) ->
    let* f = immediate env f in
    apply_arguments out env f args

and apply_arguments out env f = function
  | [] -> Ok f
  | arg :: rest ->
    let* arg = immediate env arg in
    let* f = apply out f arg in
    apply_arguments out env f rest

and apply out f arg =
  match f, arg with
  | VBuiltin, VInt n ->
    Buffer.add_string out (Int.to_string n);
    Ok VUnit
  | VBuiltin, _ -> type_error "print_int expects an integer"
  | VClosure closure, _ ->
    let applied = closure.applied @ [ arg ] in
    if List.length applied < List.length closure.code.params
    then Ok (VClosure { closure with applied })
    else if List.length applied = List.length closure.code.params
    then (
      let self = VClosure { closure with applied = [] } in
      let env = Env.singleton closure.code.self self in
      let env =
        List.fold_left2
          (fun env p value -> Env.add p value env)
          env
          closure.code.params
          applied
      in
      expression out env closure.code.body)
    else type_error "invalid closure arity"
  | _ -> type_error "application of a non-function"
;;

let run program =
  let out = Buffer.create 16 in
  let rec loop env last = function
    | [] -> Ok last
    | DExpr e :: rest ->
      let* value = expression out env e in
      loop env value rest
    | DLet (x, e) :: rest ->
      let* value = expression out env e in
      loop (Env.add x value env) value rest
  in
  let result = loop (Env.singleton "print_int" VBuiltin) VUnit program in
  { output = Buffer.contents out; result }
;;

let pp_value ppf = function
  | VInt n -> Format.fprintf ppf "%d" n
  | VBool b -> Format.fprintf ppf "%b" b
  | VUnit -> Format.pp_print_string ppf "()"
  | VBuiltin | VClosure _ -> Format.pp_print_string ppf "<fun>"
;;
