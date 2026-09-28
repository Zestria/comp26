[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib
module Env = Map.Make (String)

type builtin = Print_int

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VBuiltin of builtin
  | VClosure of closure

and closure =
  { self : string option
  ; param : string
  ; body : Ast.expr
  ; env : value Env.t
  }

type error =
  | Unbound_variable of string
  | Type_error of string
  | Division_by_zero

let ( let* ) = Result.bind

let binop op a b =
  match op, a, b with
  | Ast.Add, VInt x, VInt y -> Ok (VInt (x + y))
  | Ast.Sub, VInt x, VInt y -> Ok (VInt (x - y))
  | Ast.Mul, VInt x, VInt y -> Ok (VInt (x * y))
  | Ast.Div, VInt _, VInt 0 -> Error Division_by_zero
  | Ast.Div, VInt x, VInt y -> Ok (VInt (x / y))
  | Ast.Eq, VInt x, VInt y -> Ok (VBool (x = y))
  | Ast.Lt, VInt x, VInt y -> Ok (VBool (x < y))
  | Ast.Gt, VInt x, VInt y -> Ok (VBool (x > y))
  | Ast.Le, VInt x, VInt y -> Ok (VBool (x <= y))
  | Ast.Ge, VInt x, VInt y -> Ok (VBool (x >= y))
  | _ -> Error (Type_error "arithmetic or comparison on a non-integer")
;;

(* [out] collects what the program prints. *)
let rec eval_in out env = function
  | Ast.Const n -> Ok (VInt n)
  | Ast.Var x ->
    (match Env.find_opt x env with
     | Some v -> Ok v
     | None -> Error (Unbound_variable x))
  | Ast.Neg e ->
    let* v = eval_in out env e in
    (match v with
     | VInt n -> Ok (VInt (-n))
     | VBool _ | VUnit | VBuiltin _ | VClosure _ ->
       Error (Type_error "negation of a non-integer"))
  | Ast.BinOp (op, a, b) ->
    let* a = eval_in out env a in
    let* b = eval_in out env b in
    binop op a b
  | Ast.If (c, t, e) ->
    let* c = eval_in out env c in
    (match c with
     | VBool true -> eval_in out env t
     | VBool false -> eval_in out env e
     | VInt _ | VUnit | VBuiltin _ | VClosure _ ->
       Error (Type_error "condition is not a boolean"))
  | Ast.Abs (param, body) -> Ok (VClosure { self = None; param; body; env })
  | Ast.App (f, a) ->
    let* f = eval_in out env f in
    let* a = eval_in out env a in
    apply out f a
  | Ast.Let (x, e1, e2) ->
    let* v = eval_in out env e1 in
    eval_in out (Env.add x v env) e2
  | Ast.LetRec (f, param, body, e2) ->
    let closure = VClosure { self = Some f; param; body; env } in
    eval_in out (Env.add f closure env) e2

and apply out f arg =
  match f, arg with
  | VClosure ({ self; param; body; env } as closure), _ ->
    let env =
      match self with
      | Some name -> Env.add name (VClosure closure) env
      | None -> env
    in
    eval_in out (Env.add param arg env) body
  | VBuiltin Print_int, VInt n ->
    Buffer.add_string out (Int.to_string n);
    Ok VUnit
  | VBuiltin Print_int, _ -> Error (Type_error "print_int expects an integer")
  | (VInt _ | VBool _ | VUnit), _ -> Error (Type_error "application of a non-function")
;;

let builtins = Env.singleton "print_int" (VBuiltin Print_int)

type outcome =
  { output : string
  ; result : (value, error) result
  }

let run program =
  let out = Buffer.create 16 in
  let rec go env last = function
    | [] -> Ok last
    | Ast.DExpr e :: rest ->
      let* v = eval_in out env e in
      go env v rest
    | Ast.DLet (x, e) :: rest ->
      let* v = eval_in out env e in
      go (Env.add x v env) v rest
    | Ast.DLetRec (f, param, body) :: rest ->
      let v = VClosure { self = Some f; param; body; env } in
      go (Env.add f v env) v rest
  in
  let result = go builtins VUnit program in
  { output = Buffer.contents out; result }
;;

let eval e = (run [ Ast.DExpr e ]).result

let pp_value ppf = function
  | VInt n -> Format.fprintf ppf "%d" n
  | VBool b -> Format.fprintf ppf "%b" b
  | VUnit -> Format.fprintf ppf "()"
  | VBuiltin _ | VClosure _ -> Format.fprintf ppf "<fun>"
;;

let pp_error ppf = function
  | Unbound_variable x -> Format.fprintf ppf "unbound variable %s" x
  | Type_error msg -> Format.fprintf ppf "type error: %s" msg
  | Division_by_zero -> Format.fprintf ppf "division by zero"
;;
