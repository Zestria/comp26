[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Ast
open Anf

type body =
  | Src of A.expr
  | Anf of aexp

type value =
  | VInt of int
  | VBool of bool
  | VClos of
      { params : string list
      ; body : body
      ; env : env
      }
  | VAddr of int

and env = (string * value) list

(** Recursive bindings are addresses in a persistent map. Updating a binding
    produces a new store passed by State_error, never a shared heap cell.
    Closures capture addresses, so recursive and lifted forward references
    observe definitions in the current store while retaining lexical scope. *)
module Store = Map.Make (Int)

module Addresses = Set.Make (Int)

type state =
  { next : int
  ; store : value Store.t
  }

open State_error

let initial = { next = 0; store = Store.empty }

let allocate =
  let* state = get in
  let* () = put { state with next = state.next + 1 } in
  return (VAddr state.next)
;;

let define address value =
  match address with
  | VAddr id ->
    modify (fun state -> { state with store = Store.add id value state.store })
  | _ -> fail "internal error: binding without an address"
;;

let resolve value =
  let* state = get in
  let rec follow visited = function
    | VAddr id when Addresses.mem id visited -> fail "cyclic non-function value"
    | VAddr id ->
      (match Store.find_opt id state.store with
       | None -> fail "recursive value used before definition"
       | Some value -> follow (Addresses.add id visited) value)
    | value -> return value
  in
  follow Addresses.empty value
;;

let lookup env name =
  match List.assoc_opt name env with
  | None -> fail ("unbound variable: " ^ name)
  | Some value -> return value
;;

let closure params body env = VClos { params; body; env }

let eval_bop op left right =
  let* left = resolve left in
  let* right = resolve right in
  match op, left, right with
  | (A.Eq | A.Neq), VBool a, VBool b ->
    return (VBool (if op = A.Eq then a = b else a <> b))
  | _, VInt a, VInt b ->
    (match op with
     | A.Add -> return (VInt (a + b))
     | A.Sub -> return (VInt (a - b))
     | A.Mul -> return (VInt (a * b))
     | (A.Div | A.Mod) when b = 0 -> fail "division by zero"
     | A.Div -> return (VInt (a / b))
     | A.Mod -> return (VInt (a mod b))
     | A.Lt -> return (VBool (a < b))
     | A.Le -> return (VBool (a <= b))
     | A.Gt -> return (VBool (a > b))
     | A.Ge -> return (VBool (a >= b))
     | A.Eq -> return (VBool (a = b))
     | A.Neq -> return (VBool (a <> b)))
  | _ -> fail "operator applied to incompatible values"
;;

let negate value =
  let* value = resolve value in
  match value with
  | VInt n -> return (VInt (-n))
  | _ -> fail "negation of a non-integer"
;;

let branch value yes no =
  let* value = resolve value in
  match value with
  | VBool true -> yes
  | VBool false -> no
  | _ -> fail "condition is not a boolean"
;;

let rec apply fn arg =
  let* fn = resolve fn in
  match fn with
  | VClos { params = p :: ps; body; env } ->
    let env = (p, arg) :: env in
    (match ps with
     | [] -> eval_body env body
     | _ -> return (closure ps body env))
  | VClos { params = []; _ } -> fail "function without parameters"
  | _ -> fail "application of a non-function"

and eval_body env = function
  | Src e -> eval_src_expr env e
  | Anf e -> eval_aexp env e

and eval_src_expr env = function
  | A.Int n -> return (VInt n)
  | A.Bool b -> return (VBool b)
  | A.Var name -> lookup env name
  | A.App (f, x) ->
    let* f = eval_src_expr env f in
    let* x = eval_src_expr env x in
    apply f x
  | A.Fun ([], body) -> eval_src_expr env body
  | A.Fun (params, body) -> return (closure params (Src body) env)
  | A.Neg e ->
    let* value = eval_src_expr env e in
    negate value
  | A.Binop (op, l, r) ->
    let* l = eval_src_expr env l in
    let* r = eval_src_expr env r in
    eval_bop op l r
  | A.If (c, t, f) ->
    let* c = eval_src_expr env c in
    branch c (eval_src_expr env t) (eval_src_expr env f)
  | A.Let (b, cont) ->
    let* env = bind_src env b in
    (match cont with
     | None -> lookup env b.A.name
     | Some e -> eval_src_expr env e)

and bind_src env b =
  let eval env =
    match b.A.params with
    | [] -> eval_src_expr env b.A.body
    | params -> return (closure params (Src b.A.body) env)
  in
  if b.A.rec_
  then
    let* address = allocate in
    let env = (b.A.name, address) :: env in
    let* value = eval env in
    let* () = define address value in
    return env
  else
    let* value = eval env in
    return ((b.A.name, value) :: env)

and eval_atom env = function
  | AInt n -> return (VInt n)
  | ABool b -> return (VBool b)
  | AVar name -> lookup env name

and eval_aexp env = function
  | ACexp c -> eval_cexp env c
  | ALet (Nonrec, name, rhs, rest) ->
    let* value = eval_aexp env rhs in
    eval_aexp ((name, value) :: env) rest
  | ALet (Rec, name, rhs, rest) ->
    let* address = allocate in
    let env = (name, address) :: env in
    let* value = eval_aexp env rhs in
    let* () = define address value in
    eval_aexp env rest

and eval_cexp env = function
  | CAtom a -> eval_atom env a
  | CNeg a ->
    let* value = eval_atom env a in
    negate value
  | CBinop (op, l, r) ->
    let* l = eval_atom env l in
    let* r = eval_atom env r in
    eval_bop op l r
  | CApp (f, x) ->
    let* f = eval_atom env f in
    let* x = eval_atom env x in
    apply f x
  | CIf (c, t, f) ->
    let* c = eval_atom env c in
    branch c (eval_aexp env t) (eval_aexp env f)
  | CFun (params, body) -> return (closure params (Anf body) env)
;;

let eval_src prog =
  let rec loop env last = function
    | [] -> resolve last
    | A.Let (b, None) :: rest ->
      let* env = bind_src env b in
      let* value = lookup env b.A.name in
      loop env value rest
    | e :: rest ->
      let* value = eval_src_expr env e in
      loop env value rest
  in
  match prog with
  | [] -> Error "empty program"
  | _ -> eval (loop [] (VInt 0) prog) initial
;;

let eval_mid prog =
  let reserve = function
    | ADecl (_, name, _) as item ->
      let* address = allocate in
      return (item, Some (name, address))
    | item -> return (item, None)
  in
  let computation =
    let* items = map_list reserve prog in
    let env = List.filter_map snd items in
    let rec loop env last = function
      | [] -> resolve last
      | (ADecl (flag, name, rhs), Some (_, address)) :: rest ->
        let rhs_env =
          match flag with
          | Rec -> (name, address) :: env
          | Nonrec -> env
        in
        let* value = eval_aexp rhs_env rhs in
        let* () = define address value in
        loop ((name, address) :: env) value rest
      | (AExp e, _) :: rest ->
        let* value = eval_aexp env e in
        loop env value rest
      | _ -> fail "internal error: unreserved top-level binding"
    in
    loop env (VInt 0) items
  in
  match prog with
  | [] -> Error "empty program"
  | _ -> eval computation initial
;;

let eval_anf = eval_mid
let eval_cc = eval_mid
let eval_ll = eval_mid
