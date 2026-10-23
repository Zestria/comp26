[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Reference evaluator for the MyLittleML middle-end (Task 2).

    {!eval_anf} covers the ANF, closure-converted and lifted programs (all
    three share {!Anf.aprogram}); {!eval_src} handles source {!Frontend.Ast}
    programs.  [test_mid] checks that CPS factorial and fibonacci evaluate
    to the same integers on every stage.

    Semantics: call-by-value; applications are single-argument
    ({!Frontend.Ast.App} is curried), multi-parameter functions build
    partial-application closures.  Top-level phrases thread one environment:
    declaration phrases extend it, expression phrases only read it.

    Recursion ties the knot through a mutable cell: a recursive binding is
    allocated first as a "seed" closure carrying [self_name]/[self_cell],
    its right-hand side is evaluated with the seed already in scope, and the
    resulting value is backpatched into the cell afterwards.  {!lookup}
    resolves seeds through the cell, so every use — including the wrapper
    closures closure conversion creates in the scope of the cell — sees the
    final value. *)
module A = Frontend.Ast
open Anf

exception Eval_error of string

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
      ; self_name : string option
      ; self_cell : value option ref
      }

and env = (string * value) list

(** Follow a seed's cell to the backpatched value; plain values pass
    through.  Only seeds ever have a filled cell. *)
let resolve (v : value) : value =
  match v with
  | VClos { self_cell; _ } ->
    (match !self_cell with
     | Some w -> w
     | None -> v)
  | VInt _ | VBool _ -> v
;;

let lookup (env : env) (v : string) : value =
  match List.assoc_opt v env with
  | Some value -> resolve value
  | None -> raise (Eval_error ("unbound variable: " ^ v))
;;

let string_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClos _ -> "<fun>"
;;

(** Structural equality on base values; closures are opaque and compare
    equal (stage results of [test_mid] are integers). *)
let equal (a : value) (b : value) : bool =
  match a, b with
  | VInt m, VInt n -> m = n
  | VBool x, VBool y -> x = y
  | VClos _, VClos _ -> true
  | _ -> false
;;

let make_clos (params : string list) (body : body) (env : env) : value =
  VClos { params; body; env; self_name = None; self_cell = ref None }
;;

(** Seed for a [let rec]: [params = []] so an application before
    backpatching fails loudly instead of running the dummy body. *)
let make_seed (name : string) (cell : value option ref) (env : env) : value =
  VClos
    { params = []
    ; body = Anf (ACexp (CAtom (AInt 0)))
    ; env
    ; self_name = Some name
    ; self_cell = cell
    }
;;

(* Primitive operations -------------------------------------------------- *)

let eval_bop (op : A.bop) (l : value) (r : value) : value =
  let ints = function
    | VInt m, VInt n -> m, n
    | _ -> raise (Eval_error "arithmetic on non-integers")
  in
  match op with
  | A.Add ->
    let m, n = ints (l, r) in
    VInt (m + n)
  | A.Sub ->
    let m, n = ints (l, r) in
    VInt (m - n)
  | A.Mul ->
    let m, n = ints (l, r) in
    VInt (m * n)
  | A.Div ->
    let m, n = ints (l, r) in
    if n = 0 then raise (Eval_error "division by zero") else VInt (m / n)
  | A.Mod ->
    let m, n = ints (l, r) in
    if n = 0 then raise (Eval_error "division by zero") else VInt (m mod n)
  | A.Lt ->
    let m, n = ints (l, r) in
    VBool (m < n)
  | A.Le ->
    let m, n = ints (l, r) in
    VBool (m <= n)
  | A.Gt ->
    let m, n = ints (l, r) in
    VBool (m > n)
  | A.Ge ->
    let m, n = ints (l, r) in
    VBool (m >= n)
  | A.Eq ->
    (match l, r with
     | VInt m, VInt n -> VBool (m = n)
     | VBool a, VBool b -> VBool (a = b)
     | _ -> raise (Eval_error "equality on non-integers"))
  | A.Neq ->
    (match l, r with
     | VInt m, VInt n -> VBool (m <> n)
     | VBool a, VBool b -> VBool (a <> b)
     | _ -> raise (Eval_error "equality on non-integers"))
;;


(* Mutual recursion: application runs bodies, bodies evaluate expressions,
   source expressions build closures. *)

(** Apply a closure to one argument.  A multi-parameter function applied to
    fewer arguments yields a partial-application closure carrying fresh
    (empty) self fields — deliberately not the seed's cell, so resolving
    does not loop back to the full closure. *)
let rec apply (fn : value) (arg : value) : value =
  match resolve fn with
  | VClos { params = p :: ps; body; env; _ } ->
    let env = (p, arg) :: env in
    (match ps with
     | [] -> eval_body body env
     | _ :: _ -> make_clos ps body env)
  | VClos { params = []; self_name; self_cell; _ } ->
    (match self_name, !self_cell with
     | Some name, None ->
       raise (Eval_error ("recursive value used before definition: " ^ name))
     | _ -> raise (Eval_error "too many arguments"))
  | VInt _ | VBool _ -> raise (Eval_error "application of a non-function")

and eval_body (b : body) (env : env) : value =
  match b with
  | Src e -> eval_src_expr env e
  | Anf e -> eval_aexp env e

(* Source AST ------------------------------------------------------------- *)

and eval_src_expr (env : env) (e : A.expr) : value =
  match e with
  | A.Int n -> VInt n
  | A.Bool b -> VBool b
  | A.Var v -> lookup env v
  | A.App (f, x) -> apply (eval_src_expr env f) (eval_src_expr env x)
  | A.Fun ([], body) -> eval_src_expr env body
  | A.Fun (params, body) -> make_clos params (Src body) env
  | A.Neg e ->
    (match eval_src_expr env e with
     | VInt n -> VInt (-n)
     | _ -> raise (Eval_error "negation of a non-integer"))
  | A.Binop (op, l, r) -> eval_bop op (eval_src_expr env l) (eval_src_expr env r)
  | A.If (c, t, f) ->
    (match eval_src_expr env c with
     | VBool true -> eval_src_expr env t
     | VBool false -> eval_src_expr env f
     | _ -> raise (Eval_error "condition is not a boolean"))
  | A.Let (b, cont) ->
    let env = bind_src env b in
    (match cont with
     | Some e -> eval_src_expr env e
     | None -> lookup env b.A.name)

(** Bind one source [let]; returns the extended environment.  A recursive
    binding goes through the seed/backpatch dance (see the module doc). *)
and bind_src (env : env) (b : A.bind) : env =
  let name = b.A.name in
  if not b.A.rec_
  then (
    let v =
      match b.A.params with
      | [] -> eval_src_expr env b.A.body
      | params -> make_clos params (Src b.A.body) env
    in
    (name, v) :: env)
  else (
    let cell = ref None in
    let seed = make_seed name cell env in
    let env = (name, seed) :: env in
    let v =
      match b.A.params with
      | [] -> eval_src_expr env b.A.body
      | params -> make_clos params (Src b.A.body) env
    in
    cell := Some v;
    env)

(* ANF ------------------------------------------------------------------- *)

and eval_atom (env : env) (a : atom) : value =
  match a with
  | AInt n -> VInt n
  | ABool b -> VBool b
  | AVar v -> lookup env v

and eval_aexp (env : env) (e : aexp) : value =
  match e with

  | ACexp c -> eval_cexp env c
  | ALet (Nonrec, name, rhs, rest) ->
    let v = eval_aexp env rhs in
    eval_aexp ((name, v) :: env) rest
  | ALet (Rec, name, rhs, rest) ->
    let cell = ref None in
    let seed = make_seed name cell env in
    let env = (name, seed) :: env in
    let v = eval_aexp env rhs in
    cell := Some v;
    eval_aexp env rest

and eval_cexp (env : env) (c : cexp) : value =
  match c with
  | CAtom a -> eval_atom env a
  | CBinop (op, l, r) -> eval_bop op (eval_atom env l) (eval_atom env r)
  | CNeg a ->
    (match eval_atom env a with
     | VInt n -> VInt (-n)
     | _ -> raise (Eval_error "negation of a non-integer"))
  | CApp (f, x) -> apply (eval_atom env f) (eval_atom env x)
  | CIf (cond, t, f) ->
    (match eval_atom env cond with
     | VBool true -> eval_aexp env t
     | VBool false -> eval_aexp env f
     | _ -> raise (Eval_error "condition is not a boolean"))
  | CFun (params, body) -> make_clos params (Anf body) env
;;

(* Whole programs --------------------------------------------------------- *)

let eval_src (prog : A.prog) : value =
  let eval_phrase (env, _) (phrase : A.expr) : env * value =
    match phrase with
    | A.Let (b, None) ->
      (* Declaration phrase: extend the environment, result is the bound value. *)
      let env = bind_src env b in
      env, lookup env b.A.name
    | e -> env, eval_src_expr env e
  in
  match prog with
  | [] -> raise (Eval_error "empty program")
  | first :: rest -> snd (List.fold_left eval_phrase ([], VInt 0) (first :: rest))
;;

let eval_anf (prog : aprogram) : value =
  (* Top-level names behave as globals: closure conversion treats them as
     free-of-capture ([Closure.make_state] [top]) and lambda lifting may
     hoist a lambda before the declaration it mentions.  So every [ADecl]
     name gets its seed cell up front, and the declaration backpatches it.

     Assumption: top-level names are unique (declaration shadowing would
     make the earlier value unreachable from closures; source programs of
     [test_mid] declare each name once). *)
  let top_seeds =
    List.filter_map
      (function
        | ADecl (_, name, _) -> Some (name, ref None)
        | AExp _ -> None)
      prog
  in
  let env0 : env = List.map (fun (name, cell) -> name, make_seed name cell []) top_seeds in
  let backpatch (name : string) (v : value) : unit =
    match List.assoc_opt name top_seeds with
    | Some cell -> cell := Some v
    | None -> ()
  in
  let eval_item (env, _) (item : item) : env * value =
    match item with
    | ADecl (flag, name, rhs) ->
      (* A declaration is a [let] whose continuation is just [name]. *)
      let v = eval_aexp env (ALet (flag, name, rhs, ACexp (CAtom (AVar name)))) in
      backpatch name v;
      env, v
    | AExp e -> env, eval_aexp env e
  in
  match prog with
  | [] -> raise (Eval_error "empty program")
  | first :: rest -> snd (List.fold_left eval_item (env0, VInt 0) (first :: rest))
;;


(** [eval_cc] evaluates a [prog] that has undergone closure conversion.
    After closure conversion, every [CFun] has been closed over its free
    variables, and the program can be evaluated like ANF but with the
    assurance that all [CFun] bodies are closed. *)

let eval_cc (prog : aprogram) : value =
  let top_seeds =
    List.filter_map
      (function
        | ADecl (_, name, _) -> Some (name, ref None)
        | AExp _ -> None)
      prog
  in
  let env0 : env = List.map (fun (name, cell) -> name, make_seed name cell []) top_seeds in
  let backpatch (name : string) (v : value) : unit =
    match List.assoc_opt name top_seeds with
    | Some cell -> cell := Some v
    | None -> ()
  in
  let rec eval_item (env, _) (item : item) : env * value =
    match item with
    | ADecl (flag, name, rhs) ->
      let v = eval_aexp env (ALet (flag, name, rhs, ACexp (CAtom (AVar name)))) in
      backpatch name v;
      env, v
    | AExp e -> env, eval_aexp env e
  in
  match prog with
  | [] -> raise (Eval_error "empty program")
  | first :: rest -> snd (List.fold_left eval_item (env0, VInt 0) (first :: rest))

(** [eval_ll] evaluates a [prog] that has undergone lambda lifting.
    After lambda lifting, all free variables of [CFun] bodies are either
    parameters or enclosing [ALet]-bound cells, so the program can be
    evaluated with a simplified environment. *)

let eval_ll (prog : aprogram) : value =
  let top_seeds =
    List.filter_map
      (function
        | ADecl (_, name, _) -> Some (name, ref None)
        | AExp _ -> None)
      prog
  in
  let env0 : env = List.map (fun (name, cell) -> name, make_seed name cell []) top_seeds in
  let backpatch (name : string) (v : value) : unit =
    match List.assoc_opt name top_seeds with
    | Some cell -> cell := Some v
    | None -> ()
  in
  let rec eval_item (env, _) (item : item) : env * value =
    match item with
    | ADecl (flag, name, rhs) ->
      let v = eval_aexp env (ALet (flag, name, rhs, ACexp (CAtom (AVar name)))) in
      backpatch name v;
      env, v
    | AExp e -> env, eval_aexp env e
  in
  match prog with
  | [] -> raise (Eval_error "empty program")
  | first :: rest -> snd (List.fold_left eval_item (env0, VInt 0) (first :: rest))
