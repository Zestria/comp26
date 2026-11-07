[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Closure conversion for the MyLittleML middle-end (Task 2).

    Every [CFun] whose body mentions variables that are neither its
    parameters nor top-level globals gets those free variables prepended to
    its parameter list, and the original binding site is rewritten to apply
    the widened function to them immediately (eta-expansion):

    {[let f = fun x -> x + y in f 1]}
    becomes
    {[let cc_f0 = fun y x -> x + y in let f = cc_f0 y in f 1]}

    The rewrite is uniform for recursive and non-recursive bindings: the
    wrapper lives in the right-hand side of the very same [ALet] (which is
    why [Anf.ALet] carries an [aexp] rhs), so a recursive [name] keeps
    referring to the address the evaluator allocates for it (see [Eval]), and
    no use-site rewriting is needed.  Free variables keep their names and
    simply resolve to the fresh leading parameters.

    After this pass every lambda is closed: its free variables are a subset
    of its parameters plus top-level names (see {!is_closed}). *)
open Anf

(** Conversion state: [top] holds top-level [ADecl] names (treated as
    globals, never passed as free variables); [names] is the shared
    fresh-name generator ([cc_f<i>] for widened lambdas, [cc_t<i>] for
    application chain temporaries). *)
type state = Names.t

let make_state (prog : aprogram) : state =
  let add_names acc = function
    | ADecl (_, name, rhs) -> Names.StringSet.add name acc, rhs
    | AExp e -> acc, e
  in
  let top, rhss =
    List.fold_left
      (fun (top, rhss) item ->
         let top', rhs = add_names top item in
         top', rhs :: rhss)
      (Names.StringSet.empty, [])
      prog
  in
  let rec names_aexp acc = function
    | ACexp c -> names_cexp acc c
    | ALet (_, name, rhs, rest) ->
      names_aexp (names_aexp (Names.StringSet.add name acc) rest) rhs
  and names_cexp acc = function
    | CAtom (AVar v) -> Names.StringSet.add v acc
    | CAtom _ -> acc
    | CBinop (_, a, b) -> names_atom (names_atom acc a) b
    | CNeg a -> names_atom acc a
    | CApp (f, x) -> names_atom (names_atom acc f) x
    | CIf (a, t, f) -> names_aexp (names_aexp (names_atom acc a) t) f
    | CFun (params, body) ->
      names_aexp (List.fold_left (fun s v -> Names.StringSet.add v s) acc params) body
  and names_atom acc = function
    | AVar v -> Names.StringSet.add v acc
    | _ -> acc
  in
  let taken = List.fold_left names_aexp top rhss in
  Names.make taken
;;

(** Variables occurring free in an [aexp]: bound occurrences (lambda
    parameters, let-bound names — including a recursive name inside its own
    right-hand side) are excluded. *)
let free_vars (e : aexp) : Names.StringSet.t =
  let rec aexp bound = function
    | ACexp c -> cexp bound c
    | ALet (Rec, name, rhs, rest) ->
      let bound' = Names.StringSet.add name bound in
      Names.StringSet.union (aexp bound' rhs) (aexp bound' rest)
    | ALet (Nonrec, name, rhs, rest) ->
      let bound' = Names.StringSet.add name bound in
      Names.StringSet.union (aexp bound' rhs) (aexp bound' rest)
  and cexp bound = function
    | CAtom (AVar v) when Names.StringSet.mem v bound -> Names.StringSet.empty
    | CAtom (AVar v) -> Names.StringSet.singleton v
    | CAtom _ -> Names.StringSet.empty
    | CBinop (_, a, b) -> Names.StringSet.union (atom bound a) (atom bound b)
    | CNeg a -> atom bound a
    | CApp (f, x) -> Names.StringSet.union (atom bound f) (atom bound x)
    | CIf (c, t, f) ->
      Names.StringSet.union
        (atom bound c)
        (Names.StringSet.union (aexp bound t) (aexp bound f))
    | CFun (params, body) ->
      let bound' = List.fold_left (fun s v -> Names.StringSet.add v s) bound params in
      aexp bound' body
  and atom bound = function
    | AVar v when Names.StringSet.mem v bound -> Names.StringSet.empty
    | AVar v -> Names.StringSet.singleton v
    | _ -> Names.StringSet.empty
  in
  aexp Names.StringSet.empty e
;;

(** Free variables of a lambda body, excluding its parameters and (for
    recursive bindings) its own name. *)
let fv_of_lambda (params : string list) (self : string option) (body : aexp) : string list
  =
  let bound =
    List.fold_left (fun s v -> Names.StringSet.add v s) Names.StringSet.empty params
  in
  let bound =
    match self with
    | Some name -> Names.StringSet.add name bound
    | None -> bound
  in
  let fvs = free_vars body in
  Names.StringSet.diff fvs bound |> Names.StringSet.elements
;;

open State

let rec chain_rhs fn = function
  | [] -> return (ACexp (CAtom fn))
  | [ fv ] -> return (ACexp (CApp (fn, AVar fv)))
  | fv :: rest ->
    let* name = Names.fresh "cc_t" in
    let* rest = chain_rhs (AVar name) rest in
    return (ALet (Nonrec, name, ACexp (CApp (fn, AVar fv)), rest))
;;

let widen_rhs fvs params body =
  let* name = Names.fresh "cc_f" in
  let* rest = chain_rhs (AVar name) fvs in
  return (ALet (Nonrec, name, ACexp (CFun (fvs @ params, body)), rest))
;;

let rec convert_aexp = function
  | ACexp (CFun (params, body)) -> convert_lambda None params body
  | ACexp (CIf (c, t, f)) ->
    let* t = convert_aexp t in
    let* f = convert_aexp f in
    return (ACexp (CIf (c, t, f)))
  | ACexp c -> return (ACexp c)
  | ALet (flag, name, rhs, rest) ->
    let* rhs =
      match rhs with
      | ACexp (CFun (params, body)) ->
        convert_lambda
          (match flag with
           | Rec -> Some name
           | Nonrec -> None)
          params
          body
      | _ -> convert_aexp rhs
    in
    let* rest = convert_aexp rest in
    return (ALet (flag, name, rhs, rest))

and convert_lambda self params body =
  let* body = convert_aexp body in
  match fv_of_lambda params self body with
  | [] -> return (ACexp (CFun (params, body)))
  | fvs -> widen_rhs fvs params body
;;

let convert_prog prog =
  let item = function
    | ADecl (flag, name, rhs) ->
      let* rhs = convert_aexp rhs in
      return (ADecl (flag, name, rhs))
    | AExp e ->
      let* e = convert_aexp e in
      return (AExp e)
  in
  State.eval (State.map_list item prog) (make_state prog)
;;

(** After closure conversion every lambda is closed: each free variable of a
    [CFun] body is either one of its parameters, a top-level name, or a name
    bound by an enclosing recursive [ALet] (whose address is in scope wherever
    the lambda runs).  [test_mid] uses this as the CC invariant. *)
let is_closed (prog : aprogram) : bool =
  let top =
    List.fold_left
      (fun acc -> function
         | ADecl (_, name, _) -> Names.StringSet.add name acc
         | AExp _ -> acc)
      Names.StringSet.empty
      prog
  in
  let rec aexp_ok bound = function
    | ACexp c -> cexp_ok bound c
    | ALet (Rec, name, rhs, rest) ->
      let bound' = Names.StringSet.add name bound in
      aexp_ok bound' rhs && aexp_ok bound' rest
    | ALet (Nonrec, name, rhs, rest) ->
      aexp_ok bound rhs && aexp_ok (Names.StringSet.add name bound) rest
  and cexp_ok bound = function
    | CAtom a -> atom_ok bound a
    | CBinop (_, a, b) -> atom_ok bound a && atom_ok bound b
    | CNeg a -> atom_ok bound a
    | CApp (f, x) -> atom_ok bound f && atom_ok bound x
    | CIf (c, t, f) -> atom_ok bound c && aexp_ok bound t && aexp_ok bound f
    | CFun (params, body) ->
      let bound' = List.fold_left (fun s v -> Names.StringSet.add v s) bound params in
      aexp_ok bound' body
  and atom_ok bound = function
    | AVar v -> Names.StringSet.mem v bound || Names.StringSet.mem v top
    | _ -> true
  in
  List.for_all
    (function
      | ADecl (Rec, name, rhs) -> aexp_ok (Names.StringSet.singleton name) rhs
      | ADecl (Nonrec, _, rhs) -> aexp_ok Names.StringSet.empty rhs
      | AExp e -> aexp_ok Names.StringSet.empty e)
    prog
;;
