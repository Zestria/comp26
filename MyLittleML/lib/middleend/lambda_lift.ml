[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Lambda lifting for the MyLittleML middle-end (Task 2).

    Runs after closure conversion: every lambda in the program is therefore closed — its free
    variables are a subset of its parameters, top-level names, and names
    bound by an enclosing recursive [ALet] (see [Closure.is_closed]) — so
    hoisting a lambda to the top level never breaks scope.

    Every [CFun] that is not already a top-level [ADecl] right-hand side
    moves to the top level as [ADecl (Nonrec, ll_f<i>, …)], and the original
    occurrence becomes a bare reference to it:

    {[let t = fun x -> x + 1 in t 41]}
    becomes
    {[let ll_f0 = fun x -> x + 1
      let t = ll_f0 in t 41]}

    One exception: a [CFun] that is the right-hand side of a *nested*
    [ALet (Rec, …)] is left in place.  Hoisting it would detach the body
    from the recursive address the evaluator allocates for the binding; such
    lambdas are already closed by construction, so keeping them nested is
    harmless and preserves the recursion.

    After this pass the invariant of [test_mid] holds: [CFun] occurs only
    as a right-hand side of a top-level [ADecl] or of a nested
    [ALet (Rec, …)]. *)
open Anf

(** Lifting state: [names] is the shared fresh-name generator ([ll_f<i>]);
    [lifted] accumulates hoisted declarations in reverse order. *)
type state =
  { names : Names.t
  ; lifted : item list
  }

let make_state (prog : aprogram) : state =
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
  let names_item acc = function
    | ADecl (_, name, rhs) -> names_aexp (Names.StringSet.add name acc) rhs
    | AExp e -> names_aexp acc e
  in
  let taken = List.fold_left names_item Names.StringSet.empty prog in
  { names = Names.make taken; lifted = [] }
;;

open State

let hoist params body =
  let* state = get in
  let name, names = Names.fresh "ll_f" state.names in
  let* () =
    put
      { names
      ; lifted = ADecl (Nonrec, name, ACexp (CFun (params, body))) :: state.lifted
      }
  in
  return (ACexp (CAtom (AVar name)))
;;

let rec lift_aexp = function
  | ACexp (CFun (params, body)) ->
    let* body = lift_aexp body in
    hoist params body
  | ACexp (CIf (c, t, f)) ->
    let* t = lift_aexp t in
    let* f = lift_aexp f in
    return (ACexp (CIf (c, t, f)))
  | ACexp c -> return (ACexp c)
  | ALet (flag, name, rhs, rest) ->
    let* rhs =
      match flag with
      | Rec -> lift_rec_rhs rhs
      | Nonrec -> lift_aexp rhs
    in
    let* rest = lift_aexp rest in
    return (ALet (flag, name, rhs, rest))

and lift_rec_rhs = function
  | ACexp (CFun (params, body)) ->
    let* body = lift_aexp body in
    return (ACexp (CFun (params, body)))
  | ALet (flag, name, rhs, rest) ->
    let* rhs = lift_rec_rhs rhs in
    let* rest = lift_aexp rest in
    return (ALet (flag, name, rhs, rest))
  | e -> lift_aexp e
;;

let lift_item item =
  let* item =
    match item with
    | ADecl (flag, name, rhs) ->
      let* rhs =
        match rhs with
        | ACexp (CFun (params, body)) ->
          let* body = lift_aexp body in
          return (ACexp (CFun (params, body)))
        | _ -> lift_aexp rhs
      in
      return (ADecl (flag, name, rhs))
    | AExp e ->
      let* e = lift_aexp e in
      return (AExp e)
  in
  let* state = get in
  let* () = put { state with lifted = [] } in
  return (List.rev state.lifted @ [ item ])
;;

let lift_prog prog =
  State.eval (State.map_list lift_item prog) (make_state prog) |> List.concat
;;

(** After lambda lifting every [CFun] is either a top-level [ADecl]
    right-hand side, or the right-hand side of a nested [ALet (Rec, …)]
    (kept in place to preserve the recursive address).  [test_mid] uses this
    as the LL invariant. *)
let is_lifted (prog : aprogram) : bool =
  let rec ok_aexp ~in_rec_rhs = function
    | ACexp c -> ok_cexp ~in_rec_rhs c
    | ALet (Rec, _, rhs, rest) -> ok_rec_rhs rhs && ok_aexp ~in_rec_rhs rest
    | ALet (Nonrec, _, rhs, rest) -> ok_aexp ~in_rec_rhs rhs && ok_aexp ~in_rec_rhs rest
  and ok_rec_rhs = function
    | ACexp (CFun (_, body)) -> ok_aexp ~in_rec_rhs:true body
    | ACexp c -> ok_cexp_nested c
    | ALet (_, _, rhs, rest) -> ok_rec_rhs rhs && ok_aexp ~in_rec_rhs:true rest
  and ok_cexp ~in_rec_rhs = function
    | CAtom _ | CBinop _ | CNeg _ | CApp _ -> true
    | CIf (_, t, f) -> ok_aexp ~in_rec_rhs t && ok_aexp ~in_rec_rhs f
    | CFun _ -> in_rec_rhs
  and ok_cexp_nested = function
    | CAtom _ | CBinop _ | CNeg _ | CApp _ -> true
    | CIf (_, t, f) -> ok_rec_rhs_branch t && ok_rec_rhs_branch f
    | CFun _ -> true
  and ok_rec_rhs_branch = function
    | ACexp (CFun (_, body)) -> ok_aexp ~in_rec_rhs:true body
    | e -> ok_aexp ~in_rec_rhs:true e
  in
  List.for_all
    (function
      | ADecl (_, _, ACexp (CFun (_, body))) -> ok_aexp ~in_rec_rhs:false body
      | ADecl (_, _, rhs) -> ok_aexp ~in_rec_rhs:false rhs
      | AExp e -> ok_aexp ~in_rec_rhs:false e)
    prog
;;
