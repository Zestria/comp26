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
    referring to the cell the evaluator allocates for it (see [Eval]), and
    no use-site rewriting is needed.  Free variables keep their names and
    simply resolve to the fresh leading parameters.

    After this pass every lambda is closed: its free variables are a subset
    of its parameters plus top-level names (see {!is_closed}). *)
open Anf

(** Conversion state: [top] holds top-level [ADecl] names (treated as
    globals, never passed as free variables); [taken] seeds fresh-name
    generation ([cc_f<i>] for widened lambdas, [cc_t<i>] for application
    chain temporaries). *)
type state =
  { top : StringSet.t
  ; taken : StringSet.t
  ; mutable next : int
  }

let make_state (prog : aprogram) : state =
  let add_names acc = function
    | ADecl (_, name, rhs) -> StringSet.add name acc, rhs
    | AExp _ -> acc, ACexp (CAtom (AInt 0))
  in
  let top, rhss =
    List.fold_left
      (fun (top, rhss) item ->
        let top', rhs = add_names top item in
        top', rhs :: rhss)
      (StringSet.empty, [])
      prog
  in
  let rec names_aexp acc = function
    | ACexp c -> names_cexp acc c
    | ALet (_, name, rhs, rest) -> names_aexp (names_aexp (StringSet.add name acc) rest) rhs
  and names_cexp acc = function
    | CAtom (AVar v) -> StringSet.add v acc
    | CAtom _ -> acc
    | CBinop (_, a, b) -> names_atom (names_atom acc a) b
    | CNeg a -> names_atom acc a
    | CApp (f, x) -> names_atom (names_atom acc f) x
    | CIf (a, t, f) -> names_aexp (names_aexp (names_atom acc a) t) f
    | CFun (params, body) ->
      names_aexp (List.fold_left (fun s v -> StringSet.add v s) acc params) body
  and names_atom acc = function
    | AVar v -> StringSet.add v acc
    | _ -> acc
  in
  let taken = List.fold_left names_aexp top rhss in
  { top; taken; next = 0 }
;;

let fresh_cc (st : state) : string =
  let rec loop () =
    let name = "cc_f" ^ string_of_int st.next in
    st.next <- st.next + 1;
    if StringSet.mem name st.taken then loop () else name
  in
  loop ()
;;

let fresh_tmp (st : state) : string =
  let rec loop () =
    let name = "cc_t" ^ string_of_int st.next in
    st.next <- st.next + 1;
    if StringSet.mem name st.taken then loop () else name
  in
  loop ()
;;

(** Variables occurring free in an [aexp]: bound occurrences (lambda
    parameters, let-bound names — including a recursive name inside its own
    right-hand side) are excluded. *)
let free_vars (e : aexp) : StringSet.t =
  let rec aexp bound = function
    | ACexp c -> cexp bound c
    | ALet (Rec, name, rhs, rest) ->
      let bound' = StringSet.add name bound in
      StringSet.union (aexp bound' rhs) (aexp bound' rest)
    | ALet (Nonrec, name, rhs, rest) ->
      StringSet.union (aexp bound rhs) (aexp (StringSet.add name bound) rest)
  and cexp bound = function
    | CAtom a -> atom bound a
    | CBinop (_, a, b) -> StringSet.union (atom bound a) (atom bound b)
    | CNeg a -> atom bound a
    | CApp (f, x) -> StringSet.union (atom bound f) (atom bound x)
    | CIf (c, t, f) -> StringSet.union (atom bound c) (StringSet.union (aexp bound t) (aexp bound f))
    | CFun (params, body) ->
      let bound' = List.fold_left (fun s v -> StringSet.add v s) bound params in
      aexp bound' body
  and atom bound = function
    | AVar v -> if StringSet.mem v bound then StringSet.empty else StringSet.singleton v
    | _ -> StringSet.empty
  in
  aexp StringSet.empty e
;;

(** Free variables of a lambda body that must become extra parameters:
    sorted for deterministic output, minus parameters, minus the recursive
    name (if any), minus top-level globals. *)
let fv_of_lambda (st : state) (params : string list) (self : string option) (body : aexp)
  : string list
  =
  let bound =
    List.fold_left
      (fun s v -> StringSet.add v s)
      StringSet.empty
      (match self with
       | None -> params
       | Some name -> name :: params)
  in
  free_vars body
  |> (fun fvs -> StringSet.diff fvs bound)
  |> (fun fvs -> StringSet.diff fvs st.top)
  |> StringSet.elements
;;

(** [chain_rhs st prev fvs] builds the partial application of the atom [prev]
    to each variable of [fvs], left to right, as an [aexp] evaluating to the
    resulting closure: [let t1 = prev fv1 in … ACexp (CApp (t_last, fvn))].
    With a single variable there is no temporary at all. *)
let rec chain_rhs (st : state) (prev : string) : string list -> aexp = function
  | [] -> ACexp (CAtom (AVar prev))
  | [ v ] -> ACexp (CApp (AVar prev, AVar v))
  | v :: vs ->
    let t = fresh_tmp st in
    ALet (Nonrec, t, ACexp (CApp (AVar prev, AVar v)), chain_rhs st t vs)
;;

(** Wrap a lambda with free variables [fvs] into a widened [cc_f<i>] taking
    [fvs @ params], immediately partially applied to [fvs].  The wrapper
    lives in the right-hand side of the very same binding (which is why
    [Anf.ALet] carries an [aexp] rhs): for a recursive binding the wrapper
    closure is therefore created in the scope where the recursive cell
    already exists, so the recursive [name] keeps resolving through its own
    cell (see [Eval]) and no use-site rewriting is needed.  Free variables
    keep their names and resolve to the fresh leading parameters. *)
let widen_rhs (st : state) (fvs : string list) (params : string list) (body : aexp) : aexp =
  let fresh = fresh_cc st in
  ALet (Nonrec, fresh, ACexp (CFun (fvs @ params, body)), chain_rhs st fresh fvs)
;;

let rec convert_aexp (st : state) : aexp -> aexp = function
  | ACexp (CFun (params, body)) ->
    let body' = convert_aexp st body in
    (match fv_of_lambda st params None body' with
     | [] -> ACexp (CFun (params, body'))
     | fvs -> widen_rhs st fvs params body')
  | ACexp c -> ACexp (convert_cexp st c)
  | ALet (flag, name, rhs, rest) ->
    (match rhs with
     | ACexp (CFun (params, body)) ->
       let self = match flag with Rec -> Some name | Nonrec -> None in
       let body' = convert_aexp st body in
       (match fv_of_lambda st params self body' with
        | [] -> ALet (flag, name, ACexp (CFun (params, body')), convert_aexp st rest)
        | fvs -> ALet (flag, name, widen_rhs st fvs params body', convert_aexp st rest))
     | _ -> ALet (flag, name, convert_aexp st rhs, convert_aexp st rest))

and convert_cexp (st : state) : cexp -> cexp = function
  | CIf (c, t, f) -> CIf (c, convert_aexp st t, convert_aexp st f)
  | CFun _ ->
    (* Unreachable: [convert_aexp] intercepts every [CFun] in [ACexp]
       position, so only fun-free cexps flow through here. *)
    assert false
  | (CAtom _ | CBinop _ | CNeg _ | CApp _) as c -> c
;;

let convert_item (st : state) : item -> item = function
  | ADecl (flag, name, rhs) -> ADecl (flag, name, convert_aexp st rhs)
  | AExp e -> AExp (convert_aexp st e)
;;

(** Convert a whole program.  Conversion state (top-level names, taken
    names) is computed once up front, so fresh names are deterministic. *)
let convert_prog (prog : aprogram) : aprogram =
  let st = make_state prog in
  List.map (convert_item st) prog
;;

(** After closure conversion every lambda is closed: each free variable of a
    [CFun] body is either one of its parameters, a top-level name, or a name
    bound by an enclosing recursive [ALet] (whose cell is in scope wherever
    the lambda runs).  [test_mid] uses this as the CC invariant. *)
let is_closed (prog : aprogram) : bool =
  let top =
    List.fold_left
      (fun acc -> function
        | ADecl (_, name, _) -> StringSet.add name acc
        | AExp _ -> acc)
      StringSet.empty
      prog
  in
  let rec aexp_ok bound = function
    | ACexp c -> cexp_ok bound c
    | ALet (Rec, name, rhs, rest) ->
      let bound' = StringSet.add name bound in
      aexp_ok bound' rhs && aexp_ok bound' rest
    | ALet (Nonrec, name, rhs, rest) ->
      aexp_ok bound rhs && aexp_ok (StringSet.add name bound) rest
  and cexp_ok bound = function
    | CAtom a -> atom_ok bound a
    | CBinop (_, a, b) -> atom_ok bound a && atom_ok bound b
    | CNeg a -> atom_ok bound a
    | CApp (f, x) -> atom_ok bound f && atom_ok bound x
    | CIf (c, t, f) -> atom_ok bound c && aexp_ok bound t && aexp_ok bound f
    | CFun (params, body) ->
      let bound' = List.fold_left (fun s v -> StringSet.add v s) bound params in
      aexp_ok bound' body
  and atom_ok bound = function
    | AVar v -> StringSet.mem v bound || StringSet.mem v top
    | _ -> true
  in
  List.for_all
    (function
      | ADecl (Rec, name, rhs) -> aexp_ok (StringSet.singleton name) rhs
      | ADecl (Nonrec, _, rhs) -> aexp_ok StringSet.empty rhs
      | AExp e -> aexp_ok StringSet.empty e)
    prog
;;
