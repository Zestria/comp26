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
    from the recursive cell the evaluator allocates for the binding; such
    lambdas are already closed by construction, so keeping them nested is
    harmless and preserves the recursion.

    After this pass the invariant of [test_mid] holds: [CFun] occurs only
    as a right-hand side of a top-level [ADecl] or of a nested
    [ALet (Rec, …)]. *)
open Anf

(** Lifting state: [taken] seeds fresh-name generation ([ll_f<i>]);
    [lifted] accumulates hoisted declarations in reverse order. *)
type state =
  { taken : StringSet.t
  ; mutable next : int
  ; mutable lifted : item list
  }

let make_state (prog : aprogram) : state =
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
  let names_item acc = function
    | ADecl (_, name, rhs) -> names_aexp (StringSet.add name acc) rhs
    | AExp e -> names_aexp acc e
  in
  let taken = List.fold_left names_item StringSet.empty prog in
  { taken; next = 0; lifted = [] }
;;

let fresh_ll (st : state) : string =
  let rec loop () =
    let name = "ll_f" ^ string_of_int st.next in
    st.next <- st.next + 1;
    if StringSet.mem name st.taken then loop () else name
  in
  loop ()
;;

(** Hoist one lambda: remember it as a top-level declaration, return the
    atom that replaces the original occurrence. *)
let hoist (st : state) (params : string list) (body : aexp) : atom =
  let name = fresh_ll st in
  st.lifted <- ADecl (Nonrec, name, ACexp (CFun (params, body))) :: st.lifted;
  AVar name
;;


let rec lift_aexp (st : state) : aexp -> aexp = function
  | ACexp (CFun (params, body)) ->
    let body' = lift_aexp st body in
    ACexp (CAtom (hoist st params body'))
  | ACexp c -> ACexp (lift_cexp st c)
  | ALet (Rec, name, rhs, rest) ->
    (* A nested recursive lambda keeps its place (see the module doc);
       lambdas inside its right-hand side and the continuation still lift. *)
    ALet (Rec, name, lift_rec_rhs st rhs, lift_aexp st rest)
  | ALet (Nonrec, name, rhs, rest) -> ALet (Nonrec, name, lift_aexp st rhs, lift_aexp st rest)

and lift_rec_rhs (st : state) : aexp -> aexp = function
  | ACexp (CFun (params, body)) -> ACexp (CFun (params, lift_aexp st body))
  | ACexp c -> ACexp (lift_cexp st c)
  | ALet (flag, name, rhs, rest) -> ALet (flag, name, lift_rec_rhs st rhs, lift_aexp st rest)

and lift_cexp (st : state) : cexp -> cexp = function
  | CIf (c, t, f) -> CIf (c, lift_aexp st t, lift_aexp st f)
  | (CAtom _ | CBinop _ | CNeg _ | CApp _) as c -> c
  | CFun _ ->
    (* Unreachable: both [lift_aexp] and the declaration handler intercept
       every [CFun] in [ACexp] position, so only fun-free cexps flow
       through here. *)
    assert false
;;

let lift_item (st : state) : item -> item list = function
  | ADecl (flag, name, rhs) ->
    (* A top-level lambda stays top-level under its own name; lambdas
       nested in its body still lift.  Other declaration shapes only get
       their nested lambdas hoisted. *)
    let rhs' =
      match rhs with
      | ACexp (CFun (params, body)) -> ACexp (CFun (params, lift_aexp st body))
      | _ -> lift_aexp st rhs
    in
    let hoisted = List.rev st.lifted in
    st.lifted <- [];
    hoisted @ [ ADecl (flag, name, rhs') ]
  | AExp e ->
    (* Hoisted declarations must land right before the expression item too,
       otherwise lambdas lifted out of it vanish from the program. *)
    let e' = lift_aexp st e in
    let hoisted = List.rev st.lifted in
    st.lifted <- [];
    hoisted @ [ AExp e' ]
;;

(** Lift a whole program.  Hoisted declarations are deterministic: fresh
    names come from a single counter seeded by all program names.  Note the
    declarations hoisted out of one item land right before it, so every use
    stays after its definition. *)
let lift_prog (prog : aprogram) : aprogram =
  let st = make_state prog in
  List.concat_map (lift_item st) prog
;;

(** After lambda lifting every [CFun] is either a top-level [ADecl]
    right-hand side, or the right-hand side of a nested [ALet (Rec, …)]
    (kept in place to preserve the recursive cell).  [test_mid] uses this
    as the LL invariant. *)
let is_lifted (prog : aprogram) : bool =
  let rec ok_aexp ~in_rec_rhs = function
    | ACexp c -> ok_cexp ~in_rec_rhs c
    | ALet (Rec, _, rhs, rest) ->
      ok_rec_rhs rhs && ok_aexp ~in_rec_rhs rest
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
