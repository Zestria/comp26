[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common

(* Replace a variable by the name of a lifted function.  The name is fresh with
   respect to the whole expression, so no binder can capture it. *)
let rec subst x name = function
  | Ident y when String.equal x y -> Ident name
  | (Ident _ | Const _) as e -> e
  | App (a, b) -> App (subst x name a, subst x name b)
  | If (c, t, f) -> If (subst x name c, subst x name t, subst x name f)
  | Abstr (p, _) as e when String.equal p x -> e
  | Abstr (p, body) -> Abstr (p, subst x name body)
  | Let (Recursive, y, rhs, rest) ->
    let rhs = if String.equal y x then rhs else subst x name rhs in
    let rest = if String.equal y x then rest else subst x name rest in
    Let (Recursive, y, rhs, rest)
  | Let (NotRecursive, y, rhs, rest) ->
    Let
      ( NotRecursive
      , y
      , subst x name rhs
      , if String.equal y x then rest else subst x name rest )
;;

(* Lifted definitions may call each other, so they are emitted in the order they
   are created: a definition is always created after the definitions nested in
   it, hence it never refers to a definition emitted later.  The list is kept in
   reverse order of creation; a floated [let rec] keeps its recursive flag. *)
type state =
  { naming : Anf.naming
  ; defs : (let_flag * string * expr) list
  }

let rec lift_expr st = function
  | (Ident _ | Const _) as e -> st, e
  | App (a, b) ->
    let st, a = lift_expr st a in
    let st, b = lift_expr st b in
    st, App (a, b)
  | If (c, t, f) ->
    let st, c = lift_expr st c in
    let st, t = lift_expr st t in
    let st, f = lift_expr st f in
    st, If (c, t, f)
  | Abstr _ as e ->
    (* A definition is the chain of parameters with its body lifted; the
       abstraction itself is replaced by the name of the definition. *)
    let st, def = lift_chain st e in
    let name, naming = Anf.gensym st.naming ~prefix:"lift" () in
    { naming; defs = (NotRecursive, name, def) :: st.defs }, Ident name
  | Let (Recursive, f, rhs0, rest) ->
    let st, rhs = lift_chain st rhs0 in
    let st, rest = lift_expr st rest in
    (match rhs0 with
     | Abstr _ ->
       (* The chain is closed (closure conversion guarantees it) and its name
          stays bound inside the binding, so the whole [let rec] can be floated
          to the front of the expression: the function is created once instead
          of on every evaluation of this binding. *)
       { st with defs = (Recursive, f, rhs) :: st.defs }, rest
     | _ -> st, Let (Recursive, f, rhs, rest))
  | Let (NotRecursive, x, rhs0, rest) ->
    (* A right-hand side that becomes an abstraction is lifted; the binding that
       closure conversion introduced for it only names the lifted function, so
       it is substituted away instead of surviving as an extra indirection. *)
    let st, rhs = lift_expr st rhs0 in
    (match rhs0, rhs with
     | Abstr _, Ident name -> lift_expr st (subst x name rest)
     | _ ->
       let st, rest = lift_expr st rest in
       st, Let (NotRecursive, x, rhs, rest))

(* [lift_chain e] keeps the curried chain [fun p1 -> fun p2 -> ...] itself and
   lifts the abstractions nested inside its body.  A lifted definition needs it
   to become a single function instead of one partial application per parameter,
   and a recursive binding needs it to keep calling itself by name. *)
and lift_chain st = function
  | Abstr (p, body) ->
    let st, body = lift_chain st body in
    st, Abstr (p, body)
  | e -> lift_expr st e
;;

let lift e =
  let naming = Anf.naming_of (Anf.idents_of_expr Anf.StringSet.empty e) in
  let st, e = lift_expr { naming; defs = [] } e in
  List.fold_right
    (fun (flag, name, def) acc -> Let (flag, name, def, acc))
    (List.rev st.defs)
    e
;;
