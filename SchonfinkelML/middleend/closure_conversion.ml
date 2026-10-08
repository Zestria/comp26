[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
module StringSet = Anf.StringSet

let builtins = StringSet.of_list [ "+"; "-"; "*"; "/"; "<"; ">"; "="; "<="; ">=" ]

let rec collect_abstr = function
  | Abstr (x, body) ->
    let params, inner = collect_abstr body in
    x :: params, inner
  | e -> [], e
;;

let build_abstr params body = List.fold_right (fun p acc -> Abstr (p, acc)) params body
let apply_names f names = List.fold_left (fun acc n -> App (acc, Ident n)) f names

type ctx =
  { globals : StringSet.t
  ; subst : (string * expr) list
  }

let rec fv ctx = function
  | Ident x ->
    (match List.assoc_opt x ctx.subst with
     | Some r -> fv { ctx with subst = [] } r
     | None ->
       if StringSet.mem x ctx.globals then StringSet.empty else StringSet.singleton x)
  | Const _ -> StringSet.empty
  | App (a, b) -> StringSet.union (fv ctx a) (fv ctx b)
  | If (c, t, e) -> StringSet.union (fv ctx c) (StringSet.union (fv ctx t) (fv ctx e))
  | Abstr (x, body) -> StringSet.remove x (fv ctx body)
  | Let (NotRecursive, x, e1, e2) ->
    StringSet.union (fv ctx e1) (StringSet.remove x (fv ctx e2))
  | Let (Recursive, x, e1, e2) ->
    StringSet.remove x (StringSet.union (fv ctx e1) (fv ctx e2))
;;

type uniq_state =
  { naming : Anf.naming
  ; seen : StringSet.t
  }

let bind_fresh st x =
  if StringSet.mem x st.seen
  then (
    let x', naming = Anf.gensym st.naming ~prefix:(x ^ "_") () in
    x', { naming; seen = StringSet.add x' st.seen })
  else x, { st with seen = StringSet.add x st.seen }
;;

let rename env x =
  match List.assoc_opt x env with
  | Some x' -> x'
  | None -> x
;;

let rec uniquify st env = function
  | Ident x -> st, Ident (rename env x)
  | Const _ as e -> st, e
  | App (a, b) ->
    let st, a = uniquify st env a in
    let st, b = uniquify st env b in
    st, App (a, b)
  | If (c, t, e) ->
    let st, c = uniquify st env c in
    let st, t = uniquify st env t in
    let st, e = uniquify st env e in
    st, If (c, t, e)
  | Abstr (x, body) ->
    let x', st = bind_fresh st x in
    let st, body = uniquify st ((x, x') :: env) body in
    st, Abstr (x', body)
  | Let (flag, x, e1, e2) ->
    let x', st = bind_fresh st x in
    let env_rhs = if flag = Recursive then (x, x') :: env else env in
    let st, e1 = uniquify st env_rhs e1 in
    let st, e2 = uniquify st ((x, x') :: env) e2 in
    st, Let (flag, x', e1, e2)
;;

let rec convert ctx naming = function
  | Ident x as e ->
    (match List.assoc_opt x ctx.subst with
     | Some r -> naming, r
     | None -> naming, e)
  | Const _ as e -> naming, e
  | App (a, b) ->
    let naming, a = convert ctx naming a in
    let naming, b = convert ctx naming b in
    naming, App (a, b)
  | If (c, t, e) ->
    let naming, c = convert ctx naming c in
    let naming, t = convert ctx naming t in
    let naming, e = convert ctx naming e in
    naming, If (c, t, e)
  | Abstr _ as e ->
    let params, body = collect_abstr e in
    let naming, body = convert ctx naming body in
    let fvs =
      StringSet.diff (fv { ctx with subst = [] } body) (StringSet.of_list params)
      |> StringSet.elements
    in
    if fvs = []
    then naming, build_abstr params body
    else (
      let clo, naming = Anf.gensym naming ~prefix:"clo_" () in
      let extended = build_abstr (fvs @ params) body in
      let wrapper = apply_names (Ident clo) fvs in
      naming, Let (NotRecursive, clo, extended, wrapper))
  | Let (NotRecursive, x, e1, e2) ->
    let naming, e1 = convert ctx naming e1 in
    let naming, e2 = convert ctx naming e2 in
    naming, Let (NotRecursive, x, e1, e2)
  | Let (Recursive, f, rhs, rest) ->
    (match collect_abstr rhs with
     | [], _ ->
       let naming, rhs = convert ctx naming rhs in
       let naming, rest = convert ctx naming rest in
       naming, Let (Recursive, f, rhs, rest)
     | params, body ->
       let fvs = StringSet.elements (StringSet.remove f (fv ctx rhs)) in
       let self = apply_names (Ident f) fvs in
       let naming, body =
         convert { ctx with subst = (f, self) :: ctx.subst } naming body
       in
       let closed = build_abstr (fvs @ params) body in
       (match fvs with
        | [] ->
          let naming, rest = convert ctx naming rest in
          naming, Let (Recursive, f, closed, rest)
        | _ :: _ ->
          let clo, naming = Anf.gensym naming ~prefix:"clo_" () in
          let ctx_rest = { ctx with subst = (f, Ident clo) :: ctx.subst } in
          let naming, rest = convert ctx_rest naming rest in
          naming, Let (Recursive, f, closed, Let (NotRecursive, clo, self, rest))))
;;

let closure_convert ?(globals = []) e =
  let globals = StringSet.union builtins (StringSet.of_list globals) in
  let naming = Anf.naming_of (Anf.idents_of_expr globals e) in
  let st, e = uniquify { naming; seen = globals } [] e in
  let convert_naming = { st.naming with counter = 0 } in
  snd (convert { globals; subst = [] } convert_naming e)
;;

let closure_convert_program ?(globals = []) (p : program) : program =
  let top_names =
    List.filter_map
      (function
        | StrLet (_, name, _) -> Some name
        | StrEval _ -> None)
      p
  in
  let globals = StringSet.union builtins (StringSet.of_list (globals @ top_names)) in
  let reserved =
    List.fold_left
      (fun acc -> function
         | StrLet (_, _, e) | StrEval e -> Anf.idents_of_expr acc e)
      globals
      p
  in
  let ctx = { globals; subst = [] } in
  let convert_top naming e =
    let st, e = uniquify { naming; seen = globals } [] e in
    let convert_naming = { st.naming with counter = 0 } in
    convert ctx convert_naming e
  in
  let _, items =
    List.fold_left_map
      (fun naming -> function
         | StrLet (flag, name, e) ->
           let naming, e = convert_top naming e in
           naming, StrLet (flag, name, e)
         | StrEval e ->
           let naming, e = convert_top naming e in
           naming, StrEval e)
      (Anf.naming_of reserved)
      p
  in
  items
;;

let free_vars ?(globals = []) e =
  let globals = StringSet.union builtins (StringSet.of_list globals) in
  StringSet.elements (fv { globals; subst = [] } e)
;;
