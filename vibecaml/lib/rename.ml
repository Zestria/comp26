[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

exception Rename_error of string

(** Множество уже занятых во всей программе имён. *)
module Names = Set.Make (String)

let lookup env name = Option.value ~default:name (List.assoc_opt name env)

(* Согласно договоренности, переименовываем каждое пользовательское имя,
содержащее "__" *)
let should_rename name =
  let length = String.length name in
  let rec helper i =
    i < length - 1 && ((name.[i] = '_' && name.[i + 1] = '_') || helper (i + 1))
  in
  helper 0
;;

let rename_binding used env name =
  let operator = List.mem name (Lexicon.operators @ [ "~-" ]) in
  let name' =
    if Names.mem name used || should_rename name || operator
    then Fresh.name (if operator then "op" else name)
    else name
  in
  name', (name, name') :: env, Names.add name' used
;;

let rec rename_pattern used env = function
  | (Pat_wildcard | Pat_constant _) as pat -> pat, env, used
  | Pat_var name ->
    let name', env, used = rename_binding used env name in
    Pat_var name', env, used
  | Pat_tuple (fst, snd, rest) ->
    let fst', env, used = rename_pattern used env fst in
    let snd', env, used = rename_pattern used env snd in
    let rest', env, used = rename_patterns used env rest in
    Pat_tuple (fst', snd', rest'), env, used

and rename_patterns used env = function
  | [] -> [], env, used
  | pat :: rest ->
    let pat', env, used = rename_pattern used env pat in
    let rest', env, used = rename_patterns used env rest in
    pat' :: rest', env, used
;;

let rec rename_expr used env = function
  | Expr_constant c -> Expr_constant c, used
  | Expr_ident id -> Expr_ident (lookup env id), used
  | Expr_let (flag, first, rest, body) ->
    let bindings, env', used = rename_bindings used env flag (first :: rest) in
    let body, used = rename_expr used env' body in
    (match bindings with
     | first :: rest -> Expr_let (flag, first, rest, body), used
     | [] -> raise (Rename_error "Rename: пустая группа let"))
  | Expr_fun (first, rest, body) ->
    let first, env, used = rename_pattern used env first in
    let rest, env, used = rename_patterns used env rest in
    let body, used = rename_expr used env body in
    Expr_fun (first, rest, body), used
  | Expr_app (f, a) ->
    let f, used = rename_expr used env f in
    let a, used = rename_expr used env a in
    Expr_app (f, a), used
  | Expr_if (c, t, e) ->
    let c, used = rename_expr used env c in
    let t, used = rename_expr used env t in
    let e, used = rename_expr used env e in
    Expr_if (c, t, e), used
  | Expr_tuple (first, second, rest) ->
    let first, used = rename_expr used env first in
    let second, used = rename_expr used env second in
    let rest, used = rename_exprs used env rest in
    Expr_tuple (first, second, rest), used

and rename_exprs used env = function
  | [] -> [], used
  | expr :: rest ->
    let expr, used = rename_expr used env expr in
    let rest, used = rename_exprs used env rest in
    expr :: rest, used

and rename_bindings used env flag bindings =
  let pat = List.map (fun b -> b.binding_pattern) bindings in
  let pat', env', used = rename_patterns used env pat in
  let rhs_env =
    match flag with
    | Non_recursive -> env
    | Recursive -> env'
  in
  let rec rename_rhs used = function
    | [] -> [], used
    | (pattern, binding) :: rest ->
      let expression, used = rename_expr used rhs_env binding.binding_expression in
      let rest, used = rename_rhs used rest in
      { binding_pattern = pattern; binding_expression = expression } :: rest, used
  in
  let bindings, used = rename_rhs used (List.combine pat' bindings) in
  bindings, env', used
;;

let rec rename_structure used env = function
  | [] -> []
  | Structure_value (r, b, bs) :: rest ->
    let bs', env', used = rename_bindings used env r (b :: bs) in
    (match bs' with
     | b' :: bs' -> Structure_value (r, b', bs') :: rename_structure used env' rest
     | [] ->
       raise (Rename_error "Rename: нарушен инвариант - пустой список связанных значений"))
;;

let rename structure =
  let env = List.map (fun name -> name, name) Lexicon.builtins in
  (* env отвечает за видимость, used — за уникальность во всей программе.
     Имя из тела соседней функции уже занято, хотя в текущем env его нет,
     поэтому used протаскивается через весь обход и возвращается из каждого
     шага, а env — только вниз по области видимости.
     Fresh между проходами не сбрасываем. *)
  let used = Names.of_list Lexicon.builtins in
  try Ok (rename_structure used env structure) with
  | Rename_error e -> Error e
;;
