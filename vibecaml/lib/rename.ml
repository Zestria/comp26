[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

exception Rename_error of string

let lookup env name = Option.value ~default:name (List.assoc_opt name env)

(* Согласно договоренности, переименовываем каждое пользовательское имя,
содержащее "__" *)
let should_rename name =
  let length = String.length name in
  (* todo: поправить чтобы была хвостовая рекурсия *)
  let rec helper i =
    i < length - 1 && ((name.[i] = '_' && name.[i + 1] = '_') || helper (i + 1))
  in
  helper 0
;;

let rename_binding env name =
  let name' =
    if List.mem_assoc name env || should_rename name then Fresh.name name else name
  in
  name', (name, name') :: env
;;

let rec rename_pattern env = function
  | (Pat_wildcard | Pat_constant _) as pat -> pat, env
  | Pat_var name ->
    let name', env = rename_binding env name in
    Pat_var name', env
  | Pat_tuple (fst, snd, rest) ->
    let fst', env = rename_pattern env fst in
    let snd', env = rename_pattern env snd in
    let rest', env = rename_patterns env rest in
    Pat_tuple (fst', snd', rest'), env

and rename_patterns env = function
  | [] -> [], env
  | pat :: rest ->
    let pat', env = rename_pattern env pat in
    let rest', env = rename_patterns env rest in
    pat' :: rest', env
;;

let rec rename_expr env = function
  | Expr_constant c -> Expr_constant c
  | Expr_ident id -> Expr_ident (lookup env id)
  | Expr_let (Recursive, _, _, _) ->
    raise (Rename_error "Rename: поддерживается только `let rec` верхнего уровня")
  | Expr_let (flag, first, rest, body) ->
    let bindings, env' = rename_bindings env flag (first :: rest) in
    (match bindings with
     | first :: rest -> Expr_let (flag, first, rest, rename_expr env' body)
     | [] -> raise (Rename_error ""))
  | Expr_fun (first, rest, body) ->
    let first, env = rename_pattern env first in
    let rest, env = rename_patterns env rest in
    Expr_fun (first, rest, rename_expr env body)
  | Expr_app (f, a) -> Expr_app (rename_expr env f, rename_expr env a)
  | Expr_if (c, t, e) -> Expr_if (rename_expr env c, rename_expr env t, rename_expr env e)
  | Expr_tuple (first, second, rest) ->
    Expr_tuple
      (rename_expr env first, rename_expr env second, List.map (rename_expr env) rest)

and rename_bindings env flag bindings =
  let pat = List.map (fun b -> b.binding_pattern) bindings in
  let pat', env' = rename_patterns env pat in
  let rhs_env =
    match flag with
    | Non_recursive -> env
    | Recursive -> env'
  in
  ( List.map2
      (fun pattern binding ->
         { binding_pattern = pattern
         ; binding_expression = rename_expr rhs_env binding.binding_expression
         })
      pat'
      bindings
  , env' )
;;

let rec rename_structure env = function
  | [] -> []
  | Structure_value (r, b, bs) :: rest ->
    let bs', env' = rename_bindings env r (b :: bs) in
    (match bs' with
     | b' :: bs' -> Structure_value (r, b', bs') :: rename_structure env' rest
     | [] ->
       raise (Rename_error "Rename: нарушен инвариант - пустой список связанных значений"))
;;

let rename structure =
  let env = List.map (fun name -> name, name) Lexicon.builtins in
  try Ok (rename_structure env structure) with
  | Rename_error e -> Error e
;;
