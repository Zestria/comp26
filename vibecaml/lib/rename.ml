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
  let rec helper i =
    i < length - 1 && ((name.[i] = '_' && name.[i + 1] = '_') || helper (i + 1))
  in
  helper 0
;;

let rename_binding used env name =
  let operator = List.mem name (Lexicon.operators @ [ "~-" ]) in
  let name' =
    if List.mem name !used || should_rename name || operator
    then Fresh.name (if operator then "op" else name)
    else name
  in
  used := name' :: !used;
  name', (name, name') :: env
;;

let rec rename_pattern used env = function
  | (Pat_wildcard | Pat_constant _) as pat -> pat, env
  | Pat_var name ->
    let name', env = rename_binding used env name in
    Pat_var name', env
  | Pat_tuple (fst, snd, rest) ->
    let fst', env = rename_pattern used env fst in
    let snd', env = rename_pattern used env snd in
    let rest', env = rename_patterns used env rest in
    Pat_tuple (fst', snd', rest'), env

and rename_patterns used env = function
  | [] -> [], env
  | pat :: rest ->
    let pat', env = rename_pattern used env pat in
    let rest', env = rename_patterns used env rest in
    pat' :: rest', env
;;

let rec rename_expr used env = function
  | Expr_constant c -> Expr_constant c
  | Expr_ident id -> Expr_ident (lookup env id)
  | Expr_let (flag, first, rest, body) ->
    let bindings, env' = rename_bindings used env flag (first :: rest) in
    (match bindings with
     | first :: rest -> Expr_let (flag, first, rest, rename_expr used env' body)
     | [] -> raise (Rename_error "Rename: пустая группа let"))
  | Expr_fun (first, rest, body) ->
    let first, env = rename_pattern used env first in
    let rest, env = rename_patterns used env rest in
    Expr_fun (first, rest, rename_expr used env body)
  | Expr_app (f, a) ->
    let f = rename_expr used env f in
    let a = rename_expr used env a in
    Expr_app (f, a)
  | Expr_if (c, t, e) ->
    let c = rename_expr used env c in
    let t = rename_expr used env t in
    let e = rename_expr used env e in
    Expr_if (c, t, e)
  | Expr_tuple (first, second, rest) ->
    let first = rename_expr used env first in
    let second = rename_expr used env second in
    let rest = List.map (rename_expr used env) rest in
    Expr_tuple (first, second, rest)

and rename_bindings used env flag bindings =
  let pat = List.map (fun b -> b.binding_pattern) bindings in
  let pat', env' = rename_patterns used env pat in
  let rhs_env =
    match flag with
    | Non_recursive -> env
    | Recursive -> env'
  in
  ( List.map2
      (fun pattern binding ->
         { binding_pattern = pattern
         ; binding_expression = rename_expr used rhs_env binding.binding_expression
         })
      pat'
      bindings
  , env' )
;;

let rec rename_structure used env = function
  | [] -> []
  | Structure_value (r, b, bs) :: rest ->
    let bs', env' = rename_bindings used env r (b :: bs) in
    (match bs' with
     | b' :: bs' -> Structure_value (r, b', bs') :: rename_structure used env' rest
     | [] ->
       raise (Rename_error "Rename: нарушен инвариант - пустой список связанных значений"))
;;

let rename structure =
  let env = List.map (fun name -> name, name) Lexicon.builtins in
  (* env отвечает за видимость, used — за уникальность во всей программе.
     Имя из тела соседней функции уже занято, хотя в текущем env его нет.
     Список меняется только в этом запуске; Fresh между проходами не сбрасываем. *)
  let used = ref Lexicon.builtins in
  try Ok (rename_structure used env structure) with
  | Rename_error e -> Error e
;;
