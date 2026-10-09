[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

exception Convert_error of string

let dedup xs =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] xs
;;

let diff xs ys = List.filter (fun x -> not (List.mem x ys)) xs
let union xs ys = dedup (xs @ ys)

let rec pattern_vars pat =
  match pat with
  | Pat_wildcard -> []
  | Pat_var id -> [ id ]
  | Pat_constant _ -> []
  | Pat_tuple (p1, p2, ps) ->
    dedup (pattern_vars p1 @ pattern_vars p2 @ List.concat_map pattern_vars ps)
;;

let rec collect bound = function
  | Expr_constant _ -> []
  | Expr_ident name -> if List.mem name bound then [] else [ name ]
  | Expr_let (Recursive, _, _, _) ->
    raise
      (Convert_error "Closure conversion: поддерживается только `let rec` верхнего уровня")
  | Expr_let (Non_recursive, first, rest, body) ->
    let bindings = first :: rest in
    let bound' =
      bound @ List.concat_map (fun b -> pattern_vars b.binding_pattern) bindings
    in
    union
      (List.concat_map (fun b -> collect bound' b.binding_expression) bindings)
      (collect bound' body)
  | Expr_fun (first, rest, body) ->
    collect (bound @ List.concat_map pattern_vars (first :: rest)) body
  | Expr_app (f, a) -> union (collect bound f) (collect bound a)
  | Expr_if (c, t, e) ->
    union (collect bound c) (union (collect bound t) (collect bound e))
  | Expr_tuple (e1, e2, es) ->
    List.fold_left
      union
      (union (collect bound e1) (collect bound e2))
      (List.map (collect bound) es)
;;

let rec transform env expr =
  let transform_binding env b =
    { b with binding_expression = transform env b.binding_expression }
  in
  match expr with
  | (Expr_constant _ | Expr_ident _) as expr -> expr
  | Expr_fun (p, ps, e) ->
    let params = List.concat_map pattern_vars (p :: ps) in
    let captures = diff (dedup (collect params e)) env in
    let e' = transform env e in
    (match captures with
     | [] -> Expr_fun (p, ps, e')
     | _ ->
       let code_params = List.map (fun name -> Pat_var name) captures @ (p :: ps) in
       let code =
         match code_params with
         | first' :: rest' -> Expr_fun (first', rest', e')
         | [] -> raise (Convert_error "Closure conversion: нарушен инвариант")
       in
       List.fold_left
         (fun function_ name -> Expr_app (function_, Expr_ident name))
         code
         captures)
  | Expr_let (Recursive, _, _, _) ->
    raise
      (Convert_error "Closure conversion: поддерживается только `let rec` верхнего уровня")
  | Expr_let (Non_recursive, b, bs, e) ->
    Expr_let
      ( Non_recursive
      , transform_binding env b
      , List.map (transform_binding env) bs
      , transform env e )
  | Expr_app (f, x) -> Expr_app (transform env f, transform env x)
  | Expr_if (c, t, e) -> Expr_if (transform env c, transform env t, transform env e)
  | Expr_tuple (fst, snd, rest) ->
    Expr_tuple (transform env fst, transform env snd, List.map (transform env) rest)
;;

let binding_names bindings =
  List.concat_map (fun binding -> pattern_vars binding.binding_pattern) bindings
;;

let convert_binding env binding =
  match binding.binding_pattern, binding.binding_expression with
  | Pat_var _, Expr_fun (parameter, parameters, body) ->
    { binding with
      binding_expression = Expr_fun (parameter, parameters, transform env body)
    }
  | _ -> { binding with binding_expression = transform env binding.binding_expression }
;;

let rec convert_structure env = function
  | [] -> []
  | Structure_value (flag, first, rest) :: items ->
    let bindings = first :: rest in
    let bounded = binding_names bindings in
    let env' =
      match flag with
      | Non_recursive -> env
      | Recursive -> bounded @ env
    in
    let bindings' = List.map (convert_binding env') bindings in
    (match bindings' with
     | first' :: rest' ->
       Structure_value (flag, first', rest') :: convert_structure (bounded @ env) items
     | [] -> raise (Convert_error "Closure conversion: нарушен инвариант"))
;;

let convert structure =
  try Ok (convert_structure Lexicon.builtins structure) with
  | Convert_error message -> Error message
;;
