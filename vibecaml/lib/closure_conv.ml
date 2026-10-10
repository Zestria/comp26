[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

exception Convert_error of string

module Names = Set.Make (String)

let names_of_patterns patterns =
  Names.of_list (List.concat_map Ast.pattern_names patterns)
;;

let names_of_bindings bindings =
  names_of_patterns (List.map (fun b -> b.binding_pattern) bindings)
;;

(* Синоним оставлен для кода, который на него уже ссылается. *)
let pattern_vars = Ast.pattern_names

(* diff и union работают со списками для совместимости с вызывающим кодом,
   внутри --- через множества. diff сохраняет порядок xs, union сортирует. *)
let diff xs ys =
  let ys = Names.of_list ys in
  List.filter (fun x -> not (Names.mem x ys)) xs
;;

let union xs ys = Names.elements (Names.union (Names.of_list xs) (Names.of_list ys))

let rec free bound = function
  | Expr_constant _ -> Names.empty
  | Expr_ident name when Names.mem name bound -> Names.empty
  | Expr_ident name -> Names.singleton name
  | Expr_let (flag, first, rest, body) ->
    let bindings = first :: rest in
    let bound' = Names.union bound (names_of_bindings bindings) in
    let rhs_bound =
      match flag with
      | Non_recursive -> bound
      | Recursive -> bound'
    in
    List.fold_left
      (fun acc b -> Names.union acc (free rhs_bound b.binding_expression))
      (free bound' body)
      bindings
  | Expr_fun (first, rest, body) ->
    free (Names.union bound (names_of_patterns (first :: rest))) body
  | Expr_app (f, a) -> Names.union (free bound f) (free bound a)
  | Expr_if (c, t, e) ->
    Names.union (free bound c) (Names.union (free bound t) (free bound e))
  | Expr_tuple (e1, e2, es) ->
    List.fold_left
      (fun acc e -> Names.union acc (free bound e))
      Names.empty
      (e1 :: e2 :: es)
;;

(* Свободные имена выражения, отсортированные и без дублей. *)
let collect bound expr = Names.elements (free (Names.of_list bound) expr)

(* После rename все связанные имена различны. Поэтому имя свободной
   переменной не может совпасть с внутренним связыванием: заменяем только
   обращения, оставляя паттерны на месте. Это касается и вложенных fun.
   Замена выполняется за один проход: подставленное выражение повторно не
   обходится. *)
let rec substitute names = function
  | Expr_ident name -> Option.value ~default:(Expr_ident name) (List.assoc_opt name names)
  | Expr_constant _ as expression -> expression
  | Expr_fun (first, rest, body) -> Expr_fun (first, rest, substitute names body)
  | Expr_let (flag, first, rest, body) ->
    let binding binding =
      { binding with binding_expression = substitute names binding.binding_expression }
    in
    Expr_let (flag, binding first, List.map binding rest, substitute names body)
  | Expr_app (f, a) -> Expr_app (substitute names f, substitute names a)
  | Expr_if (c, t, e) ->
    Expr_if (substitute names c, substitute names t, substitute names e)
  | Expr_tuple (first, second, rest) ->
    Expr_tuple
      (substitute names first, substitute names second, List.map (substitute names) rest)
;;

let substitution names = List.map (fun (name, name') -> name, Expr_ident name') names

let apply function_ arguments =
  List.fold_left
    (fun function_ name -> Expr_app (function_, Expr_ident name))
    function_
    arguments
;;

(* Свободные локальные переменные функции в алфавитном порядке. *)
let captures_of env (p, ps, e) =
  Names.elements (Names.diff (free (names_of_patterns (p :: ps)) e) env)
;;

let rec transform env expr =
  let transform_binding env b =
    { b with binding_expression = transform env b.binding_expression }
  in
  match expr with
  | (Expr_constant _ | Expr_ident _) as expr -> expr
  | Expr_fun (p, ps, e) ->
    let captures = captures_of env (p, ps, e) in
    apply (close env captures (fun _ -> []) (p, ps, e)) captures
  | Expr_let (Recursive, first, rest, body) -> transform_group env first rest body
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

(* Код замыкания: захваты становятся первыми параметрами. Аргумент в месте
   создания замыкания сохраняет старое имя, а его параметр обязан быть новым
   связыванием. Иначе CC снова создаст затенение, на отсутствие которого
   рассчитывают lifting и ANF. [calls] по именам новых параметров даёт
   дополнительные замены в теле. *)
and close env captures calls (p, ps, e) =
  let names = List.map (fun name -> name, Fresh.name "capture") captures in
  let parameters = List.map snd names in
  let e = transform env (substitute (substitution names @ calls parameters) e) in
  match parameters with
  | [] -> Expr_fun (p, ps, e)
  | first :: rest ->
    Expr_fun (Pat_var first, List.map (fun name -> Pat_var name) rest @ (p :: ps), e)

(* Локальная рекурсивная группа. Захваты общие на группу: объединение
   свободных локальных переменных всех тел. Каждая функция получает их
   первыми параметрами, а каждое упоминание функции группы заменяется её
   частичным применением к захватам. Имена группы после lambda lifting станут
   глобальными, поэтому вложенные fun их не захватывают. *)
and transform_group env first rest body =
  let split = function
    | { binding_pattern = Pat_var name; binding_expression = Expr_fun (p, ps, e) } ->
      name, (p, ps, e)
    | _ ->
      raise (Convert_error "Closure conversion: в let rec справа должна стоять функция")
  in
  let first = split first in
  let rest = List.map split rest in
  let group = List.map fst (first :: rest) in
  let env = Names.union (Names.of_list group) env in
  let captures =
    Names.elements
      (List.fold_left
         (fun acc (_, code) -> Names.union acc (Names.of_list (captures_of env code)))
         Names.empty
         (first :: rest))
  in
  let call parameters =
    List.map (fun name -> name, apply (Expr_ident name) parameters) group
  in
  let binding (name, code) =
    { binding_pattern = Pat_var name; binding_expression = close env captures call code }
  in
  Expr_let
    ( Recursive
    , binding first
    , List.map binding rest
    , transform env (substitute (call captures) body) )
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
    let bounded = names_of_bindings bindings in
    let env' =
      match flag with
      | Non_recursive -> env
      | Recursive -> Names.union bounded env
    in
    let bindings' = List.map (convert_binding env') bindings in
    (match bindings' with
     | first' :: rest' ->
       Structure_value (flag, first', rest')
       :: convert_structure (Names.union bounded env) items
     | [] -> raise (Convert_error "Closure conversion: нарушен инвариант"))
;;

let convert structure =
  try Ok (convert_structure (Names.of_list Lexicon.builtins) structure) with
  | Convert_error message -> Error message
;;
