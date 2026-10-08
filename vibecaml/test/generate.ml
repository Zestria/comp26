[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml.Ast

let names = [ "x"; "y"; "f"; "k"; "fack"; "x'"; "_a"; "x1" ]
let core_identifiers = names @ List.concat Vibecaml.Pprint.binary_operators
let wide_identifiers = core_identifiers @ [ "~-"; "!"; "!="; ":="; "@@"; "##"; "or" ]

open QCheck.Gen

let constant =
  oneof
    [ map
        (fun value -> Const_int value)
        (oneof [ int_small; map ( ~- ) int_small; oneof_list [ min_int; max_int ] ])
    ; map (fun value -> Const_bool value) bool
    ; return Const_unit
    ]
;;

let short_list element = list_size (int_range 0 2) element

let pattern identifiers =
  sized_size (int_bound 3)
  @@ fix (fun self size ->
    let leaf =
      oneof
        [ return Pat_wildcard
        ; map (fun name -> Pat_var name) (oneof_list identifiers)
        ; map (fun value -> Pat_constant value) constant
        ]
    in
    if size = 0
    then leaf
    else (
      let child = self (size / 2) in
      oneof
        [ leaf
        ; map3 (fun a b rest -> Pat_tuple (a, b, rest)) child child (short_list child)
        ]))
;;

let rec_flag = oneof_list [ Recursive; Non_recursive ]

let binding identifiers expression =
  map2
    (fun binding_pattern binding_expression -> { binding_pattern; binding_expression })
    (pattern identifiers)
    expression
;;

(* [-1] --- константа, а не [( ~- ) 1]: такого дерева парсер не строит. *)
let unary_minus = function
  | Expr_constant (Const_int _) -> Expr_app (Expr_ident "~-", Expr_ident "x")
  | operand -> Expr_app (Expr_ident "~-", operand)
;;

let expression identifiers =
  let pattern = pattern identifiers in
  (* Отдельная ветка под оператор: из случайного [Expr_app] цепочки вроде
     [a ** b ** c] почти не складываются, и приоритеты остаются без проверки. *)
  (* Сначала уровень, потом оператор в нём: иначе одиночные [||] и [&&] из
     двух десятков операторов почти не встречаются. Прочие операторы из
     [identifiers] --- отдельной группой. *)
  let operators = List.filter (fun name -> not (List.mem name names)) identifiers in
  let levels = Vibecaml.Pprint.binary_operators in
  let others =
    List.filter (fun name -> not (List.exists (List.mem name) levels)) operators
  in
  let groups = List.filter (fun group -> group <> []) (others :: levels) in
  let operator = oneof (List.map oneof_list groups) in
  (* Два оператора одного уровня подряд: на них видна ассоциативность. *)
  let same_level_pair =
    oneof_list groups >>= fun group -> pair (oneof_list group) (oneof_list group)
  in
  let binary operator left right =
    Expr_app (Expr_app (Expr_ident operator, left), right)
  in
  sized_size (int_bound 20)
  @@ fix (fun self size ->
    let leaf =
      oneof
        [ map (fun value -> Expr_constant value) constant
        ; map (fun name -> Expr_ident name) (oneof_list identifiers)
        ]
    in
    if size = 0
    then leaf
    else (
      let child = self (size / 2) in
      oneof
        [ leaf
        ; map unary_minus child
        ; map3 binary operator child child
        ; map3
            (fun (outer, inner) (a, b, c) nested_left ->
               if nested_left
               then binary outer (binary inner a b) c
               else binary outer a (binary inner b c))
            same_level_pair
            (triple child child child)
            bool
        ; map2 (fun f a -> Expr_app (f, a)) child child
        ; map3 (fun c t e -> Expr_if (c, t, e)) child child child
        ; map3 (fun a b rest -> Expr_tuple (a, b, rest)) child child (short_list child)
        ; map3
            (fun first rest body -> Expr_fun (first, rest, body))
            pattern
            (short_list pattern)
            child
        ; map2
            (fun (flag, first) (rest, body) -> Expr_let (flag, first, rest, body))
            (pair rec_flag (binding identifiers child))
            (pair (short_list (binding identifiers child)) child)
        ]))
;;

let structure identifiers =
  let binding = binding identifiers (expression identifiers) in
  list_size
    (int_range 1 3)
    (map3
       (fun flag first rest -> Structure_value (flag, first, rest))
       rec_flag
       binding
       (short_list binding))
;;

(* Сначала крупные шаги (поддерево, выброшенный элемент списка), потом мелкие
   (упрощение одной части): QCheck берёт первый кандидат, который всё ещё
   падает. *)

(* Непустой список [first :: rest]: выкинуть элемент или сжать один. *)
let nonempty shrink make (first, rest) yield =
  (match rest with
   | [] -> ()
   | second :: rest -> yield (make second rest));
  shrink first (fun first -> yield (make first rest));
  QCheck.Shrink.list ~shrink rest (fun rest -> yield (make first rest))
;;

(* Не меньше двух элементов [a, b, rest]. *)
let at_least_two shrink make (a, b, rest) yield =
  QCheck.Shrink.list ~shrink rest (fun rest -> yield (make a b rest));
  shrink a (fun a -> yield (make a b rest));
  shrink b (fun b -> yield (make a b rest))
;;

let rec shrink_pattern pattern yield =
  match pattern with
  | Pat_wildcard -> ()
  | Pat_var _ | Pat_constant _ -> yield Pat_wildcard
  | Pat_tuple (a, b, rest) ->
    List.iter yield (a :: b :: rest);
    at_least_two
      shrink_pattern
      (fun a b rest -> Pat_tuple (a, b, rest))
      (a, b, rest)
      yield
;;

let rec shrink_expression expression yield =
  match expression with
  | Expr_constant (Const_int 0) | Expr_ident "x" -> ()
  | Expr_constant _ -> yield (Expr_constant (Const_int 0))
  | Expr_ident _ -> yield (Expr_ident "x")
  | Expr_let (flag, first, rest, body) ->
    yield body;
    List.iter (fun binding -> yield binding.binding_expression) (first :: rest);
    if flag = Recursive then yield (Expr_let (Non_recursive, first, rest, body));
    nonempty
      shrink_binding
      (fun first rest -> Expr_let (flag, first, rest, body))
      (first, rest)
      yield;
    shrink_expression body (fun body -> yield (Expr_let (flag, first, rest, body)))
  | Expr_fun (first, rest, body) ->
    yield body;
    nonempty
      shrink_pattern
      (fun first rest -> Expr_fun (first, rest, body))
      (first, rest)
      yield;
    shrink_expression body (fun body -> yield (Expr_fun (first, rest, body)))
  | Expr_app (Expr_ident "~-", operand) ->
    yield operand;
    shrink_expression operand (function
      | Expr_constant (Const_int _) -> ()
      | operand -> yield (Expr_app (Expr_ident "~-", operand)))
  | Expr_app (function_, argument) ->
    yield function_;
    yield argument;
    shrink_expression function_ (fun function_ -> yield (Expr_app (function_, argument)));
    shrink_expression argument (fun argument -> yield (Expr_app (function_, argument)))
  | Expr_if (condition, then_branch, else_branch) ->
    yield condition;
    yield then_branch;
    yield else_branch;
    shrink_expression condition (fun condition ->
      yield (Expr_if (condition, then_branch, else_branch)));
    shrink_expression then_branch (fun then_branch ->
      yield (Expr_if (condition, then_branch, else_branch)));
    shrink_expression else_branch (fun else_branch ->
      yield (Expr_if (condition, then_branch, else_branch)))
  | Expr_tuple (a, b, rest) ->
    List.iter yield (a :: b :: rest);
    at_least_two
      shrink_expression
      (fun a b rest -> Expr_tuple (a, b, rest))
      (a, b, rest)
      yield

and shrink_binding binding yield =
  shrink_pattern binding.binding_pattern (fun binding_pattern ->
    yield { binding with binding_pattern });
  shrink_expression binding.binding_expression (fun binding_expression ->
    yield { binding with binding_expression })
;;

let shrink_structure_item (Structure_value (flag, first, rest)) yield =
  if flag = Recursive then yield (Structure_value (Non_recursive, first, rest));
  nonempty
    shrink_binding
    (fun first rest -> Structure_value (flag, first, rest))
    (first, rest)
    yield
;;

let shrink_structure = QCheck.Shrink.list ~shrink:shrink_structure_item

let check tests =
  let seed =
    match Sys.getenv_opt "QCHECK_SEED" with
    | Some seed -> int_of_string seed
    | None ->
      Random.self_init ();
      Random.bits ()
  in
  let rand = Random.State.make [| seed |] in
  List.iter
    (fun test ->
       try QCheck.Test.check_exn ~rand test with
       | error ->
         (* Не [failwith]: необработанное исключение печатает кириллицу
            экранированной. *)
         prerr_endline
           (Printf.sprintf "QCHECK_SEED=%d\n%s" seed (Printexc.to_string error));
         exit 1)
    tests
;;
