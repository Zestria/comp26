[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(* Инфиксные операторы, записанные буквами. Голыми их не напечатать, как и
   символьные: [mod] вне скобок --- ключевое слово, а не идентификатор. *)
let keyword_operators = [ "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"; "or" ]

let is_operator name =
  List.mem name keyword_operators
  || (String.length name > 0 && String.contains "!#$%&*+-./:<=>?@^|~" name.[0])
;;

(* Пробелы вокруг оператора обязательны: скобка, слитая со звёздочкой, открывает
   комментарий. *)
let pp_identifier ppf name =
  if is_operator name
  then Format.fprintf ppf "( %s )" name
  else Format.pp_print_string ppf name
;;

(* Уровни связывания, от слабого к сильному. Выражение печатается без скобок,
   если его уровень не ниже того, что требует место. *)
let tuple_level = 0

(* [let], [fun] и [if]: тянутся вправо до конца, поэтому без скобок стоят только
   в хвосте (см. [tail] ниже). *)
let open_level = 1

(* Между ними --- уровни бинарных операторов из [Lexicon.binary_operators]. *)
let unary_minus_level = open_level + 1 + List.length Lexicon.binary_operators
let application_level = unary_minus_level + 1
let atom_level = application_level + 1

(* Уровни операторов идут сразу за [open_level]. *)
let binary_operator name =
  List.find_mapi
    (fun index (names, associativity) ->
       if List.mem name names then Some (open_level + 1 + index, associativity) else None)
    Lexicon.binary_operators
;;

let binary_operators = Lexicon.operators

type shape =
  | Binary of identifier * int * Lexicon.associativity * expression * expression
  (* [-e]. Минус перед целой константой сворачивается в константу, поэтому
     [( ~- ) 1] остаётся обычным применением. *)
  | Unary_minus of expression
  | Other

let shape = function
  | Expr_app (Expr_app (Expr_ident name, left), right) ->
    (match binary_operator name with
     | Some (level, associativity) -> Binary (name, level, associativity, left, right)
     | None -> Other)
  | Expr_app (Expr_ident "~-", operand) ->
    (match operand with
     | Expr_constant (Const_int _) -> Other
     | _ -> Unary_minus operand)
  | _ -> Other
;;

let level expression =
  match expression, shape expression with
  | _, Binary (_, level, _, _, _) -> level
  | _, Unary_minus _ -> unary_minus_level
  | Expr_tuple _, Other -> tuple_level
  | (Expr_let _ | Expr_fun _ | Expr_if _), Other -> open_level
  | Expr_app _, Other -> application_level
  | Expr_constant (Const_int value), Other when value < 0 -> unary_minus_level
  | (Expr_constant _ | Expr_ident _), Other -> atom_level
;;

let pp_constant ppf = function
  | Const_int value -> Format.fprintf ppf "%d" value
  | Const_bool value -> Format.fprintf ppf "%b" value
  | Const_unit -> Format.pp_print_string ppf "()"
;;

let pp_tail separator printer ppf values =
  List.iter (fun value -> Format.fprintf ppf "%s%a" separator printer value) values
;;

let pp_rec_flag ppf = function
  | Non_recursive -> ()
  | Recursive -> Format.pp_print_string ppf " rec"
;;

(* Кортеж без скобок --- только на верхнем уровне связывания. Отрицательная
   константа --- всегда в скобках: [let -1 = ...] вне ядра. *)
let rec pp_pattern_at ~simple ppf = function
  | Pat_wildcard -> Format.pp_print_string ppf "_"
  | Pat_var name -> pp_identifier ppf name
  | Pat_constant (Const_int value) when value < 0 -> Format.fprintf ppf "(%d)" value
  | Pat_constant constant -> pp_constant ppf constant
  | Pat_tuple (first, second, rest) ->
    let pp_simple = pp_pattern_at ~simple:true in
    Format.fprintf
      ppf
      (if simple then "(%a, %a%a)" else "%a, %a%a")
      pp_simple
      first
      pp_simple
      second
      (pp_tail ", " pp_simple)
      rest
;;

let pp_simple_pattern = pp_pattern_at ~simple:true

(* [tail]: справа от выражения до конца скобок, [in], [then], [else] или [and]
   ничего нет, и [let], [fun], [if] можно не брать в скобки. *)
let rec pp_at ~level:required ~tail ppf expression =
  let is_open =
    match expression with
    | Expr_let _ | Expr_fun _ | Expr_if _ -> true
    | _ -> false
  in
  if level expression < required || (is_open && not tail)
  then Format.fprintf ppf "(%a)" (pp_at ~level:tuple_level ~tail:true) expression
  else pp_bare ~tail ppf expression

and pp_bare ~tail ppf expression =
  match shape expression, expression with
  | Binary (name, level, associativity, left, right), _ ->
    let left_level, right_level =
      match (associativity : Lexicon.associativity) with
      | Left -> level, level + 1
      | Right -> level + 1, level
    in
    Format.fprintf
      ppf
      "%a %s %a"
      (pp_at ~level:left_level ~tail:false)
      left
      name
      (pp_at ~level:right_level ~tail:false)
      right
  | Unary_minus operand, _ ->
    (* [- -x], а не [--x]: иначе лексер прочтёт оператор [--]. *)
    let separator =
      match shape operand with
      | Unary_minus _ -> " "
      | Binary _ | Other -> ""
    in
    Format.fprintf
      ppf
      "-%s%a"
      separator
      (pp_at ~level:unary_minus_level ~tail:false)
      operand
  | Other, Expr_constant constant -> pp_constant ppf constant
  | Other, Expr_ident name -> pp_identifier ppf name
  | Other, Expr_tuple (first, second, rest) ->
    let elements = first :: second :: rest in
    let last = List.length elements - 1 in
    List.iteri
      (fun index element ->
         if index > 0 then Format.pp_print_string ppf ", ";
         pp_at ~level:open_level ~tail:(tail && index = last) ppf element)
      elements
  | Other, Expr_let (rec_flag, first, rest, body) ->
    Format.fprintf
      ppf
      "let%a %a in %a"
      pp_rec_flag
      rec_flag
      pp_value_bindings
      (first, rest)
      (pp_at ~level:tuple_level ~tail)
      body
  | Other, Expr_fun (first, rest, body) ->
    Format.fprintf
      ppf
      "fun %a -> %a"
      pp_parameters
      (first, rest)
      (pp_at ~level:tuple_level ~tail)
      body
  (* Ветка [then] без кортежа: [if a then b, c else d] OCaml не разбирает. *)
  | Other, Expr_if (condition, then_branch, else_branch) ->
    Format.fprintf
      ppf
      "if %a then %a else %a"
      (pp_at ~level:tuple_level ~tail:true)
      condition
      (pp_at ~level:open_level ~tail:true)
      then_branch
      (pp_at ~level:tuple_level ~tail)
      else_branch
  (* [true], [false] и [()] для OCaml --- конструкторы: [true x] --- это не
     применение. Функцией в применении они бывают только в скобках. *)
  | Other, Expr_app ((Expr_constant (Const_bool _ | Const_unit) as function_), argument)
    ->
    Format.fprintf
      ppf
      "(%a) %a"
      (pp_bare ~tail:false)
      function_
      (pp_at ~level:atom_level ~tail:false)
      argument
  | Other, Expr_app (function_, argument) ->
    Format.fprintf
      ppf
      "%a %a"
      (pp_at ~level:application_level ~tail:false)
      function_
      (pp_at ~level:atom_level ~tail:false)
      argument

and pp_parameters ppf (first, rest) =
  Format.fprintf ppf "%a%a" pp_simple_pattern first (pp_tail " " pp_simple_pattern) rest

(* [let f x = e] вместо [let f = fun x -> e], если первый параметр ---
   переменная: так в выводе встречаются обе формы при любом числе параметров,
   и тесты проверяют разбор обеих. *)
and pp_value_binding ppf = function
  | { binding_pattern = Pat_var name
    ; binding_expression = Expr_fun ((Pat_var _ as first), rest, body)
    } ->
    Format.fprintf
      ppf
      "%a %a = %a"
      pp_identifier
      name
      pp_parameters
      (first, rest)
      (pp_at ~level:tuple_level ~tail:true)
      body
  | { binding_pattern; binding_expression } ->
    Format.fprintf
      ppf
      "%a = %a"
      (pp_pattern_at ~simple:false)
      binding_pattern
      (pp_at ~level:tuple_level ~tail:true)
      binding_expression

and pp_value_bindings ppf (first, rest) =
  Format.fprintf ppf "%a%a" pp_value_binding first (pp_tail " and " pp_value_binding) rest
;;

let pp_expression = pp_at ~level:tuple_level ~tail:true

let pp_structure_item ppf = function
  | Structure_value (rec_flag, first, rest) ->
    Format.fprintf ppf "let%a %a" pp_rec_flag rec_flag pp_value_bindings (first, rest)
;;

let pp_structure ppf items =
  List.iteri
    (fun index item ->
       if index > 0 then Format.fprintf ppf "\n";
       pp_structure_item ppf item)
    items
;;

let string_of_expression expression = Format.asprintf "%a" pp_expression expression
let string_of_structure structure = Format.asprintf "%a" pp_structure structure
