[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type immexpr =
  | ImmNum of int
  | ImmBool of bool
  | ImmUnit
  | ImmId of Ast.identifier

type cond =
  | CondImm of immexpr
  | CondCmp of Ast.identifier * immexpr * immexpr

type cexpr =
  | CImmExpr of immexpr
  | CApp of immexpr * immexpr list
  | CIf of cond * aexpr * aexpr
  | CTuple of immexpr * immexpr * immexpr list
  | CField of immexpr * int * int

and aexpr =
  | ALet of Ast.identifier * cexpr * aexpr
  | ACExpr of cexpr

type item = Ast.rec_flag * (Ast.identifier * Ast.identifier list * aexpr) list
type program = item list

let aimm value = ACExpr (CImmExpr value)
let avar name = aimm (ImmId name)

(* Правила скобок для операторов и отрицательных констант уже есть в Pprint.
   Используем их и здесь, чтобы два принтера не расходились. *)
let pp_atom ppf value =
  let expression =
    match value with
    | ImmNum n -> Ast.Expr_constant (Ast.Const_int n)
    | ImmBool b -> Ast.Expr_constant (Ast.Const_bool b)
    | ImmUnit -> Ast.Expr_constant Ast.Const_unit
    | ImmId name -> Ast.Expr_ident name
  in
  match value with
  | ImmNum n when n < 0 -> Format.fprintf ppf "(%a)" Pprint.pp_expression expression
  | _ -> Pprint.pp_expression ppf expression
;;

let pp_atoms ppf values =
  List.iter (fun value -> Format.fprintf ppf " %a" pp_atom value) values
;;

let pp_condition ppf = function
  | CondImm value -> pp_atom ppf value
  | CondCmp (operator, left, right) ->
    Format.fprintf ppf "%a %s %a" pp_atom left operator pp_atom right
;;

let rec pp_expression ppf = function
  | ACExpr computation -> pp_computation ppf computation
  | ALet (name, computation, body) ->
    Format.fprintf
      ppf
      "@[<v 0>let %a = %a in@,%a@]"
      pp_atom
      (ImmId name)
      pp_computation
      computation
      pp_expression
      body

and pp_computation ppf = function
  | CImmExpr value -> pp_atom ppf value
  | CApp (function_, arguments) ->
    Format.fprintf ppf "%a%a" pp_atom function_ pp_atoms arguments
  | CTuple (first, second, rest) ->
    Format.fprintf ppf "(%a, %a" pp_atom first pp_atom second;
    List.iter (fun value -> Format.fprintf ppf ", %a" pp_atom value) rest;
    Format.pp_print_string ppf ")"
  | CField (tuple, index, size) ->
    (* Размер берём из исходного паттерна: пара и тройка требуют разных
       паттернов OCaml. Временное имя видно только внутри этих скобок,
       поэтому оно не захватывает переменные в продолжении. *)
    Format.pp_print_string ppf "(let (";
    List.iteri
      (fun field _ ->
         if field > 0 then Format.pp_print_string ppf ", ";
         Format.pp_print_string ppf (if field = index then "anf_field" else "_"))
      (List.init size Fun.id);
    Format.fprintf ppf ") = %a in anf_field)" pp_atom tuple
  | CIf (condition, yes, no) ->
    (* Скобки закрывают ветку даже при вложенном if или let. *)
    Format.fprintf
      ppf
      "@[<v 0>if %a then (@;<1 2>%a@,) else (@;<1 2>%a@,)@]"
      pp_condition
      condition
      pp_expression
      yes
      pp_expression
      no
;;

let pp_binding ppf (name, parameters, body) =
  Format.fprintf
    ppf
    "%a%a = %a"
    pp_atom
    (ImmId name)
    pp_atoms
    (List.map (fun name -> ImmId name) parameters)
    pp_expression
    body
;;

let pp_program ppf program =
  List.iteri
    (fun index (flag, bindings) ->
       if index > 0 then Format.fprintf ppf "@\n";
       match bindings with
       | [] -> ()
       | first :: rest ->
         Format.fprintf
           ppf
           "@[<v 0>let%s %a"
           (match flag with
            | Ast.Recursive -> " rec"
            | Ast.Non_recursive -> "")
           pp_binding
           first;
         List.iter (fun binding -> Format.fprintf ppf "@,and %a" pp_binding binding) rest;
         Format.fprintf ppf "@]")
    program
;;

let string_of_expression expression = Format.asprintf "%a" pp_expression expression
let string_of_program program = Format.asprintf "%a" pp_program program

let rec count_expression = function
  | ALet (_, computation, body) ->
    1 + count_computation computation + count_expression body
  | ACExpr computation -> count_computation computation

and count_computation = function
  | CIf (_, yes, no) -> count_expression yes + count_expression no
  | CImmExpr _ | CApp _ | CTuple _ | CField _ -> 0
;;

let count_lets program =
  List.fold_left
    (fun count (_, bindings) ->
       List.fold_left
         (fun count (_, _, body) -> count + count_expression body)
         count
         bindings)
    0
    program
;;
