[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml.Ast
open Parsetree

(* Внутри бросаем [Unsupported], на границе модуля превращаем в [Error]. *)
exception Unsupported of string

let unsupported what = raise (Unsupported what)

let constant { pconst_desc; _ } =
  match pconst_desc with
  | Pconst_integer (digits, None) -> Const_int (int_of_string digits)
  | _ -> unsupported "константа"
;;

let construct (name : Longident.t) =
  match name with
  | Lident "true" -> Const_bool true
  | Lident "false" -> Const_bool false
  | Lident "()" -> Const_unit
  | _ -> unsupported "конструктор"
;;

let rec pattern { ppat_desc; _ } =
  match ppat_desc with
  | Ppat_any -> Pat_wildcard
  | Ppat_var { txt; _ } -> Pat_var txt
  | Ppat_constant value -> Pat_constant (constant value)
  | Ppat_construct ({ txt; _ }, None) -> Pat_constant (construct txt)
  | Ppat_tuple ((None, first) :: (None, second) :: rest, Closed) ->
    Pat_tuple (pattern first, pattern second, List.map unlabeled_pattern rest)
  | _ -> unsupported "паттерн"

and unlabeled_pattern = function
  | None, value -> pattern value
  | Some _, _ -> unsupported "метка в кортеже"
;;

let parameter { pparam_desc; _ } =
  match pparam_desc with
  | Pparam_val (Nolabel, None, value) -> pattern value
  | _ -> unsupported "параметр функции"
;;

let rec expression { pexp_desc; _ } =
  match pexp_desc with
  | Pexp_constant value -> Expr_constant (constant value)
  | Pexp_construct ({ txt; _ }, None) -> Expr_constant (construct txt)
  (* [true x] и [() x] OCaml читает как конструктор с аргументом. В подмножестве
     таких конструкторов нет, и наши парсеры прочтут это как применение. *)
  | Pexp_construct ({ txt; _ }, Some argument) ->
    Expr_app (Expr_constant (construct txt), expression argument)
  | Pexp_ident { txt = Lident name; _ } -> Expr_ident name
  | Pexp_let (flag, first :: rest, body) ->
    Expr_let
      (rec_flag flag, value_binding first, List.map value_binding rest, expression body)
  | Pexp_function (first :: rest, None, Pfunction_body body) ->
    Expr_fun (parameter first, List.map parameter rest, expression body)
  | Pexp_apply (function_, arguments) ->
    List.fold_left
      (fun applied -> function
         | Asttypes.Nolabel, argument -> Expr_app (applied, expression argument)
         | _ -> unsupported "метка аргумента")
      (expression function_)
      arguments
  | Pexp_ifthenelse (condition, then_branch, Some else_branch) ->
    Expr_if (expression condition, expression then_branch, expression else_branch)
  | Pexp_tuple ((None, first) :: (None, second) :: rest) ->
    Expr_tuple (expression first, expression second, List.map unlabeled_expression rest)
  | _ -> unsupported "выражение"

and unlabeled_expression = function
  | None, value -> expression value
  | Some _, _ -> unsupported "метка в кортеже"

and value_binding = function
  | { pvb_pat; pvb_expr; pvb_constraint = None; _ } ->
    { binding_pattern = pattern pvb_pat; binding_expression = expression pvb_expr }
  | { pvb_constraint = Some _; _ } -> unsupported "ограничение типа"

and rec_flag : Asttypes.rec_flag -> Vibecaml.Ast.rec_flag = function
  | Nonrecursive -> Non_recursive
  | Recursive -> Recursive
;;

let structure_item { pstr_desc; _ } =
  match pstr_desc with
  | Pstr_value (flag, first :: rest) ->
    Structure_value (rec_flag flag, value_binding first, List.map value_binding rest)
  | _ -> unsupported "объявление верхнего уровня"
;;

let run parse convert source =
  match convert (parse (Lexing.from_string source)) with
  | result -> Ok result
  | exception Unsupported what -> Error ("вне подмножества: " ^ what)
  | exception Failure message -> Error ("не разбирается: " ^ message)
  | exception error ->
    (* [Location.report_exception] перевыбрасывает чужие исключения, поэтому
       берём отчёт через [error_of_exn]. *)
    (match Location.error_of_exn error with
     | Some (`Ok report) ->
       Error (Format.asprintf "не разбирается: %a" Location.print_report report)
     | Some `Already_displayed | None ->
       Error ("не разбирается: " ^ Printexc.to_string error))
;;

let expression source = run Parse.expression expression source
let structure source = run Parse.implementation (List.map structure_item) source
