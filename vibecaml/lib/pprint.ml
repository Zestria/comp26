open Ast

let pp_constant ppf = function
  | Const_int value -> Format.fprintf ppf "%d" value
  | Const_bool value -> Format.fprintf ppf "%b" value
  | Const_unit -> Format.pp_print_string ppf "()"
;;

let pp_tail separator printer ppf values =
  List.iter (fun value -> Format.fprintf ppf "%s%a" separator printer value) values
;;

let rec pp_pattern ppf = function
  | Pat_wildcard -> Format.pp_print_string ppf "_"
  | Pat_var name -> Format.pp_print_string ppf name
  | Pat_constant constant -> pp_constant ppf constant
  | Pat_tuple (first, second, rest) ->
    Format.fprintf
      ppf
      "(%a, %a%a)"
      pp_pattern
      first
      pp_pattern
      second
      (pp_patterns_tail ", ")
      rest

and pp_patterns_tail separator ppf = function
  | [] -> ()
  | pattern :: rest ->
    Format.fprintf
      ppf
      "%s%a%a"
      separator
      pp_pattern
      pattern
      (pp_patterns_tail separator)
      rest
;;

let rec pp_expression ppf = function
  | Expr_constant constant -> pp_constant ppf constant
  | Expr_ident name -> Format.pp_print_string ppf name
  | Expr_let (rec_flag, bindings, body) ->
    Format.fprintf
      ppf
      "(let%s %a in %a)"
      (match rec_flag with
       | Non_recursive -> ""
       | Recursive -> " rec")
      pp_value_bindings
      bindings
      pp_expression
      body
  | Expr_fun (patterns, body) ->
    Format.fprintf ppf "(fun %a -> %a)" pp_value_patterns patterns pp_expression body
  | Expr_app (function_, argument) ->
    Format.fprintf ppf "(%a %a)" pp_expression function_ pp_expression argument
  | Expr_if (condition, then_branch, else_branch) ->
    Format.fprintf
      ppf
      "(if %a then %a else %a)"
      pp_expression
      condition
      pp_expression
      then_branch
      pp_expression
      else_branch
  | Expr_tuple (first, second, rest) ->
    Format.fprintf
      ppf
      "(%a, %a%a)"
      pp_expression
      first
      pp_expression
      second
      (pp_expressions_tail ", ")
      rest

and pp_value_binding ppf { binding_pattern; binding_expression } =
  Format.fprintf ppf "%a = %a" pp_pattern binding_pattern pp_expression binding_expression

and pp_value_bindings ppf = function
  | [] -> invalid_arg "empty let binding group"
  | binding :: rest ->
    pp_value_binding ppf binding;
    List.iter (fun value -> Format.fprintf ppf " and %a" pp_value_binding value) rest

and pp_value_patterns ppf = function
  | [] -> Format.pp_print_string ppf "()"
  | pattern :: rest ->
    pp_pattern ppf pattern;
    List.iter (fun value -> Format.fprintf ppf " %a" pp_pattern value) rest

and pp_expressions_tail separator ppf = function
  | [] -> ()
  | expression :: rest ->
    Format.fprintf
      ppf
      "%s%a%a"
      separator
      pp_expression
      expression
      (pp_expressions_tail separator)
      rest
;;
