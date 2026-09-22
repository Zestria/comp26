(** Precedence-aware rendering of the surface AST as MiniML source. *)

open Ast

type associativity =
  | Left
  | Nonassociative

let control_precedence = 1
let application_precedence = 9
let prefix_precedence = 8
let atom_precedence = 10

let infix_operator = function
  | "||" -> Some (2, Left)
  | "&&" -> Some (3, Left)
  | "=" | "<>" | "<" | "<=" | ">" | ">=" -> Some (4, Nonassociative)
  | "+" | "-" -> Some (6, Left)
  | "*" | "/" -> Some (7, Left)
  | _ -> None
;;

let is_prefix_operator = function
  | "not" | "~-" | "~+" -> true
  | _ -> false
;;

let parenthesize condition printer formatter value =
  if condition
  then Format.fprintf formatter "(@[%a@])" printer value
  else printer formatter value
;;

let pp_constant formatter = function
  | Integer value -> Format.pp_print_int formatter value
  | Boolean value -> Format.pp_print_bool formatter value
  | Unit -> Format.pp_print_string formatter "()"
;;

let is_operator_name name =
  String.length name > 0
  &&
  match name.[0] with
  | '!'
  | '$'
  | '%'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '|'
  | '~' -> true
  | _ -> false
;;

let pp_identifier formatter name =
  if is_operator_name name
  then Format.fprintf formatter "(%s)" name
  else Format.pp_print_string formatter name
;;

let pp_pattern_at _precedence formatter = function
  | PWildcard -> Format.pp_print_string formatter "_"
  | PVariable name -> pp_identifier formatter name
;;

let pp_pattern = pp_pattern_at 0

let rec flatten_applications arguments = function
  | Application (function_, argument) ->
    flatten_applications (argument :: arguments) function_
  | function_ -> function_, arguments
;;

let rec flatten_lambdas parameters = function
  | Lambda (parameter, body) -> flatten_lambdas (parameter :: parameters) body
  | body -> List.rev parameters, body
;;

let rec pp_expr_at precedence formatter expression =
  match expression with
  | Application (Application (Variable operator, left), right)
    when Option.is_some (infix_operator operator) ->
    pp_infix precedence formatter operator left right
  | Application (Variable operator, operand) when is_prefix_operator operator ->
    parenthesize
      (precedence > prefix_precedence)
      (fun formatter operand ->
         let separator = if String.equal operator "not" then " " else "" in
         Format.fprintf
           formatter
           "@[<hov 2>%s%s%a@]"
           operator
           separator
           (pp_expr_at prefix_precedence)
           operand)
      formatter
      operand
  | Constant (Integer value) when value < 0 ->
    parenthesize
      (precedence > prefix_precedence)
      (fun formatter value -> Format.pp_print_int formatter value)
      formatter
      value
  | Constant constant -> pp_constant formatter constant
  | Variable name -> pp_identifier formatter name
  | Lambda _ -> pp_lambda precedence formatter expression
  | Application _ -> pp_application precedence formatter expression
  | If_then_else (condition, if_true, if_false) ->
    parenthesize
      (precedence > control_precedence)
      (fun formatter (condition, if_true, if_false) ->
         Format.fprintf
           formatter
           "@[<v 0>if %a then@;<1 2>%a@;else@;<1 2>%a@]"
           (pp_expr_at 0)
           condition
           (pp_expr_at control_precedence)
           if_true
           (pp_expr_at control_precedence)
           if_false)
      formatter
      (condition, if_true, if_false)
  | Let_in (recursive, pattern, value, body) ->
    parenthesize
      (precedence > control_precedence)
      (fun formatter () ->
         Format.fprintf
           formatter
           "@[<v 0>%a@,in@;<1 2>%a@]"
           (pp_binding ("let" ^ pp_rec_flag recursive))
           (pattern, value)
           (pp_expr_at control_precedence)
           body)
      formatter
      ()

and pp_infix precedence formatter operator left right =
  match infix_operator operator with
  | None -> assert false
  | Some (operator_precedence, associativity) ->
    let left_precedence, right_precedence =
      match associativity with
      | Left -> operator_precedence, operator_precedence + 1
      | Nonassociative -> operator_precedence + 1, operator_precedence + 1
    in
    parenthesize
      (precedence > operator_precedence)
      (fun formatter (left, right) ->
         Format.fprintf
           formatter
           "@[<hov 2>%a %s@ %a@]"
           (pp_expr_at left_precedence)
           left
           operator
           (pp_expr_at right_precedence)
           right)
      formatter
      (left, right)

and pp_application precedence formatter expression =
  let function_, arguments = flatten_applications [] expression in
  parenthesize
    (precedence > application_precedence)
    (fun formatter () ->
       Format.fprintf
         formatter
         "@[<hov 2>%a"
         (pp_expr_at application_precedence)
         function_;
       List.iter (Format.fprintf formatter "@ %a" (pp_expr_at atom_precedence)) arguments;
       Format.fprintf formatter "@]")
    formatter
    ()

and pp_lambda precedence formatter expression =
  let parameters, body = flatten_lambdas [] expression in
  parenthesize
    (precedence > control_precedence)
    (fun formatter () ->
       Format.fprintf formatter "@[<hov 2>fun";
       List.iter (Format.fprintf formatter "@ %a" (pp_pattern_at 2)) parameters;
       Format.fprintf formatter "@ ->@ %a@]" (pp_expr_at control_precedence) body)
    formatter
    ()

and pp_binding keyword formatter (pattern, value) =
  match pattern, flatten_lambdas [] value with
  | PVariable name, ((_ :: _ as parameters), body) ->
    Format.fprintf formatter "@[<hov 2>%s %a" keyword pp_identifier name;
    List.iter (Format.fprintf formatter "@ %a" (pp_pattern_at 2)) parameters;
    Format.fprintf formatter "@ =@;<1000 0>%a@]" (pp_expr_at control_precedence) body
  | _ ->
    Format.fprintf
      formatter
      "@[<hov 2>%s %a =@;<1000 0>%a@]"
      keyword
      pp_pattern
      pattern
      (pp_expr_at control_precedence)
      value

and pp_rec_flag = function
  | Nonrecursive -> ""
  | Recursive -> " rec"
;;

let pp_expr = pp_expr_at 0

let pp_definition formatter (Definition (recursive, pattern, value)) =
  pp_binding ("let" ^ pp_rec_flag recursive) formatter (pattern, value)
;;

let pp_program formatter =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,@,")
    pp_definition
    formatter
;;

let expr_to_string expression = Format.asprintf "%a" pp_expr expression
let program_to_string program = Format.asprintf "%a" pp_program program
