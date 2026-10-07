open Anf

let pp_id fmt id = Format.fprintf fmt "%s#%d" id.hint id.stamp

let pp_imm fmt = function
  | Constant (Ast.Integer n) -> Format.pp_print_int fmt n
  | Constant (Ast.Boolean b) -> Format.pp_print_bool fmt b
  | Constant Ast.Unit -> Format.pp_print_string fmt "()"
  | Local id -> pp_id fmt id
  | External name -> Format.fprintf fmt "external(%S)" name
  | Builtin builtin -> Format.fprintf fmt "builtin(%s)" (builtin_name builtin)
;;

let rec pp_term fmt = function
  | Result rhs -> pp_compound fmt rhs
  | Let (id, rhs, body) ->
    Format.fprintf fmt "@[<v 0>let %a = %a in@,%a@]" pp_id id pp_compound rhs pp_term body
  | Let_rec (fn, parameter, body, continuation) ->
    Format.fprintf
      fmt
      "@[<v 0>let rec %a %a =@;<1 2>%a@,in@,%a@]"
      pp_id
      fn
      pp_id
      parameter
      pp_term
      body
      pp_term
      continuation

and pp_compound fmt = function
  | Value value -> pp_imm fmt value
  | Apply (fn, arg) -> Format.fprintf fmt "apply %a %a" pp_imm fn pp_imm arg
  | Unary_primitive (op, arg) ->
    Format.fprintf fmt "%s %a" (builtin_name (Unary op)) pp_imm arg
  | Binary_primitive (op, left, right) ->
    Format.fprintf fmt "%a %s %a" pp_imm left (builtin_name (Binary op)) pp_imm right
  | Lambda (parameter, body) ->
    Format.fprintf fmt "@[<v 0>(fun %a ->@;<1 2>%a)@]" pp_id parameter pp_term body
  | If (condition, yes, no) ->
    Format.fprintf
      fmt
      "@[<v 0>(if %a then@;<1 2>%a@,else@;<1 2>%a)@]"
      pp_imm
      condition
      pp_term
      yes
      pp_term
      no
;;

let pp_definition fmt = function
  | Definition (id, rhs) ->
    Format.fprintf fmt "@[<v 0>let %a =@;<1 2>%a@]" pp_id id pp_term rhs
  | Recursive_definition (fn, parameter, body) ->
    Format.fprintf
      fmt
      "@[<v 0>let rec %a %a =@;<1 2>%a@]"
      pp_id
      fn
      pp_id
      parameter
      pp_term
      body
;;

let pp_program fmt program =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,@,")
    pp_definition
    fmt
    program
;;

let show_term term = Format.asprintf "%a" pp_term term
let show_program program = Format.asprintf "@[<v>%a@]" pp_program program
