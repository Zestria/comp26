(* open Ast *)

let print_binop = function
  | Ast.Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "="
  | NotEqual -> "<>"
  | More -> ">"
  | Less -> "<"
  | EMore -> ">="
  | ELess -> "<="
  | And -> "&&"
  | Or -> "||"
;;

let print_pattern : Ast.pattern -> string = function
  | Ast.PVar x -> x
  | PUnit -> "()"
  | PUnderscore -> "_"
;;

let print_unop = function
  | Ast.Neg -> "-"
  | Not -> "not"
;;

let rec print_expr_ast : Ast.expr -> string = function
  | Ast.Int x -> Printf.sprintf "EInt %d" x
  | Var v -> Printf.sprintf "EVar %s" v
  | BinOp (op, e1, e2) ->
    Printf.sprintf
      "EBinOp ('%s', %s, %s)"
      (print_binop op)
      (print_expr_ast e1)
      (print_expr_ast e2)
  | UnOp (op, x) -> Printf.sprintf "EUnOp (%s, %s)" (print_unop op) (print_expr_ast x)
  | If (c, t, e) ->
    Printf.sprintf
      "EIf (%s, %s, %s)"
      (print_expr_ast c)
      (print_expr_ast t)
      (print_expr_ast e)
  | Let (r, p, e, ep) ->
    let rec_str = if r = Rec then " rec" else "" in
    Printf.sprintf
      "ELet%s (%s = %s, %s)"
      rec_str
      (print_pattern p)
      (print_expr_ast e)
      (print_expr_ast ep)
  | Abs (p, body) ->
    Printf.sprintf "EAbs (%s, %s)" (print_pattern p) (print_expr_ast body)
  | App (f, arg) -> Printf.sprintf "EApp (%s, %s)" (print_expr_ast f) (print_expr_ast arg)
  | Unit -> "EUnit"
  | Bool b -> Printf.sprintf "EBool %b" b
;;

let rec print_expr (expr : Ast.expr) : string =
  match expr with
  | Int x -> string_of_int x
  | Var x -> x
  | BinOp (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (print_expr e1) (print_binop op) (print_expr e2)
  | UnOp (Neg, x) -> Printf.sprintf "(-%s)" (print_expr x)
  | UnOp (Not, x) -> Printf.sprintf "(not %s)" (print_expr x)
  | If (c, t, e) ->
    Printf.sprintf "if %s then %s else %s" (print_expr c) (print_expr t) (print_expr e)
  | Let (r, p, e, ep) ->
    (match r with
     | Rec ->
       Printf.sprintf
         "let rec %s = %s in %s"
         (print_pattern p)
         (print_expr e)
         (print_expr ep)
     | NonRec ->
       Printf.sprintf "let %s = %s in %s" (print_pattern p) (print_expr e) (print_expr ep))
  | Abs (p, body) -> Printf.sprintf "fun %s -> %s" (print_pattern p) (print_expr body)
  | App (f, arg) -> Printf.sprintf "(%s %s)" (print_expr f) (print_expr arg)
  | Unit -> "()"
  | Bool b -> string_of_bool b
;;

let print_program program =
  let print_item = function
    | Ast.StrEval expr -> print_expr expr
    | StrLet (r, p, e) ->
      let rec_str =
        match r with
        | Rec -> " rec"
        | NonRec -> ""
      in
      Printf.sprintf "let%s %s = %s" rec_str (print_pattern p) (print_expr e)
  in
  String.concat "\n" (List.map print_item program)
;;

let print_ast program =
  let print_item = function
    | Ast.StrEval expr -> print_expr_ast expr
    | StrLet (r, p, e) ->
      let rec_str =
        match r with
        | Rec -> " rec"
        | NonRec -> ""
      in
      Printf.sprintf "ELet%s (%s = %s)" rec_str (print_pattern p) (print_expr_ast e)
  in
  String.concat "\n" (List.map print_item program)
;;
