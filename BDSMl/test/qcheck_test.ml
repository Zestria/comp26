[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast
open QCheck.Gen

let gen_var_name =
  oneof_list [ "x"; "y"; "z"; "a"; "b"; "c"; "f"; "g"; "n"; "m"; "x42"; "var1"; "myVar" ]
;;

let gen_binop =
  oneof_list
    [ Plus; Minus; Mult; Div; Equal; NotEqual; More; Less; EMore; ELess; And; Or ]
;;

let gen_pattern =
  oneof [ map (fun name -> PVar name) gen_var_name; return PUnit; return PUnderscore ]
;;

let rec gen_expr size =
  let atom =
    oneof
      [ map (fun name -> Var name) gen_var_name
      ; map (fun n -> Int n) (int_range 0 1000)
      ; map (fun value -> Bool value) bool
      ; return Unit
      ]
  in
  if size <= 1
  then atom
  else (
    let sub = gen_expr (size / 2) in
    oneof_weighted
      [ 8, atom
      ; 6, map3 (fun op left right -> BinOp (op, left, right)) gen_binop sub sub
      ; 2, map2 (fun op body -> UnOp (op, body)) (oneof_list [ Neg; Not ]) sub
      ; 4, map3 (fun condition yes no -> If (condition, yes, no)) sub sub sub
      ; ( 3
        , map3
            (fun (flag, pattern) value body -> Let (flag, pattern, value, body))
            (pair (oneof_list [ Rec; NonRec ]) gen_pattern)
            sub
            sub )
      ; 3, map2 (fun pattern body -> Abs (pattern, body)) gen_pattern sub
      ; 3, map2 (fun fn arg -> App (fn, arg)) sub sub
      ])
;;

let rec shrink_expr = function
  | Int _ | Var _ | Bool _ | Unit -> QCheck.Iter.empty
  | UnOp (op, body) ->
    let open QCheck.Iter in
    of_list [ body ] <+> (shrink_expr body >|= fun smaller -> UnOp (op, smaller))
  | BinOp (op, left, right) ->
    let open QCheck.Iter in
    of_list [ left; right ]
    <+> (shrink_expr left >|= fun smaller -> BinOp (op, smaller, right))
    <+> (shrink_expr right >|= fun smaller -> BinOp (op, left, smaller))
  | If (condition, yes, no) ->
    let open QCheck.Iter in
    of_list [ condition; yes; no ]
    <+> (shrink_expr condition >|= fun smaller -> If (smaller, yes, no))
    <+> (shrink_expr yes >|= fun smaller -> If (condition, smaller, no))
    <+> (shrink_expr no >|= fun smaller -> If (condition, yes, smaller))
  | Let (flag, pattern, value, body) ->
    let open QCheck.Iter in
    of_list [ value; body ]
    <+> (shrink_expr value >|= fun smaller -> Let (flag, pattern, smaller, body))
    <+> (shrink_expr body >|= fun smaller -> Let (flag, pattern, value, smaller))
  | Abs (pattern, body) ->
    let open QCheck.Iter in
    of_list [ body ] <+> (shrink_expr body >|= fun smaller -> Abs (pattern, smaller))
  | App (fn, arg) ->
    let open QCheck.Iter in
    of_list [ fn; arg ]
    <+> (shrink_expr fn >|= fun smaller -> App (smaller, arg))
    <+> (shrink_expr arg >|= fun smaller -> App (fn, smaller))
;;

let binop_text = function
  | Plus -> "+"
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

let unop_text = function
  | Neg -> "-"
  | Not -> "not"
;;

(* Every compound expression is parenthesized so both parsers must recover
   exactly the generated tree, regardless of operator precedence. *)
let rec source_of_expr = function
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool value -> string_of_bool value
  | Var name -> name
  | BinOp (op, left, right) ->
    Printf.sprintf
      "(%s %s %s)"
      (source_of_expr left)
      (binop_text op)
      (source_of_expr right)
  | UnOp (op, body) -> Printf.sprintf "(%s %s)" (unop_text op) (source_of_expr body)
  | If (condition, yes, no) ->
    Printf.sprintf
      "(if %s then %s else %s)"
      (source_of_expr condition)
      (source_of_expr yes)
      (source_of_expr no)
  | Let (flag, pattern, value, body) ->
    Printf.sprintf
      "(let %s%s = %s in %s)"
      (if flag = Rec then "rec " else "")
      (BDSMl_frontend.Printer.print_pattern pattern)
      (source_of_expr value)
      (source_of_expr body)
  | Abs (pattern, body) ->
    Printf.sprintf
      "(fun %s -> %s)"
      (BDSMl_frontend.Printer.print_pattern pattern)
      (source_of_expr body)
  | App (fn, arg) -> Printf.sprintf "(%s %s)" (source_of_expr fn) (source_of_expr arg)
;;

let parse_ll = BDSMl_frontend.LL_parser.Parser.parse_string_LL
let parse_lr = BDSMl_frontend.Parse.parse

let expr_matches source expected =
  let program = Ok [ StrEval expected ] in
  parse_ll source = program && parse_lr source = program
;;

let arb_expr = QCheck.make ~print:source_of_expr ~shrink:shrink_expr (gen_expr 20)
let arb_small_int = QCheck.make ~print:string_of_int (int_range 0 1000)
let arb_bool = QCheck.make ~print:string_of_bool bool
let arb_var_name = QCheck.make ~print:(fun name -> name) gen_var_name

let arb_two_ints =
  QCheck.make
    ~print:(fun (a, b) -> Printf.sprintf "%d,%d" a b)
    (pair (int_range 0 1000) (int_range 0 1000))
;;

let arb_three_ints =
  QCheck.make
    ~print:(fun (a, b, c) -> Printf.sprintf "%d,%d,%d" a b c)
    (triple (int_range 0 1000) (int_range 0 1000) (int_range 0 1000))
;;

let print_parse_roundtrip =
  QCheck.Test.make ~count:1000 ~name:"AST -> source -> LL/LR AST" arb_expr (fun expr ->
    expr_matches (source_of_expr expr ^ ";;") expr)
;;

let parse_int_literal =
  QCheck.Test.make ~count:300 ~name:"parse int literal" arb_small_int (fun n ->
    expr_matches (string_of_int n) (Int n))
;;

let parse_negative_int =
  QCheck.Test.make ~count:300 ~name:"parse negative int" arb_small_int (fun n ->
    expr_matches ("-" ^ string_of_int n) (UnOp (Neg, Int n)))
;;

let parse_bool_literal =
  QCheck.Test.make ~count:300 ~name:"parse bool literal" arb_bool (fun value ->
    expr_matches (string_of_bool value) (Bool value))
;;

let parse_unit_literal =
  QCheck.Test.make
    ~count:1
    ~name:"parse unit literal"
    (QCheck.make (return ()))
    (fun () -> expr_matches "()" Unit)
;;

let parse_var_identifier =
  QCheck.Test.make ~count:300 ~name:"parse var identifier" arb_var_name (fun name ->
    expr_matches name (Var name))
;;

let parse_app_associativity =
  let arb =
    QCheck.make
      ~print:(fun (fn, x, y) -> Printf.sprintf "%s %s %s" fn x y)
      (triple gen_var_name gen_var_name gen_var_name)
  in
  QCheck.Test.make
    ~count:300
    ~name:"parse application associativity"
    arb
    (fun (fn, x, y) ->
       expr_matches (Printf.sprintf "%s %s %s" fn x y) (App (App (Var fn, Var x), Var y)))
;;

let parse_binop_precedence_add_mult =
  QCheck.Test.make
    ~count:300
    ~name:"parse precedence + vs *"
    arb_three_ints
    (fun (a, b, c) ->
       expr_matches
         (Printf.sprintf "%d + %d * %d" a b c)
         (BinOp (Plus, Int a, BinOp (Mult, Int b, Int c))))
;;

let parse_binop_precedence_mult_add =
  QCheck.Test.make
    ~count:300
    ~name:"parse precedence * vs +"
    arb_three_ints
    (fun (a, b, c) ->
       expr_matches
         (Printf.sprintf "%d * %d + %d" a b c)
         (BinOp (Plus, BinOp (Mult, Int a, Int b), Int c)))
;;

let parse_if_then_else =
  QCheck.Test.make ~count:300 ~name:"parse if-then-else" arb_two_ints (fun (a, b) ->
    expr_matches
      (Printf.sprintf "if true then %d else %d" a b)
      (If (Bool true, Int a, Int b)))
;;

let parse_let_in =
  QCheck.Test.make ~count:300 ~name:"parse let-in" arb_two_ints (fun (a, b) ->
    expr_matches
      (Printf.sprintf "let x = %d in x + %d" a b)
      (Let (NonRec, PVar "x", Int a, BinOp (Plus, Var "x", Int b))))
;;

let () =
  QCheck_runner.run_tests_main
    [ print_parse_roundtrip
    ; parse_int_literal
    ; parse_negative_int
    ; parse_bool_literal
    ; parse_unit_literal
    ; parse_var_identifier
    ; parse_app_associativity
    ; parse_binop_precedence_add_mult
    ; parse_binop_precedence_mult_add
    ; parse_if_then_else
    ; parse_let_in
    ]
;;
