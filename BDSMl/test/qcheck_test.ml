[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.LL_parser.Parser
open Frontend.Ast
open QCheck

let gen_var_name =
  Gen.(
    oneof
      [ pure "x"
      ; pure "y"
      ; pure "z"
      ; pure "a"
      ; pure "b"
      ; pure "c"
      ; pure "f"
      ; pure "g"
      ; pure "n"
      ; pure "m"
      ; pure "x42"
      ; pure "var1"
      ; pure "myVar"
      ])
;;

let gen_binop =
  Gen.(
    oneof
      [ pure Plus
      ; pure Minus
      ; pure Mult
      ; pure Div
      ; pure Equal
      ; pure NotEqual
      ; pure More
      ; pure Less
      ; pure EMore
      ; pure ELess
      ; pure And
      ; pure Or
      ])
;;

let gen_pattern =
  Gen.(oneof [ map (fun x -> PVar x) gen_var_name; pure PUnit; pure PUnderscore ])
;;

let rec gen_expr size =
  let gen_param = Gen.map (fun v -> PVar v) gen_var_name in
  let gen_var = map (fun x -> Var x) gen_var_name in
  let gen_int = map (fun i -> Int i) small_int in
  let gen_bool = map (fun b -> Bool b) bool in
  let gen_unit = pure Unit in
  if size <= 1
  then oneof [ gen_var; gen_int; gen_bool; gen_unit ]
  else (
    let under_expr = gen_expr (size / 2) in
    let gen_binop_e =
      map3 (fun op x1 x2 -> BinOp (op, x1, x2)) gen_binop under_expr under_expr
    in
    let gen_unop_e =
      oneof
        [ map (fun e -> UnOp (Neg, e)) under_expr
        ; map (fun e -> UnOp (Not, e)) under_expr
        ]
    in
    let gen_if = map3 (fun c t e -> If (c, t, e)) under_expr under_expr under_expr in
    let gen_let =
      let gen_rec_flag = oneof [ return Rec; return NonRec ] in
      map4 (fun r p b s -> Let (r, p, b, s)) gen_rec_flag gen_param under_expr under_expr
    in
    let gen_abs = map2 (fun p body -> Abs (p, body)) gen_param under_expr in
    let gen_app = map2 (fun x y -> App (x, y)) under_expr under_expr in
    frequency
      [ 8, gen_int
      ; 7, gen_var
      ; 2, gen_bool
      ; 1, gen_unit
      ; 6, gen_binop_e
      ; 2, gen_unop_e
      ; 4, gen_if
      ; 3, gen_let
      ; 3, gen_abs
      ; 3, gen_app
      ])
;;

let rec shrink_expr = function
  | Int _ | Var _ | Bool _ | Unit -> Iter.empty
  | UnOp (op, e) ->
    let open Iter in
    of_list [ e ] <+> (shrink_expr e >|= fun e' -> UnOp (op, e'))
  | BinOp (op, x1, x2) ->
    let open Iter in
    of_list [ x1; x2 ]
    <+> (shrink_expr x1 >|= fun e1 -> BinOp (op, e1, x2))
    <+> (shrink_expr x2 >|= fun e2 -> BinOp (op, x1, e2))
  | If (c, t, e) ->
    let open Iter in
    of_list [ c; t; e ]
    <+> (shrink_expr c >|= fun c' -> If (c', t, e))
    <+> (shrink_expr t >|= fun t' -> If (c, t', e))
    <+> (shrink_expr e >|= fun e' -> If (c, t, e'))
  | Let (rf, pat, bound, body) ->
    let open Iter in
    of_list [ body; bound ]
    <+> (shrink_expr bound >|= fun bound' -> Let (rf, pat, bound', body))
    <+> (shrink_expr body >|= fun body' -> Let (rf, pat, bound, body'))
  | App (x1, x2) ->
    let open Iter in
    of_list [ x1; x2 ]
    <+> (shrink_expr x1 >|= fun e1 -> App (e1, x2))
    <+> (shrink_expr x2 >|= fun e2 -> App (x1, e2))
  | Abs (x1, x2) ->
    let open Iter in
    of_list [ x2 ] <+> (shrink_expr x2 >|= fun e2 -> Abs (x1, e2))
;;

let arb_expr = QCheck.make ~print:show_expr ~shrink:shrink_expr (gen_expr 20)
let gen_small_int = int_range 0 1000
let arb_small_int = QCheck.make ~print:string_of_int gen_small_int
let arb_bool = QCheck.make ~print:Bool.to_string bool

let arb_two_ints =
  QCheck.make
    ~print:(fun (a, b) -> Printf.sprintf "%d,%d" a b)
    (pair gen_small_int gen_small_int)
;;

let arb_three_ints =
  QCheck.make
    ~print:(fun (a, b, c) -> Printf.sprintf "%d,%d,%d" a b c)
    (triple gen_small_int gen_small_int gen_small_int)
;;

let arb_var_name = QCheck.make gen_var_name

let parse_after_print expr =
  let s = String.concat "" [ show_expr expr; " ;;" ] in
  match parser s with
  | Ok [ StrEval expr' ] -> Ok expr'
  | Ok _ -> Error "expected expression"
  | Error msg -> Error (Printf.sprintf "parse error on `%s`: %s" s msg)
;;

let parse_single_expr s =
  match parser s with
  | Ok [ StrEval expr ] -> Ok expr
  | Ok _ -> Error "expected single expression"
  | Error msg -> Error msg
;;

let same_expr a b = String.equal (show_expr a) (show_expr b)

let expr_matches s expected =
  match parse_single_expr s with
  | Ok expr -> same_expr expr expected
  | Error _ -> false
;;

let print_parse_roundtrip =
  QCheck.Test.make
    ~count:1000
    ~name:"AST -> print_expr -> parser -> AST"
    arb_expr
    (fun expr ->
       match parse_after_print expr with
       | Ok expr' ->
         let original_ast_str = show_expr expr in
         let parsed_ast_str = show_expr expr' in
         if original_ast_str = parsed_ast_str
         then true
         else (
           Printf.eprintf
             "\nORIGINAL AST: %s\nPARSED AST:   %s\n"
             original_ast_str
             parsed_ast_str;
           false)
       | Error msg ->
         Printf.eprintf "\n%s\n" msg;
         false)
;;

let parse_int_literal =
  QCheck.Test.make ~count:300 ~name:"parse int literal" arb_small_int (fun n ->
    expr_matches (string_of_int n) (Int n))
;;

let parse_negative_int =
  QCheck.Test.make ~count:300 ~name:"parse negative int" arb_small_int (fun n ->
    expr_matches (String.concat "" [ "-"; string_of_int n ]) (UnOp (Neg, Int n)))
;;

let parse_bool_literal =
  QCheck.Test.make ~count:300 ~name:"parse bool literal" arb_bool (fun b ->
    let s = if b then "true" else "false" in
    expr_matches s (Bool b))
;;

let parse_unit_literal =
  QCheck.Test.make
    ~count:1
    ~name:"parse unit literal"
    (QCheck.make (pure ()))
    (fun () -> expr_matches "()" Unit)
;;

let parse_var_identifier =
  QCheck.Test.make ~count:300 ~name:"parse var identifier" arb_var_name (fun name ->
    expr_matches name (Var name))
;;

let parse_app_associativity =
  QCheck.Test.make
    ~count:300
    ~name:"parse application associativity"
    (QCheck.make
       ~print:(fun (f, x, y) -> Printf.sprintf "%s %s %s" f x y)
       (triple gen_var_name gen_var_name gen_var_name))
    (fun (f, x, y) ->
       let s = Printf.sprintf "%s %s %s" f x y in
       expr_matches s (App (App (Var f, Var x), Var y)))
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
