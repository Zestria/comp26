[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck
module Ast = Vibecaml.Ast
module PPrint = Vibecaml.Pprint

let int value = Ast.Expr_constant (Const_int value)
let var name = Ast.Expr_ident name

let gen_name =
  let open Gen in
  let alphanum = oneof [ char_range '0' '9'; char_range 'a' 'z'; char_range 'A' 'Z' ] in
  let name = string_size ~gen:alphanum (int_range 1 10) in
  name
;;

let gen_identifier =
  let open Gen in
  oneof_weighted
    [ 30, gen_name
    ; 1, return "+"
    ; 1, return "-"
    ; 1, return "*"
    ; 1, return "/"
    ; 1, return "mod"
    ; 1, return "+."
    ; 1, return "-."
    ; 1, return "*."
    ; 1, return "/."
    ; 1, return "land"
    ; 1, return "lor"
    ; 1, return "lxor"
    ; 1, return "lsr"
    ; 1, return "lsl"
    ; 1, return "asr"
    ; 1, return "=="
    ; 1, return "<>"
    ; 1, return "<"
    ; 1, return ">"
    ; 1, return "<="
    ; 1, return ">="
    ]
;;

let gen_constant =
  let open Gen in
  oneof_weighted
    [ 20, map (fun i -> Ast.Const_int i) (int_range 0 100000)
    ; 5, map (fun b -> Ast.Const_bool b) bool
    ; 1, return Ast.Const_unit
    ]
;;

let gen_pattern =
  (* Name & constants patterns are highly more frequent than wildcard and bool ones *)
  let open Gen in
  oneof_weighted
    [ 1, return Ast.Pat_wildcard
    ; 10, map (fun n -> Ast.Pat_var n) gen_name
    ; 10, map (fun n -> Ast.Pat_constant (Const_int n)) (int_range 0 1000)
    ; 1, map (fun n -> Ast.Pat_constant (Const_bool n)) bool
    ]
;;

let gen_rec_flag =
  let open Gen in
  oneof [ return Ast.Recursive; return Ast.Non_recursive ]
;;

let gen_expr_depth =
  let open Gen in
  fix (fun gen size ->
    let gen_value_binding parser =
      map2
        (fun p e -> { Ast.binding_pattern = p; Ast.binding_expression = e })
        gen_pattern
        parser
    in
    if size <= 1
    then
      oneof
        [ map (fun c -> Ast.Expr_constant c) gen_constant
        ; map (fun n -> Ast.Expr_ident n) gen_identifier
        ]
    else (
      let sub = gen (size / 2) in
      let sub_tiny = gen (size / 3) in
      oneof_weighted
        [ 1, map (fun c -> Ast.Expr_constant c) gen_constant
        ; 1, map (fun n -> Ast.Expr_ident n) gen_identifier
        ; ( 100
          , map4
              (fun f b bs e -> Ast.Expr_let (f, b, bs, e))
              gen_rec_flag
              (gen_value_binding sub_tiny)
              (list_size (int_range 0 3) (gen_value_binding sub_tiny))
              sub )
        ; ( 50
          , map3
              (fun p ps e -> Ast.Expr_fun (p, ps, e))
              gen_pattern
              (list_size (int_range 0 3) gen_pattern)
              sub )
        ; 75, map2 (fun e1 e2 -> Ast.Expr_app (e1, e2)) sub_tiny sub
        ; 75, map3 (fun i t e -> Ast.Expr_if (i, t, e)) sub_tiny sub sub
        ; ( 50
          , map3
              (fun x1 x2 xs -> Ast.Expr_tuple (x1, x2, xs))
              sub_tiny
              sub_tiny
              (list_size (int_range 0 3) sub_tiny) )
        ]))
;;

let gen_expr_limit limit =
  Gen.sized_size (Gen.int_bound limit) (fun size -> gen_expr_depth size)
;;

let generate_expressions depth n =
  List.map PPrint.string_of_expression (Gen.generate ~n (gen_expr_limit depth))
;;
