[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Gen = QCheck.Gen
open SchonfinkelML_frontend.Common

let binop_ops = [ "+"; "-"; "*"; "/"; "<"; ">"; "=" ]

let names =
  [ "x"; "y"; "z"; "f"; "g"; "h"; "n"; "k"; "r"; "fact"; "fib"; "foo"; "bar"; "accum" ]
;;

let gen_name = Gen.oneof (List.map Gen.return names)
let gen_op = Gen.oneof (List.map Gen.return binop_ops)
let gen_int = Gen.int_range 0 20

let rec gen_expr n =
  if n <= 0
  then
    Gen.oneof
      [ Gen.map (fun i -> Const (LitInt i)) gen_int; Gen.map (fun s -> Ident s) gen_name ]
  else (
    let gen_half = gen_expr (n / 2) in
    let gen_third = gen_expr (n / 3) in
    let gen_pred = gen_expr (pred n) in
    Gen.oneof
      [ Gen.map3 (fun op a b -> eapp (eapp (Ident op) a) b) gen_op gen_half gen_half
      ; Gen.map2 eapp gen_half gen_half
      ; Gen.map3 eif gen_third gen_third gen_third
      ; Gen.map2 (fun c t -> eif c t eunit) gen_third gen_third
      ; Gen.map3 elet gen_name gen_half gen_half
      ; Gen.map3 eletrec gen_name gen_half gen_half
      ; Gen.map2 elambda gen_name gen_pred
      ; Gen.map euminus gen_half
      ])
;;

let gen_expr_sized = Gen.sized_size (Gen.int_bound 30) gen_expr

(* A table is a sequence of words, which is what [--ops] takes.  Every word
   carries the space that separates it from the next one, so that the
   concatenation is a well formed table whenever it parses at all. *)
let table_words =
  [ "left "
  ; "right "
  ; "nonassoc "
  ; "prefix "
  ; "+ "
  ; "- "
  ; "* "
  ; "/ "
  ; "= "
  ; "< "
  ; "> "
  ; "0 "
  ; "6 "
  ; "7 "
  ; "; "
  ]
;;

let gen_table_src =
  Gen.map
    (String.concat "")
    (Gen.list_size (Gen.int_range 0 12) (Gen.oneof (List.map Gen.return table_words)))
;;

(* Sources for the totality property: words of the accepted language. *)
let lexer_words =
  [ "x"
  ; "y"
  ; "f"
  ; "1"
  ; "0"
  ; "17"
  ; "+"
  ; "-"
  ; "*"
  ; "/"
  ; "="
  ; "<"
  ; ">"
  ; "<="
  ; ">="
  ; "("
  ; ")"
  ; "()"
  ; "let"
  ; "rec"
  ; "in"
  ; "if"
  ; "then"
  ; "else"
  ; "fun"
  ; "->"
  ]
;;

let gen_source =
  Gen.map
    (String.concat " ")
    (Gen.list_size (Gen.int_range 0 20) (Gen.oneof (List.map Gen.return lexer_words)))
;;
