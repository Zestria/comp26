module Gen = QCheck.Gen
open Frontend.Common

let binop_ops = [ "+"; "-"; "*"; "/"; "<"; ">"; "=" ]
let is_binop op = List.mem op binop_ops

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
    let half = n / 2 in
    let gen_sub = gen_expr half in
    Gen.oneof_weighted
      [ 1, Gen.map (fun i -> Const (LitInt i)) gen_int
      ; 2, Gen.map (fun s -> Ident s) gen_name
      ; 3, Gen.map3 (fun op a b -> eapp (eapp (Ident op) a) b) gen_op gen_sub gen_sub
      ; 3, Gen.map2 (fun a b -> eapp a b) gen_sub gen_sub
      ; 2, Gen.map3 (fun c t e -> eif c t e) gen_sub gen_sub gen_sub
      ; 2, Gen.map2 (fun x v -> elet x v (Ident x)) gen_name gen_sub
      ; 1, Gen.map2 (fun x v -> eletrec x v (Ident x)) gen_name gen_sub
      ; 2, Gen.map2 (fun x body -> elambda x body) gen_name gen_sub
      ])
;;

let gen_expr_sized = Gen.sized gen_expr

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
  ; "("
  ; ")"
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
