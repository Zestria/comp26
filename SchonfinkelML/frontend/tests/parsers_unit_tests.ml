[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Shared inline tests: every check here describes a property of the
    [expr] grammar that both parsers are expected to agree on, and each
    [let%test] runs it against the top-down parser and the menhir-generated
    bottom-up parser in turn. *)

open Frontend.Common
module Top_down = Top_down.Parser

(* --- Bottom-up adapter: reduce [program] to a single [expr] --- *)

let parse_expr_bottom_up (src : string) : (expr, unit) result =
  try
    match
      Frontend.Bottom_up_parser.program Frontend.Lexer.token (Lexing.from_string src)
    with
    | [ StrEval e ] -> Ok e
    | _ -> Error ()
  with
  | Frontend.Bottom_up_parser.Error -> Error ()
  | Frontend.Lexer.Lexing_error _ -> Error ()
;;

(* --- Predicates: run both engines, report which one disagrees --- *)

let check_expr_both ?ops src expected =
  let top_down_ok =
    match Top_down.parse_expr ?ops src with
    | Ok e -> e = expected
    | Error _ ->
      Printf.eprintf
        "[top-down]  %S: expected %s, got a parse error\n"
        src
        (string_of_expr expected);
      false
  in
  (* the operator table is a top-down-only feature: skip the bottom-up
     side of the comparison when a non-default table is requested *)
  let bottom_up_ok =
    match ops with
    | Some _ -> true
    | None ->
      (match parse_expr_bottom_up src with
       | Ok e -> e = expected
       | Error () ->
         Printf.eprintf
           "[bottom-up] %S: expected %s, got a parse error\n"
           src
           (string_of_expr expected);
         false)
  in
  top_down_ok && bottom_up_ok
;;

let check_error_both ?ops src =
  let top_down_ok =
    match Top_down.parse_expr ?ops src with
    | Error _ -> true
    | Ok e ->
      Printf.eprintf
        "[top-down]  %S: expected a parse error, got %s\n"
        src
        (string_of_expr e);
      false
  in
  let bottom_up_ok =
    match ops with
    | Some _ -> true
    | None ->
      (match parse_expr_bottom_up src with
       | Error () -> true
       | Ok e ->
         Printf.eprintf
           "[bottom-up] %S: expected a parse error, got %s\n"
           src
           (string_of_expr e);
         false)
  in
  top_down_ok && bottom_up_ok
;;

let does_not_raise_both src =
  let safe f =
    try
      ignore (f src);
      true
    with
    | _ -> false
  in
  safe (fun s -> Top_down.parse_expr s) && safe parse_expr_bottom_up
;;

(* --- Literals and identifiers --- *)

let%test "integer literal" = check_expr_both "67" (econsti 67)
let%test "zero literal" = check_expr_both "0" (econsti 0)
let%test "single-digit literal" = check_expr_both "7" (econsti 7)
let%test "multi-digit literal" = check_expr_both "123456789" (econsti 123456789)
let%test "identifier" = check_expr_both "x" (Ident "x")
let%test "single-letter identifier" = check_expr_both "y" (Ident "y")
let%test "identifier with digits and a quote" = check_expr_both "x1'_2" (Ident "x1'_2")
let%test "identifier with underscore" = check_expr_both "foo_bar" (Ident "foo_bar")
let%test "uppercase identifier" = check_expr_both "Foo" (Ident "Foo")
let%test "mixed-case identifier" = check_expr_both "aBc" (Ident "aBc")

(* --- Application --- *)

let%test "application is left-associative" =
  check_expr_both "x y z" (eapp (eapp (Ident "x") (Ident "y")) (Ident "z"))
;;

let%test "application of four atoms" =
  check_expr_both
    "f x y z"
    (eapp (eapp (eapp (Ident "f") (Ident "x")) (Ident "y")) (Ident "z"))
;;

let%test "explicitly grouped application" =
  check_expr_both "(f x) y" (eapp (eapp (Ident "f") (Ident "x")) (Ident "y"))
;;

let%test "application with parens" =
  check_expr_both "f (g x)" (eapp (Ident "f") (eapp (Ident "g") (Ident "x")))
;;

let%test "nested parens on the right" =
  check_expr_both
    "f (g (h x))"
    (eapp (Ident "f") (eapp (Ident "g") (eapp (Ident "h") (Ident "x"))))
;;

let%test "application of several atoms" =
  check_expr_both "f 1 2" (eapp (eapp (Ident "f") (econsti 1)) (econsti 2))
;;

let%test "application binds tighter than plus" =
  check_expr_both "f x + y" (eadd (eapp (Ident "f") (Ident "x")) (Ident "y"))
;;

let%test "applications on both sides of +" =
  check_expr_both
    "f x + g y"
    (eadd (eapp (Ident "f") (Ident "x")) (eapp (Ident "g") (Ident "y")))
;;

(* --- Operators (default table / declared menhir precedence) --- *)

let%test "* binds tighter than +" =
  check_expr_both "1 + 2 * 3" (eadd (econsti 1) (emul (econsti 2) (econsti 3)))
;;

let%test "* binds tighter than + (2)" =
  check_expr_both "1 * 2 + 3" (eadd (emul (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "- is left-associative" =
  check_expr_both "1 - 2 - 3" (esub (esub (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "+ is left-associative" =
  check_expr_both "1 + 2 + 3" (eadd (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "parens change the order" =
  check_expr_both "(1 + 2) * 3" (emul (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "division" = check_expr_both "1 / 2" (ediv (econsti 1) (econsti 2))

let%test "multiplication is left-associative" =
  check_expr_both "1 * 2 * 3" (emul (emul (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "division is left-associative" =
  check_expr_both "1 / 2 / 3" (ediv (ediv (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "plus and minus share a level" =
  check_expr_both "1 + 2 - 3" (esub (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "minus then plus" =
  check_expr_both "1 - 2 + 3" (eadd (esub (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "a longer mixed chain" =
  check_expr_both
    "1 + 2 * 3 - 4 / 5"
    (esub
       (eadd (econsti 1) (emul (econsti 2) (econsti 3)))
       (ediv (econsti 4) (econsti 5)))
;;

let%test "comparison <" = check_expr_both "a < b" (elt (Ident "a") (Ident "b"))
let%test "comparison >" = check_expr_both "a > b" (egt (Ident "a") (Ident "b"))
let%test "comparison =" = check_expr_both "a = b" (eeq (Ident "a") (Ident "b"))

let%test "comparison is below +" =
  check_expr_both "a < b + c" (elt (Ident "a") (eadd (Ident "b") (Ident "c")))
;;

let%test "comparison between two sums" =
  check_expr_both
    "a + b < c + d"
    (elt (eadd (Ident "a") (Ident "b")) (eadd (Ident "c") (Ident "d")))
;;

let%test "comparisons are left-associative" =
  check_expr_both "a = b = c" (eeq (eeq (Ident "a") (Ident "b")) (Ident "c"))
;;

let%test "comparison <=" = check_expr_both "a <= b" (elte (Ident "a") (Ident "b"))
let%test "comparison >=" = check_expr_both "a >= b" (egte (Ident "a") (Ident "b"))
let%test "unary minus" = check_expr_both "- x" (euminus (Ident "x"))

let%test "unary minus below application" =
  check_expr_both "- f x" (euminus (eapp (Ident "f") (Ident "x")))
;;

let%test "binary and unary minus" =
  check_expr_both "1 - -2" (esub (econsti 1) (euminus (econsti 2)))
;;

let%test "binary minus" = check_expr_both "1 - 2" (esub (econsti 1) (econsti 2))

let%test "unary minus is tighter than multiplication" =
  check_expr_both "- a * b" (emul (euminus (Ident "a")) (Ident "b"))
;;

let%test "unary minus on the right of multiplication" =
  check_expr_both "a * - b" (emul (Ident "a") (euminus (Ident "b")))
;;

let%test "unary minus on the right of plus" =
  check_expr_both "a + - b" (eadd (Ident "a") (euminus (Ident "b")))
;;

let%test "double unary minus" = check_expr_both "- - a" (euminus (euminus (Ident "a")))

let%test "unary minus over a parenthesised sum" =
  check_expr_both "- (a + b)" (euminus (eadd (Ident "a") (Ident "b")))
;;

(* --- if / fun  --- *)

let%test "if" =
  check_expr_both "if x then 1 else 2" (eif (Ident "x") (econsti 1) (econsti 2))
;;

let%test "if branches extend to the right" =
  check_expr_both
    "if x then 1 else 2 + 3"
    (eif (Ident "x") (econsti 1) (eadd (econsti 2) (econsti 3)))
;;

let%test "if with a comparison condition" =
  check_expr_both
    "if a < b then f x else g y"
    (eif
       (elt (Ident "a") (Ident "b"))
       (eapp (Ident "f") (Ident "x"))
       (eapp (Ident "g") (Ident "y")))
;;

let%test "nested if" =
  check_expr_both
    "if x then if y then 1 else 2 else 3"
    (eif (Ident "x") (eif (Ident "y") (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "else-if chain" =
  check_expr_both
    "if x then 1 else if y then 2 else 3"
    (eif (Ident "x") (econsti 1) (eif (Ident "y") (econsti 2) (econsti 3)))
;;

let%test "fun" = check_expr_both "fun x -> x" (elambda "x" (Ident "x"))

let%test "fun with two parameters" =
  check_expr_both
    "fun x y -> x + y"
    (elambda "x" (elambda "y" (eadd (Ident "x") (Ident "y"))))
;;

let%test "fun with three parameters" =
  check_expr_both "fun x y z -> x" (elambda "x" (elambda "y" (elambda "z" (Ident "x"))))
;;

let%test "fun with an explicit fun" =
  check_expr_both "fun x -> fun y -> x" (elambda "x" (elambda "y" (Ident "x")))
;;

let%test "fun body extends to the right" =
  check_expr_both "fun x -> x + 1" (elambda "x" (eadd (Ident "x") (econsti 1)))
;;

let%test "fun returning an if" =
  check_expr_both
    "fun x -> if x then 1 else 2"
    (elambda "x" (eif (Ident "x") (econsti 1) (econsti 2)))
;;

let%test "if returning funs" =
  check_expr_both
    "if x then fun y -> y else fun z -> z"
    (eif (Ident "x") (elambda "y" (Ident "y")) (elambda "z" (Ident "z")))
;;

(* --- let --- *)

let%test "let" = check_expr_both "let x = 1 in x" (elet "x" (econsti 1) (Ident "x"))

let%test "let with a non-identifier body" =
  check_expr_both "let x = 1 in 2" (elet "x" (econsti 1) (econsti 2))
;;

let%test "let with fun" =
  check_expr_both
    "let f = fun x -> x in f 2"
    (elet "f" (elambda "x" (Ident "x")) (eapp (Ident "f") (econsti 2)))
;;

let%test "let with parameter sugar" =
  check_expr_both
    "let f x = x + 1 in f 2"
    (elet "f" (elambda "x" (eadd (Ident "x") (econsti 1))) (eapp (Ident "f") (econsti 2)))
;;

let%test "let with two parameters" =
  check_expr_both
    "let f x y = x + y in f 1 2"
    (elet
       "f"
       (elambda "x" (elambda "y" (eadd (Ident "x") (Ident "y"))))
       (eapp (eapp (Ident "f") (econsti 1)) (econsti 2)))
;;

let%test "let rec" =
  check_expr_both
    "let rec f x = f x in f"
    (eletrec "f" (elambda "x" (eapp (Ident "f") (Ident "x"))) (Ident "f"))
;;

let%test "let rec without a parameter" =
  check_expr_both
    "let rec f = fun x -> x in f 1"
    (eletrec "f" (elambda "x" (Ident "x")) (eapp (Ident "f") (econsti 1)))
;;

let%test "nested let" =
  check_expr_both
    "let x = 1 in let y = 2 in x + y"
    (elet "x" (econsti 1) (elet "y" (econsti 2) (eadd (Ident "x") (Ident "y"))))
;;

let%test "three nested lets" =
  check_expr_both
    "let x = 1 in let y = 2 in let z = 3 in x + y + z"
    (elet
       "x"
       (econsti 1)
       (elet
          "y"
          (econsti 2)
          (elet "z" (econsti 3) (eadd (eadd (Ident "x") (Ident "y")) (Ident "z")))))
;;

let%test "shadowing let" =
  check_expr_both
    "let x = 1 in let x = 2 in x"
    (elet "x" (econsti 1) (elet "x" (econsti 2) (Ident "x")))
;;

let%test "let with a let as its value" =
  check_expr_both
    "let x = let y = 1 in y in x"
    (elet "x" (elet "y" (econsti 1) (Ident "y")) (Ident "x"))
;;

let%test "let with an application as the value" =
  check_expr_both
    "let x = f 1 in g x"
    (elet "x" (eapp (Ident "f") (econsti 1)) (eapp (Ident "g") (Ident "x")))
;;

(* --- Higher-order / integration-style checks --- *)

let%test "application of a fun literal" =
  check_expr_both
    "(fun x -> x + 1) 2"
    (eapp (elambda "x" (eadd (Ident "x") (econsti 1))) (econsti 2))
;;

let%test "fun passed as an argument" =
  check_expr_both
    "f (fun x -> x) 1"
    (eapp (eapp (Ident "f") (elambda "x" (Ident "x"))) (econsti 1))
;;

let%test "apply function" =
  check_expr_both
    "let apply f x = f x in apply (fun y -> y + 1) 2"
    (elet
       "apply"
       (elambda "f" (elambda "x" (eapp (Ident "f") (Ident "x"))))
       (eapp
          (eapp (Ident "apply") (elambda "y" (eadd (Ident "y") (econsti 1))))
          (econsti 2)))
;;

let%test "compose function" =
  check_expr_both
    "let compose f g = fun x -> f (g x) in compose (fun x -> x + 1) (fun y -> y * 2)"
    (elet
       "compose"
       (elambda
          "f"
          (elambda "g" (elambda "x" (eapp (Ident "f") (eapp (Ident "g") (Ident "x"))))))
       (eapp
          (eapp (Ident "compose") (elambda "x" (eadd (Ident "x") (econsti 1))))
          (elambda "y" (emul (Ident "y") (econsti 2)))))
;;

(* --- Comments and whitespace (Frontend.Lexer is shared) --- *)

let%test "comments" = check_expr_both "(* c *) 1 (* d *)" (econsti 1)
let%test "newlines" = check_expr_both "1\n+\n2" (eadd (econsti 1) (econsti 2))
let%test "nested comments" = check_expr_both "(* a (* b *) c *) 1" (econsti 1)

let%test "inline comment between operands" =
  check_expr_both "1 (* c *) + 2" (eadd (econsti 1) (econsti 2))
;;

let%test "comment before a fun parameter" =
  check_expr_both "fun (* c *) x -> x" (elambda "x" (Ident "x"))
;;

let%test "extra spaces and parens" =
  check_expr_both "  (  1  +  2  )  " (eadd (econsti 1) (econsti 2))
;;

let%test "tabs as whitespace" = check_expr_both "1\t+\t2" (eadd (econsti 1) (econsti 2))

(* --- Names and keywords: shared, since maximal munch is a lexer property --- *)

let%test "a name may start with let" = check_expr_both "letter" (Ident "letter")
let%test "the same name in parens" = check_expr_both "(letter)" (Ident "letter")
let%test "letx is a name" = check_expr_both "letx" (Ident "letx")
let%test "letinx is one name" = check_expr_both "letinx" (Ident "letinx")
let%test "ifn is a name" = check_expr_both "ifn" (Ident "ifn")

let%test "two names separated by a space" =
  check_expr_both "f x" (eapp (Ident "f") (Ident "x"))
;;

let%test "letx=1inx" =
  check_expr_both "letx=1inx" (eeq (Ident "letx") (eapp (econsti 1) (Ident "inx")))
;;

let%test "ifn<1then0else1" =
  check_expr_both
    "ifn<1then0else1"
    (elt (Ident "ifn") (eapp (econsti 1) (Ident "then0else1")))
;;

let%test "no boundary after ->" = check_error_both "funx->x"
let%test "no boundary after in" = check_error_both "let x = 1 inx"

(* --- CPS factorial and fibonacci --- *)

let%test "CPS factorial" =
  check_expr_both
    "let rec fact n k = if n = 0 then k 1 else fact (n - 1) (fun r -> k (n * r)) in fact \
     5 (fun x -> x)"
    (eletrec
       "fact"
       (elambda
          "n"
          (elambda
             "k"
             (eif
                (eeq (Ident "n") (econsti 0))
                (eapp (Ident "k") (econsti 1))
                (eapp
                   (eapp (Ident "fact") (esub (Ident "n") (econsti 1)))
                   (elambda "r" (eapp (Ident "k") (emul (Ident "n") (Ident "r"))))))))
       (eapp (eapp (Ident "fact") (econsti 5)) (elambda "x" (Ident "x"))))
;;

let%test "CPS fibonacci" =
  check_expr_both
    "let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
     -> k (a + b))) in fib 10 (fun x -> x)"
    (eletrec
       "fib"
       (elambda
          "n"
          (elambda
             "k"
             (eif
                (elt (Ident "n") (econsti 2))
                (eapp (Ident "k") (Ident "n"))
                (eapp
                   (eapp (Ident "fib") (esub (Ident "n") (econsti 1)))
                   (elambda
                      "a"
                      (eapp
                         (eapp (Ident "fib") (esub (Ident "n") (econsti 2)))
                         (elambda "b" (eapp (Ident "k") (eadd (Ident "a") (Ident "b"))))))))))
       (eapp (eapp (Ident "fib") (econsti 10)) (elambda "x" (Ident "x"))))
;;

(* --- Errors shared by construction (both grammars reject these) --- *)

let%test "empty input" = check_error_both ""
let%test "whitespace only" = check_error_both "   "
let%test "let without in" = check_error_both "let x = 1"
let%test "unclosed paren" = check_error_both "(1"
let%test "extra paren" = check_error_both "1)"
let%test "let without a name" = check_error_both "let = 1 in x"
let%test "keyword as a name" = check_error_both "let rec = 1 in x"
let%test "dangling plus" = check_error_both "1 +"
let%test "keyword as an expression" = check_error_both "in"
let%test "literal overflow" = check_error_both "99999999999999999999"

(* more malformed inputs both grammars should reject *)
let%test "double infix plus" = check_error_both "1 + + 2"
let%test "double infix star" = check_error_both "1 * * 2"
let%test "leading binary operator" = check_error_both "* a"
let%test "leading equality" = check_error_both "= 1"
let%test "fun without a parameter" = check_error_both "fun -> x"
let%test "fun without an arrow" = check_error_both "fun x"
let%test "fun with two params and no arrow" = check_error_both "fun x y"
let%test "let without a name or binding" = check_error_both "let in x"
let%test "let without an expression" = check_error_both "let x = in x"
let%test "if without a condition" = check_error_both "if then 1 else 2"
let%test "if without a then branch" = check_error_both "if x then else 2"
let%test "if without an else branch" = check_error_both "if x then 1 else"
let%test "only a comment" = check_error_both "(* c *)"
let%test "only a paren" = check_error_both "("
let%test "only a closing paren" = check_error_both ")"

(* --- Constructs both parsers are expected to reject --- *)

let%test "if with a condition but no branches" = check_error_both "if x then"
let%test "empty parens ( )" = check_error_both "( )"

(* --- Robustness: neither engine may raise or crash the process --- *)

let%test "does not raise on ((((((" = does_not_raise_both "(((((("
let%test "does not raise on letx=1inx" = does_not_raise_both "letx=1inx"
let%test "does not raise on let rec" = does_not_raise_both "let rec"
let%test "does not raise on -->" = does_not_raise_both "-->"
let%test "does not raise on 1000 letters" = does_not_raise_both (String.make 1000 'x')

let%test "does not raise on a long plus chain" =
  does_not_raise_both (String.concat " + " (List.init 100 (fun _ -> "1")))
;;

let%test "does not raise on a long application chain" =
  does_not_raise_both (String.concat " " (List.init 100 (fun _ -> "x")))
;;

let%test "does not raise on deeply parenthesised input" =
  does_not_raise_both (String.concat "" [ String.make 50 '('; "1"; String.make 50 ')' ])
;;
