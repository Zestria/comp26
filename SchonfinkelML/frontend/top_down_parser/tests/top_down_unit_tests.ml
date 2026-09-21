[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Inline tests for the top-down parser: source -> AST, plus the operator
    table it is driven by.

    Every check is a [let%test], so the runner is what reports a failure and
    the predicates below only have to answer yes or no. *)

open Frontend.Common
module P = Top_down.Parser
module Operators = Top_down.Operators

(* The table of a test, written in the syntax of [--ops]. *)
let table src =
  match Operators.of_string src with
  | Ok t -> t
  | Error msg -> failwith ("the tests use an invalid operator table: " ^ msg)
;;

(* --- The predicates the tests are written with --- *)

(* The expression a source parses to, or [false] when it does not parse. *)
let check_expr ?ops src expected =
  match P.parse_expr ?ops src with
  | Ok e -> e = expected
  | Error _ -> false
;;

let check_error ?ops src =
  match P.parse_expr ?ops src with
  | Error _ -> true
  | Ok _ -> false
;;

let check_table src expected =
  match Operators.of_string src with
  | Ok t -> String.equal (Operators.string_of t) expected
  | Error _ -> false
;;

let check_bad_table src =
  match Operators.of_string src with
  | Error _ -> true
  | Ok _ -> false
;;

(* printing a table and reading it back gives the same table *)
let check_round_trip src =
  match Operators.of_string src with
  | Error _ -> false
  | Ok t ->
    (match Operators.of_string (Operators.string_of t) with
     | Ok t' -> t' = t
     | Error _ -> false)
;;

(* Stress tests bounded by [?max_budget]: the parser must not raise even on
   pathological inputs; it must return an [Error] instead. *)
let does_not_raise src =
  try
    ignore (P.parse_expr src);
    true
  with
  | _ -> false
;;

(* --- The operator table: custom tables via [~ops] ---

<<<<<<< HEAD
let%test "integer literal" = check_expr "42" (econsti 42)
let%test "identifier" = check_expr "x" (Ident "x")
let%test "identifier with digits and a quote" = check_expr "x1'_2" (Ident "x1'_2")
let%test "uppercase identifier" = check_expr "Foo" (Ident "Foo")

(* --- Unit --- *)

let%test "unit" = check_expr "()" eunit
let%test "unit as an argument" = check_expr "f ()" (eapp (Ident "f") eunit)

let%test "unit in if branches" =
  check_expr "if x then () else ()" (eif (Ident "x") eunit eunit)
;;

(* --- Application --- *)

let%test "application is left-associative" =
  check_expr "x y z" (eapp (eapp (Ident "x") (Ident "y")) (Ident "z"))
;;

let%test "application with parens" =
  check_expr "f (g x)" (eapp (Ident "f") (eapp (Ident "g") (Ident "x")))
;;

let%test "application of several atoms" =
  check_expr "f 1 2" (eapp (eapp (Ident "f") (econsti 1)) (econsti 2))
;;

let%test "application binds tighter than plus" =
  check_expr "f x + y" (eadd (eapp (Ident "f") (Ident "x")) (Ident "y"))
;;

(* --- Operators --- *)

let%test "* binds tighter than +" =
  check_expr "1 + 2 * 3" (eadd (econsti 1) (emul (econsti 2) (econsti 3)))
;;

let%test "* binds tighter than + (2)" =
  check_expr "1 * 2 + 3" (eadd (emul (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "- is left-associative" =
  check_expr "1 - 2 - 3" (esub (esub (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "+ is left-associative" =
  check_expr "1 + 2 + 3" (eadd (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "parens change the order" =
  check_expr "(1 + 2) * 3" (emul (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "division" = check_expr "1 / 2" (ediv (econsti 1) (econsti 2))
let%test "unary minus" = check_expr "- x" (euminus (Ident "x"))

let%test "unary minus below application" =
  check_expr "- f x" (euminus (eapp (Ident "f") (Ident "x")))
;;

let%test "binary and unary minus" =
  check_expr "1 - -2" (esub (econsti 1) (euminus (econsti 2)))
;;

let%test "binary minus" = check_expr "1 - 2" (esub (econsti 1) (econsti 2))
let%test "comparison <" = check_expr "a < b" (elt (Ident "a") (Ident "b"))
let%test "comparison >" = check_expr "a > b" (egt (Ident "a") (Ident "b"))
let%test "comparison =" = check_expr "a = b" (eeq (Ident "a") (Ident "b"))

let%test "comparison is below +" =
  check_expr "a < b + c" (elt (Ident "a") (eadd (Ident "b") (Ident "c")))
;;

let%test "comparisons are left-associative" =
  check_expr "a = b = c" (eeq (eeq (Ident "a") (Ident "b")) (Ident "c"))
;;

let%test "comparison <=" = check_expr "a <= b" (elte (Ident "a") (Ident "b"))
let%test "comparison >=" = check_expr "a >= b" (egte (Ident "a") (Ident "b"))

(* --- if / fun --- *)

let%test "if" = check_expr "if x then 1 else 2" (eif (Ident "x") (econsti 1) (econsti 2))

let%test "if branches extend to the right" =
  check_expr
    "if x then 1 else 2 + 3"
    (eif (Ident "x") (econsti 1) (eadd (econsti 2) (econsti 3)))
;;

let%test "if with a comparison condition" =
  check_expr
    "if a < b then f x else g y"
    (eif
       (elt (Ident "a") (Ident "b"))
       (eapp (Ident "f") (Ident "x"))
       (eapp (Ident "g") (Ident "y")))
;;

let%test "nested if" =
  check_expr
    "if x then if y then 1 else 2 else 3"
    (eif (Ident "x") (eif (Ident "y") (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "if without else" = check_expr "if x then 1" (eif (Ident "x") (econsti 1) eunit)

let%test "if without else with a unit branch" =
  check_expr "if x then ()" (eif (Ident "x") eunit eunit)
;;

(* An [else] belongs to the innermost [if] that still wants one. *)
let%test "an else goes to the inner if" =
  check_expr
    "if x then if y then 1 else 2"
    (eif (Ident "x") (eif (Ident "y") (econsti 1) (econsti 2)) eunit)
;;

let%test "parens close an if without else" =
  check_expr
    "if x then (if y then 1) else 2"
    (eif (Ident "x") (eif (Ident "y") (econsti 1) eunit) (econsti 2))
;;

let%test "if without else as a let body" =
  check_expr
    "let x = 1 in if x then x"
    (elet "x" (econsti 1) (eif (Ident "x") (Ident "x") eunit))
;;

let%test "if without else as a fun body" =
  check_expr "fun x -> if x then 1" (elambda "x" (eif (Ident "x") (econsti 1) eunit))
;;

(* Reading back what [Common.pp_expr] writes. *)
let%test "the printed form parses back" =
  let e = eif (Ident "x") (eif (Ident "y") (econsti 1) eunit) (econsti 2) in
  P.parse_expr (string_of_expr e) = Ok e
;;

let%test "fun" = check_expr "fun x -> x" (elambda "x" (Ident "x"))

let%test "fun with two parameters" =
  check_expr "fun x y -> x + y" (elambda "x" (elambda "y" (eadd (Ident "x") (Ident "y"))))
;;

let%test "fun with an explicit fun" =
  check_expr "fun x -> fun y -> x" (elambda "x" (elambda "y" (Ident "x")))
;;

let%test "fun body extends to the right" =
  check_expr "fun x -> x + 1" (elambda "x" (eadd (Ident "x") (econsti 1)))
;;

(* --- let --- *)

let%test "let" = check_expr "let x = 1 in x" (elet "x" (econsti 1) (Ident "x"))

let%test "let with fun" =
  check_expr
    "let f = fun x -> x in f 2"
    (elet "f" (elambda "x" (Ident "x")) (eapp (Ident "f") (econsti 2)))
;;

let%test "let with parameter sugar" =
  check_expr
    "let f x = x + 1 in f 2"
    (elet "f" (elambda "x" (eadd (Ident "x") (econsti 1))) (eapp (Ident "f") (econsti 2)))
;;

let%test "let rec" =
  check_expr
    "let rec f x = f x in f"
    (eletrec "f" (elambda "x" (eapp (Ident "f") (Ident "x"))) (Ident "f"))
;;

let%test "nested let" =
  check_expr
    "let x = 1 in let y = 2 in x + y"
    (elet "x" (econsti 1) (elet "y" (econsti 2) (eadd (Ident "x") (Ident "y"))))
;;

(* --- Comments and whitespace --- *)

let%test "comments" = check_expr "(* c *) 1 (* d *)" (econsti 1)
let%test "newlines" = check_expr "1\n+\n2" (eadd (econsti 1) (econsti 2))
let%test "nested comments" = check_expr "(* a (* b *) c *) 1" (econsti 1)

let%test "extra spaces and parens" =
  check_expr "  (  1  +  2  )  " (eadd (econsti 1) (econsti 2))
;;

(* --- Names and keywords ---

   A keyword is only a keyword when it is a whole token: the lexer takes the
   longest match, so a name may start with one. *)

let%test "a name may start with let" = check_expr "letter" (Ident "letter")
let%test "the same name in parens" = check_expr "(letter)" (Ident "letter")
let%test "letters" = check_expr "letters" (Ident "letters")
let%test "letx is a name" = check_expr "letx" (Ident "letx")
let%test "letinx is one name" = check_expr "letinx" (Ident "letinx")
let%test "ifn is a name" = check_expr "ifn" (Ident "ifn")

let%test "two names separated by a space" =
  check_expr "f x" (eapp (Ident "f") (Ident "x"))
;;

(* Nothing glues: what is not a boundary for the lexer is application or an
   error for the parser. *)
let%test "letx=1inx" =
  check_expr "letx=1inx" (eeq (Ident "letx") (eapp (econsti 1) (Ident "inx")))
;;

let%test "ifn<1then0else1" =
  check_expr "ifn<1then0else1" (elt (Ident "ifn") (eapp (econsti 1) (Ident "then0else1")))
;;

let%test "no boundary after ->" = check_error "funx->x"
let%test "no boundary after in" = check_error "let x = 1 inx"

(* --- CPS factorial and fibonacci --- *)

let%test "CPS factorial" =
  check_expr
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
  check_expr
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

(* --- The operator table --- *)

(* Associativity is data: the same source reads differently. *)
=======
   Associativity is data: the same source reads differently. *)
>>>>>>> 09dd69f (test: separate common and top-down unit tests)
let%test "left +" =
  check_expr
    ~ops:(table "left 6 +")
    "1 + 2 + 3"
    (eadd (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

let%test "right +" =
  check_expr
    ~ops:(table "right 6 +")
    "1 + 2 + 3"
    (eadd (econsti 1) (eadd (econsti 2) (econsti 3)))
;;

(* So is precedence: here [+] is the tighter of the two. *)
let%test "+ tighter than *" =
  check_expr
    ~ops:(table "left 9 +; left 7 *")
    "1 + 2 * 3"
    (emul (eadd (econsti 1) (econsti 2)) (econsti 3))
;;

(* The precedence of a prefix operator decides how much it takes. *)
let%test "a tight prefix -" =
  check_expr
    ~ops:(table "prefix 9 -; left 6 +")
    "- a + b"
    (eadd (euminus (Ident "a")) (Ident "b"))
;;

let%test "a loose prefix -" =
  check_expr
    ~ops:(table "prefix 3 -; left 6 +")
    "- a + b"
    (euminus (eadd (Ident "a") (Ident "b")))
;;

(* Arity is data as well: without a prefix [-] there is no unary minus. *)
let%test "no prefix minus" = check_error ~ops:(table "left 6 -") "- a"
let%test "no infix minus" = check_error ~ops:(table "prefix 9 -") "a - b"

(* nonassoc forbids chaining an operator with itself, and with an operator of
   the same precedence. *)
let%test "= chained as left" =
  check_expr
    ~ops:(table "left 4 =")
    "a = b = c"
    (eeq (eeq (Ident "a") (Ident "b")) (Ident "c"))
;;

let%test "= chained as nonassoc" = check_error ~ops:(table "nonassoc 4 =") "a = b = c"

let%test "position of a chained =" =
  match P.parse_expr ~ops:(table "nonassoc 4 =") "a = b = c" with
  | Error e -> e.pos.pos_cnum = 6
  | Ok _ -> false
;;

let%test "= and < chained" = check_error ~ops:(table "nonassoc 4 = <") "a = b < c"

(* A table without an operator is a table without that operator. *)
let%test "no multiplication" = check_error ~ops:(table "left 6 +") "a * b"

(* The table only describes operators: [=] of a binding is still a token. *)
let%test "the table does not touch let" =
  check_expr
    ~ops:(table "right 6 +")
    "let x = 1 in x + x"
    (elet "x" (econsti 1) (eadd (Ident "x") (Ident "x")))
;;

(* --- The syntax of a table, as given to [--ops] --- *)

let%test "an empty table" = check_table "" ""
let%test "one group" = check_table "left 6 + -" "left 6 + -"

let%test "the default table" =
  check_table
    "left 4 = < > <= >=; left 6 + -; left 7 * /; prefix 9 -"
    (Operators.string_of Operators.default)
;;

let%test "spaces and tabs" = check_table "  left \t 6\t+  " "left 6 +"
let%test "an empty group" = check_table "left 6 +;" "left 6 +"

let%test "one spelling, two fixities" =
  check_table "prefix 9 -; left 6 -" "prefix 9 -; left 6 -"
;;

let%test "unknown fixity" = check_bad_table "fat 6 +"
let%test "no precedence" = check_bad_table "left"
let%test "no operators" = check_bad_table "left 6"
let%test "precedence is not a number" = check_bad_table "left + 6"
let%test "unknown operator" = check_bad_table "left 6 ^"
let%test "the same spelling twice" = check_bad_table "left 6 +; right 6 +"
let%test "the same spelling twice as a prefix" = check_bad_table "prefix 9 -; prefix 8 -"

(* printing a table and reading it back gives the same table *)
let%test "pp round-trip of an empty table" = check_round_trip ""
let%test "pp round-trip of left 6 +" = check_round_trip "left 6 +"
let%test "pp round-trip of prefix 9 -" = check_round_trip "prefix 9 -"

let%test "pp round-trip of the default table" =
  check_round_trip "left 4 = < >; left 6 + -; left 7 * /; prefix 9 -"
;;

let%test "pp round-trip of nonassoc 4 =" = check_round_trip "nonassoc 4 ="
let%test "pp round-trip of prefix 9 -; left 6 -" = check_round_trip "prefix 9 -; left 6 -"

(* a spelling can be looked up as an infix and as a prefix operator *)
let%test "find_infix/find_prefix on -" =
  let t = table "prefix 9 -; left 6 -" in
  match Operators.find_infix t MINUS, Operators.find_prefix t MINUS with
  | Some i, Some p -> i.Operators.prec = 6 && p.Operators.prec = 9
  | _ -> false
;;

let%test "lookups of an absent operator" =
  match
    ( Operators.find_infix (table "prefix 9 -") PLUS
    , Operators.find_prefix (table "left 6 +") PLUS )
  with
  | None, None -> true
  | _ -> false
;;

let%test "if without else" = check_error "if x then 1"
let%test "unit is not supported" = check_error "()"
let%test "empty parens" = check_error "( )"

(* --- Position-based error checks ---

   Everything that can go wrong is an [Error] here: the lexer raises
   [Lexing_error], and [parse_expr] catches it, so a source it refuses is a
   value rather than a crash. *)

<<<<<<< HEAD
let%test "empty input" = check_error ""
let%test "whitespace only" = check_error "   "
let%test "let without in" = check_error "let x = 1"
let%test "if without then" = check_error "if x"
let%test "if without a then branch" = check_error "if x then"
let%test "if without a condition" = check_error "if then 1 else 2"
let%test "else without a branch" = check_error "if x then 1 else"
let%test "stray else" = check_error "1 else 2"
let%test "unclosed paren" = check_error "(1"
let%test "extra paren" = check_error "1)"
let%test "unclosed parens around a unit" = check_error "( ()"
let%test "extra paren after a unit" = check_error "() )"
let%test "let without a name" = check_error "let = 1 in x"
let%test "keyword as a name" = check_error "let rec = 1 in x"
let%test "dangling plus" = check_error "1 +"
let%test "keyword as an expression" = check_error "in"

(* the lexer refuses a literal that does not fit in an [int] *)
let%test "literal overflow" = check_error "99999999999999999999"

=======
>>>>>>> 09dd69f (test: separate common and top-down unit tests)
let%test "offset of a missing in" =
  match P.parse_expr "let x = 1" with
  | Error e -> e.pos.pos_cnum = 9
  | Ok _ -> false
;;

let%test "offset of the end of the input" =
  match P.parse_expr "1 +" with
  | Error e -> e.pos.pos_cnum = 3
  | Ok _ -> false
;;

let%test "position of a missing in" =
  match P.parse_expr "let x = 1" with
  | Error e -> e.pos.pos_lnum = 1 && e.pos.pos_cnum - e.pos.pos_bol + 1 = 10
  | Ok _ -> false
;;

let%test "position of a dangling operator" =
  match P.parse_expr "1 +" with
  | Error e -> e.pos.pos_lnum = 1 && e.pos.pos_cnum - e.pos.pos_bol + 1 = 4
  | Ok _ -> false
;;

(* Lexing is interleaved with parsing: a lexing error is only reached when the
   lexer gets there, so a parser error earlier in the source wins.  In
   [1 ) %] the parser stops at [)] before the '%' is ever lexed. *)
let%test "a parser error before a later lexing error" =
  match P.parse_expr "1 ) %" with
  | Error e -> e.pos.pos_cnum = 2
  | Ok _ -> false
;;

(* --- Stress tests bounded by [?max_budget] ---

   The top-down parser bounds how many frames may be alive at once. Going
   over the budget is reported as an error rather than crashing. These
   pathological inputs have no bottom-up equivalent to compare against. *)

let%test "does not raise on 100 parens" = does_not_raise (String.make 100 '(')
let%test "does not raise on 100 minuses" = does_not_raise (String.make 100 '-')
