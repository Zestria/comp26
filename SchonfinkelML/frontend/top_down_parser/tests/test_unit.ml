[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Unit tests for the top-down parser: source -> AST table, plus the operator
    table it is driven by. *)

open Frontend.Common
module P = Top_down.Parser
module Operators = Top_down.Operators

let failures = ref 0
let total = ref 0

let report_fail name ~expected ~actual =
  incr failures;
  Printf.printf "FAIL: %s\n  expected: %s\n  actual:   %s\n%!" name expected actual
;;

(* The table of a test, written in the syntax of [--ops]. *)
let table src =
  match Operators.of_string src with
  | Ok t -> t
  | Error msg -> failwith ("the tests use an invalid operator table: " ^ msg)
;;

let check_expr name ?ops src expected =
  incr total;
  match P.parse_expr ?ops src with
  | Ok e when e = expected -> ()
  | Ok e ->
    report_fail name ~expected:(string_of_expr expected) ~actual:(string_of_expr e)
  | Error err ->
    report_fail
      name
      ~expected:(string_of_expr expected)
      ~actual:("error: " ^ P.string_of_error err)
;;

let check_error name ?ops src =
  incr total;
  match P.parse_expr ?ops src with
  | Error _ -> ()
  | Ok e -> report_fail name ~expected:"parse error" ~actual:(string_of_expr e)
;;

let check_error_at name ?ops src expected_offset =
  incr total;
  match P.parse_expr ?ops src with
  | Error e when e.pos.pos_cnum = expected_offset -> ()
  | Error e ->
    report_fail
      name
      ~expected:(string_of_int expected_offset)
      ~actual:(string_of_int e.pos.pos_cnum)
  | Ok e -> report_fail name ~expected:"parse error" ~actual:(string_of_expr e)
;;

let check_error_at_pos name ?ops src ~line ~col =
  incr total;
  let expected = Printf.sprintf "%d:%d" line col in
  let actual (e : P.error) =
    Printf.sprintf "%d:%d" e.pos.pos_lnum (e.pos.pos_cnum - e.pos.pos_bol + 1)
  in
  match P.parse_expr ?ops src with
  | Error e when e.pos.pos_lnum = line && e.pos.pos_cnum - e.pos.pos_bol + 1 = col -> ()
  | Error e -> report_fail name ~expected ~actual:(actual e)
  | Ok e -> report_fail name ~expected ~actual:(string_of_expr e)
;;

let check_does_not_raise name src =
  incr total;
  let raised = ref None in
  (try ignore (P.parse_expr src) with
   | exn -> raised := Some (Printexc.to_string exn));
  match !raised with
  | None -> ()
  | Some s -> report_fail name ~expected:"Ok/Error without exceptions" ~actual:s
;;

(* --- Literals and identifiers --- *)

let () =
  check_expr "integer literal" "42" (econsti 42);
  check_expr "identifier" "x" (Ident "x");
  check_expr "identifier with digits and a quote" "x1'_2" (Ident "x1'_2");
  check_expr "uppercase identifier" "Foo" (Ident "Foo")
;;

(* --- Application --- *)

let () =
  let x = Ident "x"
  and y = Ident "y"
  and z = Ident "z"
  and f = Ident "f" in
  let g = Ident "g" in
  check_expr "application is left-associative" "x y z" (eapp (eapp x y) z);
  check_expr "application with parens" "f (g x)" (eapp f (eapp g x));
  check_expr
    "application of several atoms"
    "f 1 2"
    (eapp (eapp f (econsti 1)) (econsti 2));
  check_expr "application binds tighter than plus" "f x + y" (eadd (eapp f x) y)
;;

(* --- Operators --- *)

let () =
  let a = Ident "a"
  and b = Ident "b"
  and c = Ident "c"
  and x = Ident "x" in
  check_expr
    "* binds tighter than +"
    "1 + 2 * 3"
    (eadd (econsti 1) (emul (econsti 2) (econsti 3)));
  check_expr
    "* binds tighter than + (2)"
    "1 * 2 + 3"
    (eadd (emul (econsti 1) (econsti 2)) (econsti 3));
  check_expr
    "- is left-associative"
    "1 - 2 - 3"
    (esub (esub (econsti 1) (econsti 2)) (econsti 3));
  check_expr
    "+ is left-associative"
    "1 + 2 + 3"
    (eadd (eadd (econsti 1) (econsti 2)) (econsti 3));
  check_expr
    "parens change the order"
    "(1 + 2) * 3"
    (emul (eadd (econsti 1) (econsti 2)) (econsti 3));
  check_expr "division" "1 / 2" (ediv (econsti 1) (econsti 2));
  check_expr "unary minus" "- x" (euminus x);
  check_expr "unary minus below application" "- f x" (euminus (eapp (Ident "f") x));
  check_expr "binary and unary minus" "1 - -2" (esub (econsti 1) (euminus (econsti 2)));
  check_expr "binary minus" "1 - 2" (esub (econsti 1) (econsti 2));
  check_expr "comparison <" "a < b" (elt a b);
  check_expr "comparison >" "a > b" (egt a b);
  check_expr "comparison =" "a = b" (eeq a b);
  check_expr "comparison is below +" "a < b + c" (elt a (eadd b c));
  check_expr "comparisons are left-associative" "a = b = c" (eeq (eeq a b) c);
  (* The lexer has no two-character operators, so these are only errors. *)
  check_error "no <=" "a <= b";
  check_error "no >=" "a >= b"
;;

(* --- if / fun --- *)

let () =
  let x = Ident "x"
  and y = Ident "y" in
  check_expr "if" "if x then 1 else 2" (eif x (econsti 1) (econsti 2));
  check_expr
    "if branches extend to the right"
    "if x then 1 else 2 + 3"
    (eif x (econsti 1) (eadd (econsti 2) (econsti 3)));
  check_expr
    "if with a comparison condition"
    "if a < b then f x else g y"
    (eif (elt (Ident "a") (Ident "b")) (eapp (Ident "f") x) (eapp (Ident "g") y));
  check_expr
    "nested if"
    "if x then if y then 1 else 2 else 3"
    (eif x (eif y (econsti 1) (econsti 2)) (econsti 3));
  check_expr "fun" "fun x -> x" (elambda "x" x);
  check_expr
    "fun with two parameters"
    "fun x y -> x + y"
    (elambda "x" (elambda "y" (eadd x y)));
  check_expr
    "fun with an explicit fun"
    "fun x -> fun y -> x"
    (elambda "x" (elambda "y" x));
  check_expr
    "fun body extends to the right"
    "fun x -> x + 1"
    (elambda "x" (eadd x (econsti 1)))
;;

(* --- let --- *)

let () =
  let x = Ident "x"
  and f = Ident "f" in
  check_expr "let" "let x = 1 in x" (elet "x" (econsti 1) x);
  check_expr
    "let with fun"
    "let f = fun x -> x in f 2"
    (elet "f" (elambda "x" x) (eapp f (econsti 2)));
  check_expr
    "let with parameter sugar"
    "let f x = x + 1 in f 2"
    (elet "f" (elambda "x" (eadd x (econsti 1))) (eapp f (econsti 2)));
  check_expr "let rec" "let rec f x = f x in f" (eletrec "f" (elambda "x" (eapp f x)) f);
  check_expr
    "nested let"
    "let x = 1 in let y = 2 in x + y"
    (elet "x" (econsti 1) (elet "y" (econsti 2) (eadd x (Ident "y"))))
;;

(* --- Comments and whitespace --- *)

let () =
  check_expr "comments" "(* c *) 1 (* d *)" (econsti 1);
  check_expr "newlines" "1\n+\n2" (eadd (econsti 1) (econsti 2));
  check_expr "nested comments" "(* a (* b *) c *) 1" (econsti 1);
  check_expr "extra spaces and parens" "  (  1  +  2  )  " (eadd (econsti 1) (econsti 2))
;;

(* --- Names and keywords ---

   A keyword is only a keyword when it is a whole token: the lexer takes the
   longest match, so a name may start with one. *)

let () =
  check_expr "a name may start with let" "letter" (Ident "letter");
  check_expr "the same name in parens" "(letter)" (Ident "letter");
  check_expr "letters" "letters" (Ident "letters");
  check_expr "letx is a name" "letx" (Ident "letx");
  check_expr "letinx is one name" "letinx" (Ident "letinx");
  check_expr "ifn is a name" "ifn" (Ident "ifn");
  check_expr "two names separated by a space" "f x" (eapp (Ident "f") (Ident "x"));
  (* Nothing glues: what is not a boundary for the lexer is application or an
     error for the parser. *)
  check_expr "letx=1inx" "letx=1inx" (eeq (Ident "letx") (eapp (econsti 1) (Ident "inx")));
  check_expr
    "ifn<1then0else1"
    "ifn<1then0else1"
    (elt (Ident "ifn") (eapp (econsti 1) (Ident "then0else1")));
  check_error "no boundary after ->" "funx->x";
  check_error "no boundary after in" "let x = 1 inx"
;;

(* --- CPS factorial and fibonacci --- *)

let () =
  check_expr
    "CPS factorial"
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
       (eapp (eapp (Ident "fact") (econsti 5)) (elambda "x" (Ident "x"))));
  check_expr
    "CPS fibonacci"
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

let () =
  let a = Ident "a"
  and b = Ident "b"
  and c = Ident "c"
  and x = Ident "x" in
  (* Associativity is data: the same source reads differently. *)
  check_expr
    ~ops:(table "left 6 +")
    "left +"
    "1 + 2 + 3"
    (eadd (eadd (econsti 1) (econsti 2)) (econsti 3));
  check_expr
    ~ops:(table "right 6 +")
    "right +"
    "1 + 2 + 3"
    (eadd (econsti 1) (eadd (econsti 2) (econsti 3)));
  (* So is precedence: here [+] is the tighter of the two. *)
  check_expr
    ~ops:(table "left 9 +; left 7 *")
    "+ tighter than *"
    "1 + 2 * 3"
    (emul (eadd (econsti 1) (econsti 2)) (econsti 3));
  (* The precedence of a prefix operator decides how much it takes. *)
  check_expr
    ~ops:(table "prefix 9 -; left 6 +")
    "a tight prefix -"
    "- a + b"
    (eadd (euminus a) b);
  check_expr
    ~ops:(table "prefix 3 -; left 6 +")
    "a loose prefix -"
    "- a + b"
    (euminus (eadd a b));
  (* Arity is data as well: without a prefix [-] there is no unary minus. *)
  check_error ~ops:(table "left 6 -") "no prefix minus" "- a";
  check_error ~ops:(table "prefix 9 -") "no infix minus" "a - b";
  (* nonassoc forbids chaining an operator with itself, and with an operator of
     the same precedence. *)
  check_expr ~ops:(table "left 4 =") "= chained as left" "a = b = c" (eeq (eeq a b) c);
  check_error ~ops:(table "nonassoc 4 =") "= chained as nonassoc" "a = b = c";
  check_error_at ~ops:(table "nonassoc 4 =") "position of a chained =" "a = b = c" 6;
  check_error ~ops:(table "nonassoc 4 = <") "= and < chained" "a = b < c";
  (* A table without an operator is a table without that operator. *)
  check_error ~ops:(table "left 6 +") "no multiplication" "a * b";
  (* The table only describes operators: [=] of a binding is still a token. *)
  check_expr
    ~ops:(table "right 6 +")
    "the table does not touch let"
    "let x = 1 in x + x"
    (elet "x" (econsti 1) (eadd x x))
;;

(* --- The syntax of a table, as given to [--ops] --- *)

let () =
  let check_table name src expected =
    incr total;
    match Operators.of_string src with
    | Ok t when String.equal (Operators.string_of t) expected -> ()
    | Ok t -> report_fail name ~expected ~actual:(Operators.string_of t)
    | Error msg -> report_fail name ~expected ~actual:("error: " ^ msg)
  in
  let check_bad_table name src =
    incr total;
    match Operators.of_string src with
    | Error _ -> ()
    | Ok t -> report_fail name ~expected:"a table error" ~actual:(Operators.string_of t)
  in
  check_table "an empty table" "" "";
  check_table "one group" "left 6 + -" "left 6 + -";
  check_table
    "the default table"
    "left 4 = < >; left 6 + -; left 7 * /; prefix 9 -"
    (Operators.string_of Operators.default);
  check_table "spaces and tabs" "  left \t 6\t+  " "left 6 +";
  check_table "an empty group" "left 6 +;" "left 6 +";
  check_table "one spelling, two fixities" "prefix 9 -; left 6 -" "prefix 9 -; left 6 -";
  check_bad_table "unknown fixity" "fat 6 +";
  check_bad_table "no precedence" "left";
  check_bad_table "no operators" "left 6";
  check_bad_table "precedence is not a number" "left + 6";
  check_bad_table "unknown operator" "left 6 ^";
  check_bad_table "the same spelling twice" "left 6 +; right 6 +";
  check_bad_table "the same spelling twice as a prefix" "prefix 9 -; prefix 8 -";
  (* printing a table and reading it back gives the same table *)
  List.iter
    (fun src ->
       incr total;
       match Operators.of_string src with
       | Error msg ->
         report_fail ("pp round-trip of " ^ src) ~expected:"a table" ~actual:msg
       | Ok t ->
         let expected = Operators.string_of t in
         (match Operators.of_string expected with
          | Ok t' when t' = t -> ()
          | Ok t' ->
            report_fail
              ("pp round-trip of " ^ src)
              ~expected
              ~actual:(Operators.string_of t')
          | Error msg -> report_fail ("pp round-trip of " ^ src) ~expected ~actual:msg))
    [ ""
    ; "left 6 +"
    ; "prefix 9 -"
    ; "left 4 = < >; left 6 + -; left 7 * /; prefix 9 -"
    ; "nonassoc 4 ="
    ; "prefix 9 -; left 6 -"
    ];
  (* a spelling can be looked up as an infix and as a prefix operator *)
  incr total;
  let t = table "prefix 9 -; left 6 -" in
  (match Operators.find_infix t MINUS, Operators.find_prefix t MINUS with
   | Some i, Some p when i.Operators.prec = 6 && p.Operators.prec = 9 -> ()
   | _ ->
     report_fail
       "find_infix/find_prefix on -"
       ~expected:"precedences 6 and 9"
       ~actual:"something else");
  incr total;
  match
    ( Operators.find_infix (table "prefix 9 -") PLUS
    , Operators.find_prefix (table "left 6 +") PLUS )
  with
  | None, None -> ()
  | _ -> report_fail "lookups of an absent operator" ~expected:"None" ~actual:"Some"
;;

(* --- Errors ---

   [Frontend.Lexer] is left as it was: on a character it does not know, and on a
   comment that is not closed, it prints a message and exits the process.  Such
   inputs cannot be tested at all -- the test binary would die instead of
   reporting a failure -- so they are absent below.  Nor does the lexer call
   [Lexing.new_line] yet, so the line of a position is always 1 and a position
   is read as a character offset. *)

let () =
  check_error "empty input" "";
  check_error "whitespace only" "   ";
  check_error "let without in" "let x = 1";
  check_error "if without else" "if x then 1";
  check_error "unclosed paren" "(1";
  check_error "extra paren" "1)";
  check_error "unit is not supported" "()";
  check_error "empty parens" "( )";
  check_error "let without a name" "let = 1 in x";
  check_error "keyword as a name" "let rec = 1 in x";
  check_error "dangling plus" "1 +";
  check_error "keyword as an expression" "in";
  (* the one lexing failure that is a value and not an exit *)
  check_error "literal overflow" "99999999999999999999";
  check_error_at "position of a missing in" "let x = 1" 9;
  check_error_at "position of the end of the input" "1 +" 3;
  check_error_at_pos "position of a missing in" "let x = 1" ~line:1 ~col:10;
  check_error_at_pos "position of a dangling operator" "1 +" ~line:1 ~col:4;
  List.iter
    (fun src -> check_does_not_raise ("does not raise on " ^ String.escaped src) src)
    [ "(((((("
    ; "letx=1inx"
    ; "let rec"
    ; "-->"
    ; String.make 100 '('
    ; String.make 100 '-'
    ; String.make 1000 'x'
    ]
;;

if !failures = 0
then Printf.printf "unit: %d checks passed\n%!" !total
else (
  Printf.printf "unit: %d of %d checks failed\n%!" !failures !total;
  exit 1)
