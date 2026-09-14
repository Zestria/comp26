[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Unit tests for the top-down parser: source -> AST table. *)

open Frontend.Common
module P = Top_down.Parser

let failures = ref 0
let total = ref 0

let report_fail name ~expected ~actual =
  incr failures;
  Printf.printf "FAIL: %s\n  expected: %s\n  actual:   %s\n%!" name expected actual
;;

let check_expr name ?glued src expected =
  incr total;
  match P.parse_expr ?glued src with
  | Ok e when e = expected -> ()
  | Ok e ->
    report_fail name ~expected:(string_of_expr expected) ~actual:(string_of_expr e)
  | Error err ->
    report_fail
      name
      ~expected:(string_of_expr expected)
      ~actual:("error: " ^ P.string_of_error err)
;;

let check_error name ?glued src =
  incr total;
  match P.parse_expr ?glued src with
  | Error _ -> ()
  | Ok e -> report_fail name ~expected:"parse error" ~actual:(string_of_expr e)
;;

let check_error_at name src expected_offset =
  incr total;
  match P.parse_expr src with
  | Error e when e.pos.pos_cnum = expected_offset -> ()
  | Error e ->
    report_fail
      name
      ~expected:(string_of_int expected_offset)
      ~actual:(string_of_int e.pos.pos_cnum)
  | Ok e -> report_fail name ~expected:"parse error" ~actual:(string_of_expr e)
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
  check_expr "comparison <=" "a <= b" (elte a b);
  check_expr "comparison >" "a > b" (egt a b);
  check_expr "comparison >=" "a >= b" (egte a b);
  check_expr "comparison =" "a = b" (eeq a b);
  check_expr "comparison is below +" "a < b + c" (elt a (eadd b c));
  check_expr "comparisons are left-associative" "a = b = c" (eeq (eeq a b) c)
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

(* --- Glued keywords --- *)

let () =
  check_expr ~glued:true "glued: let/in" "letx=1inx" (elet "x" (econsti 1) (Ident "x"));
  check_expr ~glued:true "glued: fun" "funx->x" (elambda "x" (Ident "x"));
  check_expr
    ~glued:true
    "glued: if/then/else"
    "ifn<1then0else1"
    (eif (elt (Ident "n") (econsti 1)) (econsti 0) (econsti 1));
  check_expr
    ~glued:true
    "glued: rec, application in parens"
    "letrecf=funx->ifx=0then1elsef(x-1)inf(5)"
    (eletrec
       "f"
       (elambda
          "x"
          (eif
             (eeq (Ident "x") (econsti 0))
             (econsti 1)
             (eapp (Ident "f") (esub (Ident "x") (econsti 1)))))
       (eapp (Ident "f") (econsti 5)));
  (* A name starting with a keyword stays a name while
     the keyword branch does not parse. *)
  check_expr ~glued:true "letter as a name" "letter" (Ident "letter");
  check_expr ~glued:true "letter in parens" "(letter)" (Ident "letter");
  check_expr ~glued:true "letters as a name" "letters" (Ident "letters");
  (* Documented ambiguity: keyword-first wins, so this reads as
     `let ter = 1 in letter`. *)
  check_expr
    ~glued:true
    "letter= ambiguity"
    "letter=1inletter"
    (elet "ter" (econsti 1) (Ident "letter"));
  (* Without gluing the same text reads differently, which is
     exactly what the flag does: the tree either differs or the text
     does not parse at all. *)
  check_error "gluing off: fun" "funx->x";
  check_error "gluing off: no boundary after in" "let x = 1 inx";
  check_expr
    "gluing off: letx is a name"
    "letx=1inx"
    (eeq (Ident "letx") (eapp (econsti 1) (Ident "inx")));
  check_expr
    "gluing off: ifn is a name"
    "ifn<1then0else1"
    (elt (Ident "ifn") (eapp (econsti 1) (Ident "then0else1")));
  check_error ~glued:true "name gluing: let x=1 without in" "let x=1";
  (* Two names separated by a space are still two tokens:
     only keyword+identifier glues, not everything. *)
  check_expr
    ~glued:true
    "two names separated by a space"
    "f x"
    (eapp (Ident "f") (Ident "x"))
;;

(* --- Errors --- *)

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
  check_error "unclosed comment" "(* unterminated";
  check_error "invalid character" "1 % 2";
  check_error "keyword as an expression" "in";
  check_error "literal overflow" "99999999999999999999";
  check_error_at "position of an invalid character" "1 % 2" 2;
  check_error_at "position of a missing in" "let x = 1" 9;
  List.iter
    (fun src -> check_does_not_raise ("does not raise on " ^ String.escaped src) src)
    [ "(((((("; "%$#@!"; "(*"; "letx=1inx"; "let rec"; "-->"; "\""; "'''" ]
;;

if !failures = 0
then Printf.printf "unit: %d checks passed\n%!" !total
else (
  Printf.printf "unit: %d of %d checks failed\n%!" !failures !total;
  exit 1)
