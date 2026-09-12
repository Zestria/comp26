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
  Printf.printf "FAIL: %s\n  ожидалось: %s\n  получено:  %s\n%!" name expected actual
;;

let check_expr name ?glued src expected =
  incr total;
  match P.parse_expr ?glued src with
  | Ok e when e = expected -> ()
  | Ok e -> report_fail name ~expected:(string_of_expr expected) ~actual:(string_of_expr e)
  | Error err ->
    report_fail name ~expected:(string_of_expr expected) ~actual:("ошибка: " ^ P.string_of_error err)
;;

let check_error name ?glued src =
  incr total;
  match P.parse_expr ?glued src with
  | Error _ -> ()
  | Ok e -> report_fail name ~expected:"ошибка разбора" ~actual:(string_of_expr e)
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
  | Ok e -> report_fail name ~expected:"ошибка разбора" ~actual:(string_of_expr e)
;;

let check_does_not_raise name src =
  incr total;
  let raised = ref None in
  (try ignore (P.parse_expr src) with
   | exn -> raised := Some (Printexc.to_string exn));
  match !raised with
  | None -> ()
  | Some s -> report_fail name ~expected:"Ok/Error без исключений" ~actual:s
;;

(* --- Literals and identifiers --- *)

let () =
  check_expr "числовой литерал" "42" (econsti 42);
  check_expr "идентификатор" "x" (Ident "x");
  check_expr "идентификатор с цифрами и штрихом" "x1'_2" (Ident "x1'_2");
  check_expr "верхний регистр" "Foo" (Ident "Foo")
;;

(* --- Application --- *)

let () =
  let x = Ident "x" and y = Ident "y" and z = Ident "z" and f = Ident "f" in
  let g = Ident "g" in
  check_expr "аппликация левоассоциативна" "x y z" (eapp (eapp x y) z);
  check_expr "аппликация с скобками" "f (g x)" (eapp f (eapp g x));
  check_expr "аппликация нескольких атомов" "f 1 2" (eapp (eapp f (econsti 1)) (econsti 2));
  check_expr "аппликация связывает крепче плюса" "f x + y" (eadd (eapp f x) y)
;;

(* --- Operators --- *)

let () =
  let a = Ident "a" and b = Ident "b" and c = Ident "c" and x = Ident "x" in
  check_expr "* крепче +" "1 + 2 * 3" (eadd (econsti 1) (emul (econsti 2) (econsti 3)));
  check_expr "* крепче + (2)" "1 * 2 + 3" (eadd (emul (econsti 1) (econsti 2)) (econsti 3));
  check_expr "- левоассоциативен" "1 - 2 - 3" (esub (esub (econsti 1) (econsti 2)) (econsti 3));
  check_expr "+ левоассоциативен" "1 + 2 + 3" (eadd (eadd (econsti 1) (econsti 2)) (econsti 3));
  check_expr "скобки меняют порядок" "(1 + 2) * 3" (emul (eadd (econsti 1) (econsti 2)) (econsti 3));
  check_expr "деление" "1 / 2" (ediv (econsti 1) (econsti 2));
  check_expr "унарный минус" "- x" (euminus x);
  check_expr "унарный минус ниже аппликации" "- f x" (euminus (eapp (Ident "f") x));
  check_expr "бинарный и унарный минус" "1 - -2" (esub (econsti 1) (euminus (econsti 2)));
  check_expr "бинарный минус" "1 - 2" (esub (econsti 1) (econsti 2));
  check_expr "сравнение <" "a < b" (elt a b);
  check_expr "сравнение <=" "a <= b" (elte a b);
  check_expr "сравнение >" "a > b" (egt a b);
  check_expr "сравнение >=" "a >= b" (egte a b);
  check_expr "сравнение =" "a = b" (eeq a b);
  check_expr "сравнение ниже +" "a < b + c" (elt a (eadd b c));
  check_expr "сравнения левоассоциативны" "a = b = c" (eeq (eeq a b) c)
;;

(* --- if / fun --- *)

let () =
  let x = Ident "x" and y = Ident "y" in
  check_expr "if" "if x then 1 else 2" (eif x (econsti 1) (econsti 2));
  check_expr "ветви if тянутся вправо" "if x then 1 else 2 + 3" (eif x (econsti 1) (eadd (econsti 2) (econsti 3)));
  check_expr
    "if с условием-сравнением"
    "if a < b then f x else g y"
    (eif (elt (Ident "a") (Ident "b")) (eapp (Ident "f") x) (eapp (Ident "g") y));
  check_expr "вложенный if" "if x then if y then 1 else 2 else 3"
    (eif x (eif y (econsti 1) (econsti 2)) (econsti 3));
  check_expr "fun" "fun x -> x" (elambda "x" x);
  check_expr "fun с двумя параметрами" "fun x y -> x + y" (elambda "x" (elambda "y" (eadd x y)));
  check_expr "fun с явным fun" "fun x -> fun y -> x" (elambda "x" (elambda "y" x));
  check_expr "тело fun тянется вправо" "fun x -> x + 1" (elambda "x" (eadd x (econsti 1)))
;;

(* --- let --- *)

let () =
  let x = Ident "x" and f = Ident "f" in
  check_expr "let" "let x = 1 in x" (elet "x" (econsti 1) x);
  check_expr "let с fun" "let f = fun x -> x in f 2" (elet "f" (elambda "x" x) (eapp f (econsti 2)));
  check_expr "let с сахаром параметров" "let f x = x + 1 in f 2"
    (elet "f" (elambda "x" (eadd x (econsti 1))) (eapp f (econsti 2)));
  check_expr "let rec" "let rec f x = f x in f"
    (eletrec "f" (elambda "x" (eapp f x)) f);
  check_expr "вложенные let"
    "let x = 1 in let y = 2 in x + y"
    (elet "x" (econsti 1) (elet "y" (econsti 2) (eadd x (Ident "y"))))
;;

(* --- Comments and whitespace --- *)

let () =
  check_expr "комментарии" "(* c *) 1 (* d *)" (econsti 1);
  check_expr "переводы строк" "1\n+\n2" (eadd (econsti 1) (econsti 2));
  check_expr "вложенные комментарии" "(* a (* b *) c *) 1" (econsti 1);
  check_expr "лишние пробелы и скобки" "  (  1  +  2  )  " (eadd (econsti 1) (econsti 2))
;;

(* --- CPS factorial and fibonacci --- *)

let () =
  check_expr
    "CPS факториал"
    "let rec fact n k = if n = 0 then k 1 else fact (n - 1) (fun r -> k (n * r)) in \
     fact 5 (fun x -> x)"
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
    "CPS фибоначчи"
    "let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a \
     + b))) in fib 10 (fun x -> x)"
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
                         (elambda
                            "b"
                            (eapp (Ident "k") (eadd (Ident "a") (Ident "b"))))))))))
       (eapp (eapp (Ident "fib") (econsti 10)) (elambda "x" (Ident "x"))))
;;

(* --- Glued keywords --- *)

let () =
  check_expr ~glued:true "слипание: let/in" "letx=1inx" (elet "x" (econsti 1) (Ident "x"));
  check_expr ~glued:true "слипание: fun" "funx->x" (elambda "x" (Ident "x"));
  check_expr ~glued:true "слипание: if/then/else" "ifn<1then0else1"
    (eif (elt (Ident "n") (econsti 1)) (econsti 0) (econsti 1));
  check_expr ~glued:true "слипание: rec, аппликация в скобках"
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
  check_expr ~glued:true "letter как имя" "letter" (Ident "letter");
  check_expr ~glued:true "letter в скобках" "(letter)" (Ident "letter");
  check_expr ~glued:true "letters как имя" "letters" (Ident "letters");
  (* Documented ambiguity: keyword-first wins, so this reads as
     `let ter = 1 in letter`. *)
  check_expr ~glued:true "неоднозначность letter=" "letter=1inletter"
    (elet "ter" (econsti 1) (Ident "letter"));
  (* Without gluing the same text reads differently, which is
     exactly what the flag does: the tree either differs or the text
     does not parse at all. *)
  check_error "слипание выключено: fun" "funx->x";
  check_error "слипание выключено: нет границы после in" "let x = 1 inx";
  check_expr "слипание выключено: letx — имя" "letx=1inx"
    (eeq (Ident "letx") (eapp (econsti 1) (Ident "inx")));
  check_expr "слипание выключено: ifn — имя" "ifn<1then0else1"
    (elt (Ident "ifn") (eapp (econsti 1) (Ident "then0else1")));
  check_error ~glued:true "слипание имён: let x=1 без in" "let x=1";
  (* Two names separated by a space are still two tokens:
     only keyword+identifier glues, not everything. *)
  check_expr ~glued:true "два имени через пробел" "f x" (eapp (Ident "f") (Ident "x"))
;;

(* --- Errors --- *)

let () =
  check_error "пустой ввод" "";
  check_error "только пробелы" "   ";
  check_error "let без in" "let x = 1";
  check_error "if без else" "if x then 1";
  check_error "незакрытая скобка" "(1";
  check_error "лишняя скобка" "1)";
  check_error "unit не поддержан" "()";
  check_error "пустые скобки" "( )";
  check_error "let без имени" "let = 1 in x";
  check_error "имя-ключевое слово" "let rec = 1 in x";
  check_error "оборванный плюс" "1 +";
  check_error "незакрытый комментарий" "(* unterminated";
  check_error "недопустимый символ" "1 % 2";
  check_error "ключевое слово как выражение" "in";
  check_error "переполнение литерала" "99999999999999999999";
  check_error_at "позиция недопустимого символа" "1 % 2" 2;
  check_error_at "позиция отсутствующего in" "let x = 1" 9;
  List.iter
    (fun src -> check_does_not_raise ("не падает на " ^ String.escaped src) src)
    [ "(((((("; "%$#@!"; "(*"; "letx=1inx"; "let rec"; "-->"; "\""; "'''" ]
;;

if !failures = 0
then Printf.printf "unit: %d проверок пройдено\n%!" !total
else (
  Printf.printf "unit: упало %d из %d проверок\n%!" !failures !total;
  exit 1)
