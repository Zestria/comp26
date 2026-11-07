[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend
open Ast

module Make (Parser : sig
    val parse_expression : string -> (Ast.expression, string) result
    val parse_program : string -> (Ast.program, string) result
  end) =
struct
  let print_expression source =
    match Parser.parse_expression source with
    | Ok result -> Format.printf "%a@." Ast_print.expression result
    | Error error -> Printf.printf "Error: %s\n" error
  ;;

  let print_program source =
    match Parser.parse_program source with
    | Ok items -> Format.printf "%a@." Ast_print.program items
    | Error error -> Printf.printf "Error: %s\n" error
  ;;

  let reject parse source =
    match parse source with
    | Error _ -> print_endline "Rejected"
    | Ok _ -> Printf.printf "Unexpectedly accepted: %S\n" source
  ;;

  module Expressions = struct
    let%expect_test "integer spelling" =
      print_expression "001__2_";
      [%expect {| Int "001__2_" |}]
    ;;

    let%expect_test "negative literal" =
      print_expression "-42";
      [%expect {| Int "-42" |}]
    ;;

    let%expect_test "double negation" =
      print_expression "-(-42)";
      [%expect {| Int "42" |}]
    ;;

    let%expect_test "variable" =
      print_expression "class";
      [%expect {| Var "class" |}]
    ;;

    let%expect_test "boolean" =
      print_expression "true";
      [%expect {| true |}]
    ;;

    let%expect_test "unit with comments" =
      print_expression "( (* unit *) )";
      [%expect {| () |}]
    ;;

    let%expect_test "nested comments" =
      print_expression "(* a (* nested *) b *) 1 (* last *)";
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "quoted delimiter inside comment" =
      print_expression "(* \"*)\" *) 1";
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "keyword boundary" =
      print_expression "ifx";
      [%expect {| Var "ifx" |}]
    ;;

    let%expect_test "operator value" =
      print_expression "( mod )";
      [%expect {| Var "mod" |}]
    ;;

    let%expect_test "custom operator value" =
      print_expression "( +++--+++ )";
      [%expect {| Var "+++--+++" |}]
    ;;

    let%expect_test "multi argument application" =
      print_expression "f x y";
      [%expect {| Apply (Var "f", [Var "x"; Var "y"]) |}]
    ;;

    let%expect_test "parenthesized partial application" =
      print_expression "(f x) y";
      [%expect {| Apply (Apply (Var "f", [Var "x"]), [Var "y"]) |}]
    ;;

    let%expect_test "nested application" =
      print_expression "f (g x)";
      [%expect {| Apply (Var "f", [Apply (Var "g", [Var "x"])]) |}]
    ;;

    let%expect_test "anonymous function application" =
      print_expression "(fun x -> x) 1";
      [%expect {| Apply (Fun ([PVar "x"], Var "x"), [Int "1"]) |}]
    ;;

    let%expect_test "unary minus and power" =
      print_expression "-a ** b";
      [%expect {| Apply (Var "**", [Apply (Var "~-", [Var "a"]); Var "b"]) |}]
    ;;

    let%expect_test "minus consumes application" =
      print_expression "-f x";
      [%expect {| Apply (Var "~-", [Apply (Var "f", [Var "x"])]) |}]
    ;;

    let%expect_test "minus after function is infix" =
      print_expression "f -1";
      [%expect {| Apply (Var "-", [Var "f"; Int "1"]) |}]
    ;;

    let%expect_test "prefix before application" =
      print_expression "!f x";
      [%expect {| Apply (Apply (Var "!", [Var "f"]), [Var "x"]) |}]
    ;;

    let%expect_test "prefix argument" =
      print_expression "f !x";
      [%expect {| Apply (Var "f", [Apply (Var "!", [Var "x"])]) |}]
    ;;

    let%expect_test "hash before application" =
      print_expression "f ## x y";
      [%expect {| Apply (Apply (Var "##", [Var "f"; Var "x"]), [Var "y"]) |}]
    ;;

    let%expect_test "not is a function" =
      print_expression "not x = false";
      [%expect {| Apply (Var "=", [Apply (Var "not", [Var "x"]); false]) |}]
    ;;

    let%expect_test "inequality is infix" =
      print_expression "a != b";
      [%expect {| Apply (Var "!=", [Var "a"; Var "b"]) |}]
    ;;

    let%expect_test "custom operator maximal match" =
      print_expression "a +++--+++ b";
      [%expect {| Apply (Var "+++--+++", [Var "a"; Var "b"]) |}]
    ;;

    let%expect_test "assignment below tuple" =
      print_expression "a := b, c";
      [%expect {| Apply (Var ":=", [Var "a"; Tuple [Var "b"; Var "c"]]) |}]
    ;;

    let%expect_test "assignment adjacent to prefix operator" =
      print_expression "a:=!b";
      [%expect {| Apply (Var ":=", [Var "a"; Apply (Var "!", [Var "b"])]) |}]
    ;;

    let%expect_test "hash belongs to a prefix operator name" =
      print_expression "!#x";
      [%expect {| Apply (Var "!#", [Var "x"]) |}]
    ;;

    let%expect_test "comparison is left associative" =
      print_expression "a < b = c";
      [%expect {| Apply (Var "=", [Apply (Var "<", [Var "a"; Var "b"]); Var "c"]) |}]
    ;;

    let%expect_test "operator name in a parameter pattern" =
      print_expression "fun (+) -> (+)";
      [%expect {| Fun ([PVar "+"], Var "+") |}]
    ;;

    let%expect_test "flat tuple" =
      print_expression "a,b,c";
      [%expect {| Tuple [Var "a"; Var "b"; Var "c"] |}]
    ;;

    let%expect_test "nested tuple stays nested" =
      print_expression "(a,b),c";
      [%expect {| Tuple [Tuple [Var "a"; Var "b"]; Var "c"] |}]
    ;;

    let%expect_test "multiple function parameters" =
      print_expression "fun x y -> x";
      [%expect {| Fun ([PVar "x"; PVar "y"], Var "x") |}]
    ;;

    let%expect_test "function body includes an unparenthesized tuple" =
      print_expression "fun x -> x, y";
      [%expect {| Fun ([PVar "x"], Tuple [Var "x"; Var "y"]) |}]
    ;;

    let%expect_test "function body includes assignment" =
      print_expression "fun x -> x := y";
      [%expect {| Fun ([PVar "x"], Apply (Var ":=", [Var "x"; Var "y"])) |}]
    ;;

    let%expect_test "nested function stays nested" =
      print_expression "fun x -> fun y -> x";
      [%expect {| Fun ([PVar "x"], Fun ([PVar "y"], Var "x")) |}]
    ;;

    let%expect_test "parameter patterns" =
      print_expression "fun (x, (_, ())) -> x";
      [%expect {| Fun ([PTuple [PVar "x"; PTuple [_; ()]]], Var "x") |}]
    ;;

    let%expect_test "local function binding" =
      print_expression "let f x y = x in f";
      [%expect {| Let (PVar "f" = Fun ([PVar "x"; PVar "y"], Var "x"), Var "f") |}]
    ;;

    let%expect_test "local tuple binding" =
      print_expression "let x,y = 1,2 in x";
      [%expect
        {| Let (PTuple [PVar "x"; PVar "y"] = Tuple [Int "1"; Int "2"], Var "x") |}]
    ;;

    let%expect_test "recursive function expression" =
      print_expression "let rec f = fun x -> f x in f";
      [%expect
        {| Let (rec PVar "f" = Fun ([PVar "x"], Apply (Var "f", [Var "x"])), Var "f") |}]
    ;;

    let%expect_test "operator rebinding" =
      print_expression "let ( + ) x y = x in 1 + 2";
      [%expect
        {| Let (PVar "+" = Fun ([PVar "x"; PVar "y"], Var "x"), Apply (Var "+", [Int "1"; Int "2"])) |}]
    ;;

    let%expect_test "if without else" =
      print_expression "if true then ()";
      [%expect {| If (true, (), None) |}]
    ;;

    let%expect_test "nearest else" =
      print_expression "if a then if b then x else y";
      [%expect {| If (Var "a", If (Var "b", Var "x", Some (Var "y")), None) |}]
    ;;

    let%expect_test "tuple in else branch" =
      print_expression "if a then b else c,d";
      [%expect {| If (Var "a", Var "b", Some (Tuple [Var "c"; Var "d"])) |}]
    ;;

    let%expect_test "conditional on infix right" =
      print_expression "1 + if true then 2 else 3";
      [%expect {| Apply (Var "+", [Int "1"; If (true, Int "2", Some (Int "3"))]) |}]
    ;;
  end

  module Deferred_checks = struct
    let%expect_test "type checking is deferred" =
      print_expression "if 1 then 2 else false";
      [%expect {| If (Int "1", Int "2", Some (false)) |}]
    ;;

    let%expect_test "recursive safety is deferred" =
      print_expression "let rec f = f in f";
      [%expect {| Let (rec PVar "f" = Var "f", Var "f") |}]
    ;;

    let%expect_test "duplicate pattern variables reach type checker" =
      print_expression "fun (x,x) -> x";
      [%expect {| Fun ([PTuple [PVar "x"; PVar "x"]], Var "x") |}]
    ;;

    let%expect_test "a non-unit branch without else reaches type checker" =
      print_expression "if true then 1";
      [%expect {| If (true, Int "1", None) |}]
    ;;

    let%expect_test "a recursive tuple pattern reaches semantic checking" =
      print_expression "let rec (x,y) = (1,2) in x";
      [%expect
        {| Let (rec PTuple [PVar "x"; PVar "y"] = Tuple [Int "1"; Int "2"], Var "x") |}]
    ;;

    let%expect_test "integer range checking is deferred past unary minus" =
      print_expression "-999999999999999999999999999999";
      [%expect {| Int "-999999999999999999999999999999" |}]
    ;;

    let%expect_test "division by zero is not evaluated during parsing" =
      print_expression "1 / 0";
      [%expect {| Apply (Var "/", [Int "1"; Int "0"]) |}]
    ;;
  end

  module Constructor_syntax = struct
    let%expect_test "a direct constructor with one argument reaches type checking" =
      print_expression "true x";
      [%expect {| Apply (true, [Var "x"]) |}]
    ;;

    let%expect_test "an unclosed constructor argument is not ignored" =
      reject Parser.parse_expression "true (";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "a direct true constructor cannot take two arguments" =
      reject Parser.parse_expression "true x y";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "a direct false constructor cannot take two arguments" =
      reject Parser.parse_expression "false x y";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "a direct unit constructor cannot take two arguments" =
      reject Parser.parse_expression "() x y";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "a parenthesized boolean allows ordinary application" =
      print_expression "(true) x y";
      [%expect {| Apply (true, [Var "x"; Var "y"]) |}]
    ;;

    let%expect_test "a parenthesized unit allows ordinary application" =
      print_expression "(()) x y";
      [%expect {| Apply ((), [Var "x"; Var "y"]) |}]
    ;;

    let%expect_test "a parenthesized constructor application can be applied again" =
      print_expression "(true x) y";
      [%expect {| Apply (Apply (true, [Var "x"]), [Var "y"]) |}]
    ;;

    let%expect_test "a constructor used as an argument does not consume the next argument"
      =
      print_expression "f true x";
      [%expect {| Apply (Var "f", [true; Var "x"]) |}]
    ;;

    let%expect_test "prefix application to a constructor allows ordinary application" =
      print_expression "!true x y";
      [%expect {| Apply (Apply (Var "!", [true]), [Var "x"; Var "y"]) |}]
    ;;

    let%expect_test "hash application to a constructor allows ordinary application" =
      print_expression "true ## x y";
      [%expect {| Apply (Apply (Var "##", [true; Var "x"]), [Var "y"]) |}]
    ;;

    let%expect_test "a constructor argument consumes the hash expression" =
      print_expression "true x ## y";
      [%expect {| Apply (true, [Apply (Var "##", [Var "x"; Var "y"])]) |}]
    ;;

    let%expect_test "a hash expression remains a single constructor argument" =
      reject Parser.parse_expression "true x ## y z";
      [%expect {| Rejected |}]
    ;;
  end

  module Programs = struct
    let%expect_test "empty" =
      print_program "";
      [%expect {| [] |}]
    ;;

    let%expect_test "only separators" =
      print_program ";;;;";
      [%expect {| [] |}]
    ;;

    let%expect_test "definitions without separators" =
      print_program "let x = 1\nlet y = 2";
      [%expect {| [Definition (PVar "x" = Int "1"); Definition (PVar "y" = Int "2")] |}]
    ;;

    let%expect_test "evaluation then definition" =
      print_program "1;; let x = 2";
      [%expect {| [Evaluation (Int "1"); Definition (PVar "x" = Int "2")] |}]
    ;;

    let%expect_test "evaluation after definition" =
      print_program "let x = 1;; x";
      [%expect {| [Definition (PVar "x" = Int "1"); Evaluation (Var "x")] |}]
    ;;

    let%expect_test "local let as initial evaluation" =
      print_program "let x = 1 in x";
      [%expect {| [Evaluation (Let (PVar "x" = Int "1", Var "x"))] |}]
    ;;

    let%expect_test "repeated separators" =
      print_program ";;let x=1;;;;x;;";
      [%expect {| [Definition (PVar "x" = Int "1"); Evaluation (Var "x")] |}]
    ;;

    let%expect_test "evaluation before definition needs no separator" =
      print_program "x let y = 2";
      [%expect {| [Evaluation (Var "x"); Definition (PVar "y" = Int "2")] |}]
    ;;

    let%expect_test "newline continues a binding expression" =
      print_program "let f = g\n x";
      [%expect {| [Definition (PVar "f" = Apply (Var "g", [Var "x"]))] |}]
    ;;

    let%expect_test "local let after separator" =
      print_program "let x = 1;; let y = 2 in y";
      [%expect
        {| [Definition (PVar "x" = Int "1"); Evaluation (Let (PVar "y" = Int "2", Var "y"))] |}]
    ;;
  end

  module Rejections = struct
    let%expect_test "expression: " =
      reject Parser.parse_expression "";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: let x = 1" =
      reject Parser.parse_expression "let x = 1";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: fun -> 1" =
      reject Parser.parse_expression "fun -> 1";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: fun x,y -> x" =
      reject Parser.parse_expression "fun x,y -> x";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: if true then" =
      reject Parser.parse_expression "if true then";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: if true else 1" =
      reject Parser.parse_expression "if true else 1";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: (1,)" =
      reject Parser.parse_expression "(1,)";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: (1,2" =
      reject Parser.parse_expression "(1,2";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: 1 +" =
      reject Parser.parse_expression "1 +";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: 1 0xff" =
      reject Parser.parse_expression "1 0xff";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: 1L" =
      reject Parser.parse_expression "1L";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: 12abc" =
      reject Parser.parse_expression "12abc";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: 1.0" =
      reject Parser.parse_expression "1.0";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: X" =
      reject Parser.parse_expression "X";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: _" =
      reject Parser.parse_expression "_";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: let let = 1 in let" =
      reject Parser.parse_expression "let let = 1 in let";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: let (x,y) z = z in x" =
      reject Parser.parse_expression "let (x,y) z = z in x";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: fun ~x -> x" =
      reject Parser.parse_expression "fun ~x -> x";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: 1;2" =
      reject Parser.parse_expression "1;2";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: (* unclosed" =
      reject Parser.parse_expression "(* unclosed";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: (* outer (* inner *)" =
      reject Parser.parse_expression "(* outer (* inner *)";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: (* \"unclosed *)" =
      reject Parser.parse_expression "(* \"unclosed *)";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: fun x --> x" =
      reject Parser.parse_expression "fun x --> x";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: f (" =
      reject Parser.parse_expression "f (";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "expression: a -> b" =
      reject Parser.parse_expression "a -> b";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "program: let x=1 let y=2 in y" =
      reject Parser.parse_program "let x=1 let y=2 in y";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "program: let x=1;;let" =
      reject Parser.parse_program "let x=1;;let";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "program: an error after a complete local let evaluation" =
      reject Parser.parse_program "let x=1 in x;;@";
      [%expect {| Rejected |}]
    ;;

    let%expect_test "program: ;;let x=1 in" =
      reject Parser.parse_program ";;let x=1 in";
      [%expect {| Rejected |}]
    ;;
  end

  module Comments = struct
    let%expect_test "apostrophe in an identifier before a quote character" =
      print_expression {|(* f' '"' *) 1|};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "empty character spelling before a quote character" =
      print_expression {|(* '''"' *) 1|};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "decimal character escape followed by a quote character" =
      print_expression {|(* '\034''"' *) 1|};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "octal character escape followed by a quote character" =
      print_expression {|(* '\o042''"' *) 1|};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "quoted extension shields a double quote" =
      print_expression {source|(* {%foo|"|} *) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "quoted structure extension shields a comment delimiter" =
      print_expression {source|(* {%%Foo.bar tag|*)|tag} *) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "uppercase text does not open a quoted string" =
      print_expression {source|(* {Tag|*) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "digits cannot form a quoted string delimiter" =
      print_expression {source|(* {x1|*) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "unterminated quoted extension" =
      reject Parser.parse_expression {source|(* {%foo|*)|source};
      [%expect {| Rejected |}]
    ;;

    let%expect_test "quote character inside comment" =
      print_expression {|(* '"' *) 1|};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "escaped quote character inside comment" =
      print_expression {|(* '\"' *) 1|};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "quoted string delimiter inside comment" =
      print_expression {source|(* {tag| " *) (* |tag} *) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "unterminated quoted string inside comment" =
      reject Parser.parse_expression {source|(* {tag| *)|source};
      [%expect {| Rejected |}]
    ;;

    let%expect_test "a different closing delimiter remains quoted" =
      print_expression {source|(* {tag||other}*)|tag} *) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "a closing delimiter must match the whole name" =
      print_expression {source|(* {tag||tagx}*)|tag} *) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "a closing delimiter can immediately follow an incomplete one" =
      print_expression {source|(* {tag||ta|tag} *) 1|source};
      [%expect {| Int "1" |}]
    ;;

    let%expect_test "an empty closing delimiter can follow a literal pipe" =
      print_expression {source|(* {|||} *) 1|source};
      [%expect {| Int "1" |}]
    ;;
  end

  module Cps = struct
    let ( let* ) = Result.bind
    let name text = Result.map_error Name.error_to_string (Name.of_identifier text)

    let integer text =
      let* literal =
        Result.map_error Integer_literal.error_to_string (Integer_literal.of_string text)
      in
      Ok (Constant (Int literal))
    ;;

    let call f argument rest = Apply (f, argument, rest)

    let operator text =
      let* name = Result.map_error Name.error_to_string (Name.of_operator text) in
      Ok (fun a b -> call (Var name) a [ b ])
    ;;

    let lambda parameter body = Fun { parameter = PVar parameter; parameters = []; body }

    let definition function_name parameter continuation body =
      Definition
        { recursion = Recursive
        ; pattern = PVar function_name
        ; expression =
            Fun { parameter = PVar parameter; parameters = [ PVar continuation ]; body }
        }
    ;;

    let check source = function
      | Error error -> Printf.printf "Invalid expected AST for %S: %s\n" source error
      | Ok expected ->
        (match Parser.parse_program source with
         | Ok actual when actual = expected -> print_endline "Matches expected AST"
         | Ok actual ->
           Format.printf
             "Input: %S@.Expected: %a@.Actual: %a@."
             source
             Ast_print.program
             expected
             Ast_print.program
             actual
         | Error error -> Printf.printf "Unexpected parse error for %S: %s\n" source error)
    ;;

    let%expect_test "factorial" =
      let expected =
        let* fact = name "fact" in
        let* n = name "n" in
        let* k = name "k" in
        let* r = name "r" in
        let* zero = integer "0" in
        let* one = integer "1" in
        let* equal = operator "=" in
        let* subtract = operator "-" in
        let* multiply = operator "*" in
        Ok
          [ definition
              fact
              n
              k
              (If
                 ( equal (Var n) zero
                 , call (Var k) one []
                 , Some
                     (call
                        (Var fact)
                        (subtract (Var n) one)
                        [ lambda r (call (Var k) (multiply (Var n) (Var r)) []) ]) ))
          ]
      in
      check
        {|let rec fact n k =
          if n = 0 then k 1
          else fact (n - 1) (fun r -> k (n * r))|}
        expected;
      [%expect {| Matches expected AST |}]
    ;;

    let%expect_test "fibonacci" =
      let expected =
        let* fib = name "fib" in
        let* n = name "n" in
        let* k = name "k" in
        let* a = name "a" in
        let* b = name "b" in
        let* one = integer "1" in
        let* two = integer "2" in
        let* less_equal = operator "<=" in
        let* subtract = operator "-" in
        let* add = operator "+" in
        Ok
          [ definition
              fib
              n
              k
              (If
                 ( less_equal (Var n) one
                 , call (Var k) (Var n) []
                 , Some
                     (call
                        (Var fib)
                        (subtract (Var n) one)
                        [ lambda
                            a
                            (call
                               (Var fib)
                               (subtract (Var n) two)
                               [ lambda b (call (Var k) (add (Var a) (Var b)) []) ])
                        ]) ))
          ]
      in
      check
        {|let rec fib n k =
          if n <= 1 then k n
          else fib (n - 1) (fun a ->
            fib (n - 2) (fun b ->
              k (a + b)))|}
        expected;
      [%expect {| Matches expected AST |}]
    ;;
  end
end

let%test_module "Angstrom" = (module Make (Bkml_frontend.Parser))
let%test_module "Menhir" = (module Make (Bkml_frontend.Menhir_parser))

module Diagnostics = struct
  let print_error source =
    match Parser.parse_program source with
    | Error message -> print_endline message
    | Ok program ->
      Format.printf "Unexpectedly accepted %S: %a@." source Ast_print.program program
  ;;

  let%expect_test "missing then branch after another definition" =
    print_error "let x = 1 let y = if x then";
    [%expect {| binding expression > expression after then: not enough input |}]
  ;;

  let%expect_test "missing else branch keeps the conditional context" =
    print_error "if x then y else";
    [%expect {| expression after else: not enough input |}]
  ;;

  let%expect_test "missing local body does not become a top-level definition" =
    print_error "let x = 1 in";
    [%expect {| expression after in: not enough input |}]
  ;;

  let%expect_test "missing function body keeps the function context" =
    print_error "fun x ->";
    [%expect {| function body: not enough input |}]
  ;;

  let%expect_test "missing binding expression keeps the binding context" =
    print_error "let x =";
    [%expect {| binding expression: not enough input |}]
  ;;

  let%expect_test "missing condition keeps the conditional context" =
    print_error "if";
    [%expect {| if condition: not enough input |}]
  ;;
end

module Lexical_rules = struct
  open Grammar

  let named kind name = Printf.sprintf "%s %S" kind (Name.to_string name)

  let token = function
    | IDENT name -> named "IDENT" name
    | PREFIX name -> named "PREFIX" name
    | HASHOP name -> named "HASHOP" name
    | INFIX0 name -> named "INFIX0" name
    | INFIX1 name -> named "INFIX1" name
    | INFIX2 name -> named "INFIX2" name
    | INFIX3 name -> named "INFIX3" name
    | INFIX4 name -> named "INFIX4" name
    | MINUS name -> named "MINUS" name
    | EQUAL name -> named "EQUAL" name
    | AND name -> named "AND" name
    | OR name -> named "OR" name
    | ASSIGN name -> named "ASSIGN" name
    | INT literal -> Printf.sprintf "INT %S" (Integer_literal.to_string literal)
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | LET -> "LET"
    | REC -> "REC"
    | IN -> "IN"
    | FUN -> "FUN"
    | ARROW -> "ARROW"
    | IF -> "IF"
    | THEN -> "THEN"
    | ELSE -> "ELSE"
    | UNDERSCORE -> "UNDERSCORE"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | COMMA -> "COMMA"
    | SEMISEMI -> "SEMISEMI"
    | EOF -> "EOF"
  ;;

  let print_tokens source =
    let lexbuf = Lexing.from_string source in
    let rec read accumulated =
      match Lexer.token lexbuf with
      | EOF -> String.concat "; " (List.rev ("EOF" :: accumulated))
      | next -> read (token next :: accumulated)
    in
    try print_endline (read []) with
    | Lexer.Error message -> Printf.printf "Lexical error: %s\n" message
  ;;

  let%expect_test "minus is separate from digits" =
    print_tokens "f-1";
    [%expect {| IDENT "f"; MINUS "-"; INT "1"; EOF |}]
  ;;

  let%expect_test "symbolic operators use maximal match" =
    print_tokens "--2";
    [%expect {| INFIX2 "--"; INT "2"; EOF |}]
  ;;

  let%expect_test "parenthesized operator is a sequence of tokens" =
    print_tokens "( + )";
    [%expect {| LPAREN; INFIX2 "+"; RPAREN; EOF |}]
  ;;

  let%expect_test "malformed integer is rejected by the lexer" =
    print_tokens "12abc";
    [%expect {| Lexical error: Invalid integer literal: "12abc" |}]
  ;;
end
