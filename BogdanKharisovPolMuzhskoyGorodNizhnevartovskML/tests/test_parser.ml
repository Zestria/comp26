(* SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend
open Ast

let list show items = "[" ^ String.concat "; " (List.map show items) ^ "]"

let rec pattern = function
  | PVar name -> Printf.sprintf "PVar %S" (Name.to_string name)
  | PWildcard -> "_"
  | PUnit -> "()"
  | PTuple (a, b, rest) -> "PTuple " ^ list pattern (a :: b :: rest)
;;

let rec expression = function
  | Constant (Int literal) -> Printf.sprintf "Int %S" (Integer_literal.to_string literal)
  | Constant (Bool value) -> string_of_bool value
  | Constant Unit -> "()"
  | Var name -> Printf.sprintf "Var %S" (Name.to_string name)
  | Tuple (a, b, rest) -> "Tuple " ^ list expression (a :: b :: rest)
  | Apply (f, a, rest) ->
    "Apply (" ^ expression f ^ ", " ^ list expression (a :: rest) ^ ")"
  | Fun { parameter; parameters; body } ->
    "Fun (" ^ list pattern (parameter :: parameters) ^ ", " ^ expression body ^ ")"
  | Let (definition, body) -> "Let (" ^ binding definition ^ ", " ^ expression body ^ ")"
  | If (condition, yes, no) ->
    "If ("
    ^ expression condition
    ^ ", "
    ^ expression yes
    ^ ", "
    ^ (match no with
       | None -> "None"
       | Some e -> "Some (" ^ expression e ^ ")")
    ^ ")"

and binding { recursion; pattern = p; expression = e } =
  (match recursion with
   | Nonrecursive -> ""
   | Recursive -> "rec ")
  ^ pattern p
  ^ " = "
  ^ expression e
;;

let print_expression source =
  match Parser.parse_expression source with
  | Ok result -> print_endline (expression result)
  | Error error -> Printf.printf "Error: %s\n" error
;;

let print_program source =
  match Parser.parse_program source with
  | Error error -> Printf.printf "Error: %s\n" error
  | Ok items ->
    print_endline
      (list
         (function
           | Definition b -> "Definition (" ^ binding b ^ ")"
           | Evaluation e -> "Evaluation (" ^ expression e ^ ")")
         items)
;;

let reject parse source =
  match parse source with
  | Error _ -> print_endline "Rejected"
  | Ok _ -> failwith ("Unexpectedly accepted: " ^ source)
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

  let%expect_test "multiplication before addition" =
    print_expression "1 + 2 * 3";
    [%expect {| Apply (Var "+", [Int "1"; Apply (Var "*", [Int "2"; Int "3"])]) |}]
  ;;

  let%expect_test "left associative subtraction" =
    print_expression "a - b - c";
    [%expect {| Apply (Var "-", [Apply (Var "-", [Var "a"; Var "b"]); Var "c"]) |}]
  ;;

  let%expect_test "right associative power" =
    print_expression "a ** b ** c";
    [%expect {| Apply (Var "**", [Var "a"; Apply (Var "**", [Var "b"; Var "c"])]) |}]
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

  let%expect_test "mod and multiplication" =
    print_expression "a mod b * c";
    [%expect {| Apply (Var "*", [Apply (Var "mod", [Var "a"; Var "b"]); Var "c"]) |}]
  ;;

  let%expect_test "right associative conjunction" =
    print_expression "a && b && c";
    [%expect {| Apply (Var "&&", [Var "a"; Apply (Var "&&", [Var "b"; Var "c"])]) |}]
  ;;

  let%expect_test "logical precedence" =
    print_expression "a || b && c";
    [%expect {| Apply (Var "||", [Var "a"; Apply (Var "&&", [Var "b"; Var "c"])]) |}]
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

  let%expect_test "right associative concatenation" =
    print_expression "a @ b ^ c";
    [%expect {| Apply (Var "@", [Var "a"; Apply (Var "^", [Var "b"; Var "c"])]) |}]
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
    [%expect {| Let (PTuple [PVar "x"; PVar "y"] = Tuple [Int "1"; Int "2"], Var "x") |}]
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

  let%expect_test "a constructor used as an argument does not consume the next argument" =
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
end

module Cps = struct
  let name text = Result.get_ok (Name.of_identifier text)
  let variable text = Var (name text)
  let integer text = Constant (Int (Result.get_ok (Integer_literal.of_string text)))
  let call f argument rest = Apply (f, argument, rest)
  let operator text a b = call (Var (Result.get_ok (Name.of_operator text))) a [ b ]

  let lambda parameter body =
    Fun { parameter = PVar (name parameter); parameters = []; body }
  ;;

  let definition function_name body =
    Definition
      { recursion = Recursive
      ; pattern = PVar (name function_name)
      ; expression =
          Fun { parameter = PVar (name "n"); parameters = [ PVar (name "k") ]; body }
      }
  ;;

  let check source expected =
    match Parser.parse_program source with
    | Ok actual when actual = expected -> print_endline "Matches expected AST"
    | _ ->
      print_program source;
      failwith "CPS program did not produce the expected AST"
  ;;

  let%expect_test "factorial" =
    let n = variable "n" in
    let k = variable "k" in
    let expected =
      [ definition
          "fact"
          (If
             ( operator "=" n (integer "0")
             , call k (integer "1") []
             , Some
                 (call
                    (variable "fact")
                    (operator "-" n (integer "1"))
                    [ lambda "r" (call k (operator "*" n (variable "r")) []) ]) ))
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
    let n = variable "n" in
    let k = variable "k" in
    let expected =
      [ definition
          "fib"
          (If
             ( operator "<=" n (integer "1")
             , call k n []
             , Some
                 (call
                    (variable "fib")
                    (operator "-" n (integer "1"))
                    [ lambda
                        "a"
                        (call
                           (variable "fib")
                           (operator "-" n (integer "2"))
                           [ lambda
                               "b"
                               (call k (operator "+" (variable "a") (variable "b")) [])
                           ])
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
