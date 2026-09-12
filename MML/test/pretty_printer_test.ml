open Ast

let var name = Variable name
let pvar name = PVariable name
let int value = Constant (Integer value)
let bool value = Constant (Boolean value)

let check ~name ~show ~printer expected value =
  let actual = Format.asprintf "%a" printer value in
  if not (String.equal expected actual)
  then
    failwith
      (Format.asprintf
         "%s:@ expected:@,%S@ but got:@,%S@ AST:@,%s"
         name
         expected
         actual
         (show value))
;;

let check_expr name expected =
  check ~name ~show:show_expr ~printer:Pretty_printer.pp_expr expected
;;

let check_pattern name expected =
  check ~name ~show:show_pattern ~printer:Pretty_printer.pp_pattern expected
;;

let check_program name expected =
  check ~name ~show:show_program ~printer:Pretty_printer.pp_program expected
;;

let test_constants_and_collections () =
  check_expr
    "tuple"
    "(1, true, ())"
    (Tuple (int 1, bool true, [ Constant Unit ]));
  check_expr "list" "[1; 2 + 3]" (List [ int 1; infix "+" (int 2) (int 3) ]);
  check_pattern
    "list pattern"
    "[1; true; _]"
    (PList [ PConstant (Integer 1); PConstant (Boolean true); PWildcard ]);
  check_pattern
    "right-associative cons pattern"
    "x :: y :: []"
    (PCons (pvar "x", PCons (pvar "y", PList [])))
;;

let test_operator_precedence () =
  let a = var "a" and b = var "b" and c = var "c" and d = var "d" in
  check_expr
    "mixed precedence"
    "(a + b) * (c - d)"
    (infix "*" (infix "+" a b) (infix "-" c d));
  check_expr "left associativity" "a - b - c" (infix "-" (infix "-" a b) c);
  check_expr "right operand grouping" "a - (b - c)" (infix "-" a (infix "-" b c));
  check_expr
    "right-associative cons"
    "a :: b :: []"
    (infix "::" a (infix "::" b (List [])));
  check_expr "prefix precedence" "not (a && b)" (prefix "not" (infix "&&" a b));
  check_expr
    "operator as a value"
    "(++) a b"
    (application (var "++") [ a; b ])
;;

let test_functions_and_applications () =
  check_expr
    "curried function"
    "fun x y -> x + y"
    (function_ [ pvar "x"; pvar "y" ] (infix "+" (var "x") (var "y")));
  check_expr
    "compound arguments"
    "f (g x) (fun y -> y)"
    (application
       (var "f")
       [ application (var "g") [ var "x" ]; function_ [ pvar "y" ] (var "y") ])
;;

let test_match () =
  let empty_case = { case_pattern = PList []; case_expression = int 0 } in
  let cons_case =
    { case_pattern = PCons (pvar "x", pvar "xs")
    ; case_expression = infix "+" (var "x") (int 1)
    }
  in
  check_expr
    "match"
    "match xs with\n  | [] -> 0\n  | x :: xs -> x + 1"
    (Match (var "xs", empty_case, [ cons_case ]));
  check_expr
    "function cases"
    "function\n  | [] -> 0\n  | x :: xs -> x + 1"
    (Function (empty_case, [ cons_case ]))
;;

let test_program () =
  let fac_body =
    If_then_else
      ( infix "<" (var "n") (int 2)
      , application (var "k") [ int 1 ]
      , application
          (var "fac")
          [ infix "-" (var "n") (int 1)
          ; function_
              [ pvar "a" ]
              (application (var "k") [ infix "*" (var "a") (var "n") ])
          ] )
  in
  let fac =
    { pattern = pvar "fac"
    ; expression = function_ [ pvar "n"; pvar "k" ] fac_body
    }
  in
  check_program
    "CPS factorial"
    (String.concat
       "\n"
       [ "let rec fac n k ="
       ; "  if n < 2 then"
       ; "    k 1"
       ; "  else"
       ; "    fac (n - 1) (fun a -> k (a * n))"
       ])
    [ Value (Recursive, fac, []) ]
;;

let () =
  test_constants_and_collections ();
  test_operator_precedence ();
  test_functions_and_applications ();
  test_match ();
  test_program ()
;;
