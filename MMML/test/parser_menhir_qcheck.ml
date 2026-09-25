open Ast
open QCheck

let var name = Variable name
let pvar name = PVariable name
let int value = Constant (Integer value)
let binding pattern expression = { pattern; expression }
let case case_pattern case_expression = { case_pattern; case_expression }

let operators =
  [ "+"; "-"; "*"; "/"; "="; "<>"; "<"; "<="; ">"; ">="; "&&"; "||"; "::"; "~-"; "~+" ]
;;

let parenthesize source = "( " ^ source ^ " )"
let joined render separator values = String.concat separator (List.map render values)
let name_source name = if List.mem name operators then parenthesize name else name

let constant_source = function
  | Integer value -> string_of_int value
  | Boolean value -> string_of_bool value
  | Unit -> "()"
;;

(** An intentionally fully parenthesized test serializer keeps the oracle
    independent of production pretty-printing and operator precedence. *)
let rec pattern_source = function
  | PWildcard -> "_"
  | PVariable name -> name_source name
  | PConstant constant -> parenthesize (constant_source constant)
  | PList patterns -> "[ " ^ joined pattern_source "; " patterns ^ " ]"
  | PCons (head, tail) -> parenthesize (pattern_source head ^ " :: " ^ pattern_source tail)
  | PTuple (first, second, rest) ->
    parenthesize (joined pattern_source ", " (first :: second :: rest))
;;

let rec expression_source expression =
  let source =
    match expression with
    | Constant constant -> constant_source constant
    | Variable name -> name_source name
    | Tuple (first, second, rest) ->
      joined expression_source ", " (first :: second :: rest)
    | List expressions -> "[ " ^ joined expression_source "; " expressions ^ " ]"
    | Lambda (parameter, body) ->
      "fun " ^ pattern_source parameter ^ " -> " ^ expression_source body
    | Application (function_, argument) ->
      expression_source function_ ^ " " ^ expression_source argument
    | If_then_else (condition, yes, no) ->
      "if "
      ^ expression_source condition
      ^ " then "
      ^ expression_source yes
      ^ " else "
      ^ expression_source no
    | Let_in (recursive, first, rest, body) ->
      bindings_source recursive (first :: rest) ^ " in " ^ expression_source body
    | Function (first, rest) -> "function " ^ cases_source (first :: rest)
    | Match (scrutinee, first, rest) ->
      "match " ^ expression_source scrutinee ^ " with " ^ cases_source (first :: rest)
  in
  parenthesize source

and binding_source { pattern; expression } =
  pattern_source pattern ^ " = " ^ expression_source expression

and bindings_source recursive bindings =
  let keyword =
    match recursive with
    | Nonrecursive -> "let "
    | Recursive -> "let rec "
  in
  keyword ^ joined binding_source " and " bindings

and cases_source cases =
  joined
    (fun { case_pattern; case_expression } ->
      "| " ^ pattern_source case_pattern ^ " -> " ^ expression_source case_expression)
    " "
    cases
;;

let program_source program =
  joined
    (fun (Value (recursive, first, rest)) ->
      bindings_source recursive (first :: rest) ^ ";;")
    "\n"
    program
;;

let small_list generator = Gen.list_size (Gen.int_range 0 2) generator
let names = Gen.oneof_list [ "x"; "y"; "n"; "k"; "xs"; "letx"; "value_1"; "f'"; "_tail" ]

let constants =
  Gen.oneof
    [ Gen.map (fun value -> Integer value) (Gen.int_range (-100) 100)
    ; Gen.map (fun value -> Boolean value) Gen.bool
    ; Gen.return Unit
    ]
;;

let recursive_flags = Gen.oneof_list [ Nonrecursive; Recursive ]

let rec patterns depth =
  let leaf =
    Gen.oneof
      [ Gen.return PWildcard
      ; Gen.map pvar names
      ; Gen.map (fun constant -> PConstant constant) constants
      ]
  in
  if depth = 0
  then leaf
  else (
    let child = patterns (depth - 1) in
    Gen.oneof_weighted
      [ 4, leaf
      ; 1, Gen.map (fun values -> PList values) (small_list child)
      ; 1, Gen.map2 (fun head tail -> PCons (head, tail)) child child
      ; 1, Gen.map3 (fun a b rest -> PTuple (a, b, rest)) child child (small_list child)
      ])
;;

let rec expressions depth =
  let leaf =
    Gen.oneof
      [ Gen.map (fun constant -> Constant constant) constants
      ; Gen.map var names
      ; Gen.map var (Gen.oneof_list operators)
      ]
  in
  if depth = 0
  then leaf
  else (
    let child = expressions (depth - 1) in
    let pattern = patterns (min 2 (depth - 1)) in
    let bindings = Gen.map2 binding pattern child in
    let cases = Gen.map2 case pattern child in
    Gen.oneof_weighted
      [ 4, leaf
      ; 1, Gen.map3 (fun a b rest -> Tuple (a, b, rest)) child child (small_list child)
      ; 1, Gen.map (fun values -> List values) (small_list child)
      ; 2, Gen.map2 (fun parameter body -> Lambda (parameter, body)) pattern child
      ; ( 3
        , Gen.map2
            (fun function_ argument -> Application (function_, argument))
            child
            child )
      ; ( 1
        , Gen.map3
            (fun condition yes no -> If_then_else (condition, yes, no))
            child
            child
            child )
      ; ( 1
        , Gen.map4
            (fun recursive first rest body -> Let_in (recursive, first, rest, body))
            recursive_flags
            bindings
            (small_list bindings)
            child )
      ; 1, Gen.map2 (fun first rest -> Function (first, rest)) cases (small_list cases)
      ; ( 1
        , Gen.map3
            (fun value first rest -> Match (value, first, rest))
            child
            cases
            (small_list cases) )
      ])
;;

let rec shrink_expression expression yield =
  let shrink_child rebuild child =
    yield child;
    shrink_expression child (fun smaller -> yield (rebuild smaller))
  in
  match expression with
  | Constant (Integer value) -> Shrink.int value (fun smaller -> yield (int smaller))
  | Constant _ | Variable _ -> ()
  | List expressions ->
    List.iter yield expressions;
    Shrink.list ~shrink:shrink_expression expressions (fun smaller ->
      yield (List smaller))
  | Tuple (a, b, rest) ->
    shrink_child (fun smaller -> Tuple (smaller, b, rest)) a;
    shrink_child (fun smaller -> Tuple (a, smaller, rest)) b;
    List.iter yield rest;
    Shrink.list ~shrink:shrink_expression rest (fun smaller ->
      yield (Tuple (a, b, smaller)))
  | Lambda (parameter, body) ->
    shrink_child (fun smaller -> Lambda (parameter, smaller)) body
  | Application (function_, argument) ->
    shrink_child (fun smaller -> Application (smaller, argument)) function_;
    shrink_child (fun smaller -> Application (function_, smaller)) argument
  | If_then_else (condition, yes, no) ->
    shrink_child (fun smaller -> If_then_else (smaller, yes, no)) condition;
    shrink_child (fun smaller -> If_then_else (condition, smaller, no)) yes;
    shrink_child (fun smaller -> If_then_else (condition, yes, smaller)) no
  | Let_in (recursive, first, rest, body) ->
    shrink_child (fun smaller -> Let_in (recursive, first, rest, smaller)) body;
    shrink_binding first (fun smaller -> yield (Let_in (recursive, smaller, rest, body)));
    Shrink.list ~shrink:shrink_binding rest (fun smaller ->
      yield (Let_in (recursive, first, smaller, body)))
  | Function (first, rest) ->
    yield first.case_expression;
    shrink_case first (fun smaller -> yield (Function (smaller, rest)));
    Shrink.list ~shrink:shrink_case rest (fun smaller ->
      yield (Function (first, smaller)))
  | Match (scrutinee, first, rest) ->
    shrink_child (fun smaller -> Match (smaller, first, rest)) scrutinee;
    yield first.case_expression;
    shrink_case first (fun smaller -> yield (Match (scrutinee, smaller, rest)));
    Shrink.list ~shrink:shrink_case rest (fun smaller ->
      yield (Match (scrutinee, first, smaller)))

and shrink_binding value yield =
  shrink_expression value.expression (fun expression -> yield { value with expression })

and shrink_case value yield =
  shrink_expression value.case_expression (fun case_expression ->
    yield { value with case_expression })
;;

let shrink_item (Value (recursive, first, rest)) yield =
  shrink_binding first (fun smaller -> yield (Value (recursive, smaller, rest)));
  Shrink.list ~shrink:shrink_binding rest (fun smaller ->
    yield (Value (recursive, first, smaller)))
;;

let arbitrary_expression =
  make
    ~print:expression_source
    ~shrink:shrink_expression
    (Gen.sized (fun size -> expressions (min 4 size)))
;;

let arbitrary_program =
  let bindings = Gen.map2 binding (patterns 2) (expressions 3) in
  let items =
    Gen.map3
      (fun recursive first rest -> Value (recursive, first, rest))
      recursive_flags
      bindings
      (small_list bindings)
  in
  make
    ~print:program_source
    ~shrink:(Shrink.list ~shrink:shrink_item)
    (Gen.list_size (Gen.int_range 0 4) items)
;;

let check_parse ~parse ~equal ~show source expected =
  match parse source with
  | Ok actual when equal expected actual -> true
  | Ok actual ->
    Test.fail_reportf
      "source: %S\nexpected: %s\nactual: %s"
      source
      (show expected)
      (show actual)
  | Error error ->
    Test.fail_reportf
      "source: %S\nunexpected error: %s"
      source
      (Error_monad.show_error error)
;;

let check_expression =
  check_parse ~parse:Parser_menhir.parse_expression ~equal:equal_expr ~show:show_expr
;;

let check_program =
  check_parse ~parse:Parser_menhir.parse_program ~equal:equal_program ~show:show_program
;;

let expression_round_trip =
  Test.make
    ~name:"Menhir: generated expressions recover the expected AST"
    ~count:1_000
    arbitrary_expression
    (fun expression -> check_expression (expression_source expression) expression)
;;

let program_round_trip =
  Test.make
    ~name:"Menhir: generated declarations recover the expected AST"
    ~count:500
    arbitrary_program
    (fun program -> check_program (program_source program) program)
;;

let refilled_lexbuf source =
  let offset = ref 0 in
  Lexing.from_function (fun buffer requested ->
    let count = min 1 (min requested (String.length source - !offset)) in
    Bytes.blit_string source !offset buffer 0 count;
    offset := !offset + count;
    count)
;;

let streaming_expressions =
  Test.make
    ~name:"Menhir: one-byte refills and nested comments preserve expression ASTs"
    ~count:250
    arbitrary_expression
    (fun expression ->
       let source =
         expression_source expression
         |> String.split_on_char ' '
         |> String.concat " (* outer\n(* inner *) *) "
       in
       check_parse
         ~parse:(fun source ->
           Parser_menhir.parse_expression_lexbuf (refilled_lexbuf source))
         ~equal:equal_expr
         ~show:show_expr
         source
         expression)
;;

let fixed_expressions =
  let a = var "a"
  and b = var "b"
  and c = var "c" in
  [ "a - b - c", infix "-" (infix "-" a b) c
  ; "a / b * c", infix "*" (infix "/" a b) c
  ; "a + b * c", infix "+" a (infix "*" b c)
  ; "a < b = c", infix "=" (infix "<" a b) c
  ; "a || b || c", infix "||" a (infix "||" b c)
  ; "a && b && c", infix "&&" a (infix "&&" b c)
  ; "a || b && c", infix "||" a (infix "&&" b c)
  ; "a < b && b <> c", infix "&&" (infix "<" a b) (infix "<>" b c)
  ; "a :: b :: []", infix "::" a (infix "::" b (List []))
  ; "a + b :: c", infix "::" (infix "+" a b) c
  ; "f a b + c", infix "+" (application (var "f") [ a; b ]) c
  ; "f (a + b)", Application (var "f", infix "+" a b)
  ; "-f a", prefix "~-" (Application (var "f", a))
  ; "~-f a", Application (prefix "~-" (var "f"), a)
  ; "+f a", prefix "~+" (Application (var "f", a))
  ; "~+f a", Application (prefix "~+" (var "f"), a)
  ; "f -1", infix "-" (var "f") (int 1)
  ; "f (-1)", Application (var "f", int (-1))
  ; "-1", int (-1)
  ; "+1", int 1
  ; "~-1", prefix "~-" (int 1)
  ; "~+1", prefix "~+" (int 1)
  ; "( * ) 2 3", infix "*" (int 2) (int 3)
  ; "( :: ) a b", infix "::" a b
  ; "a, b, c", Tuple (a, b, [ c ])
  ; ( "[(fun x -> x); (fun y -> y)]"
    , List [ Lambda (pvar "x", var "x"); Lambda (pvar "y", var "y") ] )
  ; ( "[(let x = 1 in x); 2]"
    , List [ Let_in (Nonrecursive, binding (pvar "x") (int 1), [], var "x"); int 2 ] )
  ; "fun x y -> x y", function_ [ pvar "x"; pvar "y" ] (Application (var "x", var "y"))
  ; ( "let rec f x = x and g y = y in f"
    , Let_in
        ( Recursive
        , binding (pvar "f") (Lambda (pvar "x", var "x"))
        , [ binding (pvar "g") (Lambda (pvar "y", var "y")) ]
        , var "f" ) )
  ; ( "match xs with [] -> 0 | x :: y :: rest -> x"
    , Match
        ( var "xs"
        , case (PList []) (int 0)
        , [ case (PCons (pvar "x", PCons (pvar "y", pvar "rest"))) (var "x") ] ) )
  ; ( "function (x, y, _) -> [x; y;]"
    , Function
        (case (PTuple (pvar "x", pvar "y", [ PWildcard ])) (List [ var "x"; var "y" ]), [])
    )
  ; ( "match a with _ -> match b with _ -> 1 | x -> 2"
    , Match
        ( a
        , case PWildcard (Match (b, case PWildcard (int 1), [ case (pvar "x") (int 2) ]))
        , [] ) )
  ; ( "match a with _ -> (match b with _ -> 1) | x -> 2"
    , Match
        ( a
        , case PWildcard (Match (b, case PWildcard (int 1), []))
        , [ case (pvar "x") (int 2) ] ) )
  ; ( "if a then if b then 1 else 2 else 3"
    , If_then_else (a, If_then_else (b, int 1, int 2), int 3) )
  ]
;;

let precedence_and_syntax =
  Test.make
    ~name:"Menhir: precedence, associativity, binding sugar and patterns"
    ~count:1
    unit
    (fun () ->
       List.for_all
         (fun (source, expected) -> check_expression source expected)
         fixed_expressions)
;;

let cps_examples =
  let n = var "n"
  and k = var "k" in
  let factorial =
    binding
      (pvar "fac")
      (function_
         [ pvar "n"; pvar "k" ]
         (If_then_else
            ( infix "<=" n (int 1)
            , Application (k, int 1)
            , application
                (var "fac")
                [ infix "-" n (int 1)
                ; Lambda (pvar "r", Application (k, infix "*" n (var "r")))
                ] )))
  in
  let fibonacci =
    binding
      (pvar "fib")
      (function_
         [ pvar "n"; pvar "k" ]
         (If_then_else
            ( infix "<=" n (int 1)
            , Application (k, n)
            , application
                (var "fib")
                [ infix "-" n (int 1)
                ; Lambda
                    ( pvar "a"
                    , application
                        (var "fib")
                        [ infix "-" n (int 2)
                        ; Lambda (pvar "b", Application (k, infix "+" (var "a") (var "b")))
                        ] )
                ] )))
  in
  let source =
    "let rec fac n k =\n\
     if n <= 1 then k 1 else fac (n - 1) (fun r -> k (n * r))\n\
     let rec fib n k =\n\
     if n <= 1 then k n else\n\
     fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)));;\n\
     let answer = fac 5 (fun x -> x)"
  in
  let expected =
    [ Value (Recursive, factorial, [])
    ; Value (Recursive, fibonacci, [])
    ; Value
        ( Nonrecursive
        , binding
            (pvar "answer")
            (application (var "fac") [ int 5; Lambda (pvar "x", var "x") ])
        , [] )
    ]
  in
  Test.make ~name:"Menhir: CPS factorial and Fibonacci programs" ~count:1 unit (fun () ->
    check_program source expected
    && check_parse
         ~parse:(fun source ->
           Parser_menhir.parse_program_lexbuf (refilled_lexbuf source))
         ~equal:equal_program
         ~show:show_program
         source
         expected)
;;

let check_rejected parse source =
  match parse source with
  | Error _ -> true
  | Ok _ -> Test.fail_reportf "unexpectedly accepted malformed source: %S" source
;;

let invalid_expressions =
  [ ""
  ; "(1"
  ; "1)"
  ; "(1,)"
  ; "[1;;2]"
  ; "[;]"
  ; "[fun x -> x; fun y -> y]"
  ; "[let x = 1 in x; 2]"
  ; "fun -> x"
  ; "fun x x"
  ; "let = 1 in x"
  ; "let x = 1"
  ; "let x = 1 in"
  ; "if x then y"
  ; "if x else y"
  ; "match x with"
  ; "function | -> x"
  ; "function x -> x |"
  ; "1 +"
  ; "1 in x"
  ; "x; y"
  ; "_"
  ; "X"
  ; "module"
  ]
;;

let invalid_syntax =
  Test.make
    ~name:"Menhir: malformed expressions and declarations fail"
    ~count:1
    unit
    (fun () ->
       List.for_all (check_rejected Parser_menhir.parse_expression) invalid_expressions
       && List.for_all
            (check_rejected Parser_menhir.parse_program)
            [ "1"; "let x ="; "let x = 1 in x"; "let x = 1;"; "let x = 1;; 42" ]
       && check_program "" [])
;;

let full_input_consumption =
  Test.make
    ~name:"Menhir: valid expression prefixes cannot hide trailing syntax errors"
    ~count:250
    arbitrary_expression
    (fun expression ->
       let source = expression_source expression in
       List.for_all
         (fun suffix -> check_rejected Parser_menhir.parse_expression (source ^ suffix))
         [ " )"; " in x"; " ;"; " let y = 0" ])
;;

let check_error_location parse source offset line column =
  match parse source with
  | Error { Error_monad.location; message } ->
    if location.offset = offset
       && location.line = line
       && location.column = column
       && String.length message > 0
    then true
    else
      Test.fail_reportf
        "source: %S\nexpected error at offset %d, line %d, column %d; got %s"
        source
        offset
        line
        column
        (Error_monad.show_error { location; message })
  | Ok _ -> Test.fail_reportf "expected a positioned parser error for %S" source
;;

let error_locations =
  Test.make
    ~name:"Menhir: syntax, EOF and lexical errors retain their positions"
    ~count:1
    unit
    (fun () ->
       check_error_location Parser_menhir.parse_expression "if true then\nelse 1" 13 2 1
       && check_error_location Parser_menhir.parse_expression "fun x ->\n" 9 2 1
       && check_error_location Parser_menhir.parse_expression "fun x ->\n@" 9 2 1
       && check_error_location Parser_menhir.parse_program "let x =\n)" 8 2 1)
;;

let () =
  QCheck_runner.run_tests_main
    [ expression_round_trip
    ; program_round_trip
    ; streaming_expressions
    ; precedence_and_syntax
    ; cps_examples
    ; invalid_syntax
    ; full_input_consumption
    ; error_locations
    ]
;;
