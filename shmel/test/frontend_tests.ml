open Shmel_lib

let fail fmt = Printf.ksprintf failwith fmt

let check_ast parser_name parse (name, source, expected) =
  match parse source with
  | Ok actual when actual = expected -> ()
  | Ok actual ->
    fail
      "%s: %s: expected %s, got %s"
      parser_name
      name
      (Ast.show_expr expected)
      (Ast.show_expr actual)
  | Error message -> fail "%s: %s: rejected input: %s" parser_name name message
;;

let menhir source =
  try Ok (Main.parse_with_menhir source) with
  | Parser.Error -> Error "syntax error"
;;

let combinator source =
  match Main.parse_with_comb source with
  | Ok ast -> Ok ast
  | Error error -> Error (Format.asprintf "%a" Parser_comb.pp_error error)
;;

let check_rejected parser_name parse source =
  match parse source with
  | Error _ -> ()
  | Ok ast ->
    fail "%s: accepted invalid input %S as %s" parser_name source (Ast.show_expr ast)
;;

let lex source =
  let lexbuf = Lexing.from_string source in
  let rec collect acc =
    let token = Lexer.read lexbuf in
    match token with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | _ -> collect (token :: acc)
  in
  collect []
;;

let check_tokens name source expected =
  if lex source <> expected then fail "lexer: %s: unexpected token sequence" name
;;

let check_lexer_rejected name source =
  match lex source with
  | _ -> fail "lexer: %s: accepted invalid input %S" name source
  | exception Failure message when message = "lexing: empty token" -> ()
;;

let cps_factorial =
  {|
let rec fact n k =
  if n = 0 then k 1
  else fact (n - 1) (fun x -> k (n * x))
in fact 5 (fun x -> x)
|}
;;

let cps_fibonacci =
  {|
let rec fib n k =
  if n <= 1 then k n
  else fib (n - 1) (fun x -> fib (n - 2) (fun y -> k (x + y)))
in fib 10 (fun x -> x)
|}
;;

let cps_factorial_ast =
  let open Ast in
  LetRec
    ( "fact"
    , "n"
    , Abs
        ( "k"
        , If
            ( BinOp (Eq, Var "n", Const 0)
            , App (Var "k", Const 1)
            , App
                ( App (Var "fact", BinOp (Sub, Var "n", Const 1))
                , Abs ("x", App (Var "k", BinOp (Mul, Var "n", Var "x"))) ) ) )
    , App (App (Var "fact", Const 5), Abs ("x", Var "x")) )
;;

let cps_fibonacci_ast =
  let open Ast in
  LetRec
    ( "fib"
    , "n"
    , Abs
        ( "k"
        , If
            ( BinOp (Le, Var "n", Const 1)
            , App (Var "k", Var "n")
            , App
                ( App (Var "fib", BinOp (Sub, Var "n", Const 1))
                , Abs
                    ( "x"
                    , App
                        ( App (Var "fib", BinOp (Sub, Var "n", Const 2))
                        , Abs ("y", App (Var "k", BinOp (Add, Var "x", Var "y"))) ) ) ) )
        )
    , App (App (Var "fib", Const 10), Abs ("x", Var "x")) )
;;

let () =
  let open Ast in
  check_tokens
    "keywords and whitespace"
    "let\trec f2 x = 12\nin f2 x"
    [ Parser.LET
    ; Parser.REC
    ; Parser.ID "f2"
    ; Parser.ID "x"
    ; Parser.EQUALS
    ; Parser.CONST 12
    ; Parser.IN
    ; Parser.ID "f2"
    ; Parser.ID "x"
    ; Parser.EOF
    ];
  check_tokens
    "longest operators"
    "< <= > >= ->"
    [ Parser.LT; Parser.LE; Parser.GT; Parser.GE; Parser.ARROW; Parser.EOF ];
  check_tokens
    "keyword prefixes are identifiers"
    "letter inside iffy"
    [ Parser.ID "letter"; Parser.ID "inside"; Parser.ID "iffy"; Parser.EOF ];
  check_lexer_rejected "invalid character" "1 + @";
  let cases =
    [ ( "precedence and left associativity"
      , "1 - 2 * 3 - 4"
      , BinOp (Sub, BinOp (Sub, Const 1, BinOp (Mul, Const 2, Const 3)), Const 4) )
    ; ( "application with a parenthesized argument"
      , "f x (g y)"
      , App (App (Var "f", Var "x"), App (Var "g", Var "y")) )
    ; ( "recursive function with two parameters"
      , "let rec sum x y = x + y in sum 2 3"
      , LetRec
          ( "sum"
          , "x"
          , Abs ("y", BinOp (Add, Var "x", Var "y"))
          , App (App (Var "sum", Const 2), Const 3) ) )
    ; ( "conditional with application in both branches"
      , "if x <= 1 then f x else g (x - 1)"
      , If
          ( BinOp (Le, Var "x", Const 1)
          , App (Var "f", Var "x")
          , App (Var "g", BinOp (Sub, Var "x", Const 1)) ) )
    ]
  in
  List.iter (check_ast "menhir" menhir) cases;
  List.iter (check_ast "combinator" combinator) cases;
  List.iter
    (fun (parse_name, parse) ->
       check_ast parse_name parse ("CPS factorial", cps_factorial, cps_factorial_ast);
       check_ast parse_name parse ("CPS fibonacci", cps_fibonacci, cps_fibonacci_ast))
    [ "menhir", menhir; "combinator", combinator ];
  List.iter
    (fun source ->
       check_rejected "menhir" menhir source;
       check_rejected "combinator" combinator source)
    [ "(1 + 2"; "1 +" ];
  check_rejected "combinator" combinator "1 + @";
  let extended source =
    match Main.parse_with_comb_extended source with
    | Ok ast -> Ok ast
    | Error error -> Error (Format.asprintf "%a" Parser_comb.pp_error error)
  in
  let source = "let in = 1 in in" in
  check_rejected "combinator without --extended" combinator source;
  check_ast
    "combinator with --extended"
    extended
    ("keyword as name", source, Let ("in", Const 1, Var "in"))
;;
