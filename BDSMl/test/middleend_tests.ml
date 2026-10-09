open BDSMl_frontend
open BDSMl_frontend.Ast
open BDSMl_middleend.Ast_passes

let check_free_vars name expected expression =
  let actual = Free_vars.free_vars expression |> Free_vars.String_set.elements in
  Alcotest.(check (list string)) name expected actual
;;

let free_variables () =
  check_free_vars
    "function"
    [ "k"; "n" ]
    (Abs
       ( PVar "p"
       , App (Var "k", BinOp (Mult, Var "p", Var "n")) ));
  check_free_vars
    "non-recursive let"
    [ "x" ]
    (Let
       ( NonRec
       , PVar "y"
       , Int 1
       , BinOp (Plus, Var "x", Var "y") ));
  check_free_vars
    "recursive let"
    []
    (Let
       ( Rec
       , PVar "f"
       , Abs (PVar "a", App (Var "f", Var "a"))
       , Var "f" ))
;;

let check_conversion name expected source =
  let actual = Closure_conv.closure_conv source in
  Alcotest.check Alcotest.string name expected (Printer.print_program actual)
;;

let converts_anonymous_closure () =
  check_conversion
    "free variables become parameters"
    "(((fun k -> (fun n -> (fun p -> (k (p * n))))) k) n);;"
    [ StrEval
        (Abs
           ( PVar "p"
           , App (Var "k", BinOp (Mult, Var "p", Var "n")) ))
    ]
;;

let converts_local_recursion () =
  let loop_body =
    If
      ( BinOp (Equal, Var "i", Int 0)
      , Var "y"
      , App (Var "loop", BinOp (Minus, Var "i", Int 1)) )
  in
  let source =
    [ StrLet
        ( NonRec
        , PVar "f"
        , Abs
            ( PVar "y"
            , Let
                ( Rec
                , PVar "loop"
                , Abs (PVar "i", loop_body)
                , App (Var "loop", Int 10) ) ) )
    ]
  in
  check_conversion
    "recursive calls receive the environment"
    "let f = (fun y -> (let rec loop = (fun y -> (fun i -> (if (i = 0) then y else ((loop y) (i - 1))))) in ((loop y) 10)));;"
    source
;;

let captures_a_shadowing_local () =
  let source =
    [ StrLet (NonRec, PVar "y", Int 100)
    ; StrLet
        ( NonRec
        , PVar "main"
        , Let
            ( NonRec
            , PVar "y"
            , Int 1
            , Let
                ( NonRec
                , PVar "f"
                , Abs (PVar "x", BinOp (Plus, Var "y", Var "x"))
                , App (Var "f", Int 2) ) ) )
    ]
  in
  check_conversion
    "a local shadows a global of the same name"
    "let y = 100;;\nlet main = (let y = 1 in (let f = ((fun y -> (fun x -> (y + x))) y) in (f 2)));;"
    source
;;

let alpha_scopes () =
  let source =
    [ StrLet (NonRec, PVar "x", Int 1)
    ; StrLet
        ( Rec
        , PVar "f"
        , Abs
            ( PVar "x"
            , Let
                ( NonRec
                , PVar "x"
                , BinOp (Plus, Var "x", Var "x_1")
                , App (Var "f", Var "x") ) ) )
    ; StrEval (Var "x")
    ]
  in
  let result = Alpha.alpha source in
  Alcotest.(check string)
    "recursive and sequential scope; free names stay free"
    "let x_2 = 1;;\nlet rec f_3 = (fun x_4 -> (let x_5 = (x_4 + x_1) in (f_3 x_5)));;\nx_2;;"
    (Printer.print_program result)
;;

let alpha_nonrecursive_binding () =
  let source =
    [ StrLet (NonRec, PVar "x", Int 5)
    ; StrLet (NonRec, PVar "x", BinOp (Plus, Var "x", Int 1))
    ; StrEval (Var "x")
    ]
  in
  Alcotest.(check string)
    "nonrecursive initializer sees the previous binding"
    "let x_1 = 5;;\nlet x_2 = (x_1 + 1);;\nx_2;;"
    (Printer.print_program (Alpha.alpha source))
;;

let () =
  Alcotest.run
    "middleend"
    [ "free variables", [ Alcotest.test_case "examples" `Quick free_variables ]
    ; ( "alpha conversion"
      , [ Alcotest.test_case "scopes and collisions" `Quick alpha_scopes
        ; Alcotest.test_case "sequential definitions" `Quick alpha_nonrecursive_binding
        ] )
    ; ( "closure conversion"
      , [ Alcotest.test_case "anonymous closure" `Quick converts_anonymous_closure
        ; Alcotest.test_case "local recursion" `Quick converts_local_recursion
        ; Alcotest.test_case "shadowing" `Quick captures_a_shadowing_local
        ] )
    ]
;;
