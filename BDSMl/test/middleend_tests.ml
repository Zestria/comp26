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

let () =
  Alcotest.run
    "middleend"
    [ "free variables", [ Alcotest.test_case "examples" `Quick free_variables ]
    ; ( "closure conversion"
      , [ Alcotest.test_case "anonymous closure" `Quick converts_anonymous_closure
        ; Alcotest.test_case "local recursion" `Quick converts_local_recursion
        ; Alcotest.test_case "shadowing" `Quick captures_a_shadowing_local
        ] )
    ]
;;
