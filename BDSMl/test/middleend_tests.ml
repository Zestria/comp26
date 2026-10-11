open BDSMl_frontend
open BDSMl_frontend.Ast
open BDSMl_middleend.Ast_passes
open BDSMl_middleend.Anf

let anf_or_fail program =
  match Anf_conv.anf program with
  | Ok program -> program
  | Error message -> Alcotest.fail message
;;

let check_free_vars name expected expression =
  let actual = Free_vars.free_vars expression |> Free_vars.String_set.elements in
  Alcotest.(check (list string)) name expected actual
;;

let fresh_names_are_distinct () =
  let module Fresh = BDSMl_middleend.Fresh in
  let first, state = Fresh.fresh_ident "x" (Fresh.create []) in
  let second, state = Fresh.fresh_ident "x" state in
  let third, _ = Fresh.fresh_ident "y" state in
  Alcotest.(check (list string)) "one counter across bases"
    [ "x_1"; "x_2"; "y_3" ] [ first; second; third ]
;;

let fresh_names_reset () =
  let module Fresh = BDSMl_middleend.Fresh in
  let initial = Fresh.create [ "tmp_1"; "tmp_2" ] in
  let first, advanced = Fresh.fresh_ident "tmp" initial in
  let second, _ = Fresh.fresh_ident "tmp" advanced in
  let repeated, _ = Fresh.fresh_ident "tmp" initial in
  Alcotest.(check string) "reserved names are skipped" "tmp_3" first;
  Alcotest.(check string) "advanced supply" "tmp_4" second;
  Alcotest.(check string) "initial supply stays unchanged" first repeated
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

let rec has_local_function = function
  | Unit | Int _ | Bool _ | Var _ -> false
  | Abs _ | Let (Rec, _, _, _) -> true
  | BinOp (_, left, right) | App (left, right) ->
    has_local_function left || has_local_function right
  | UnOp (_, operand) -> has_local_function operand
  | If (condition, if_true, if_false) ->
    has_local_function condition
    || has_local_function if_true
    || has_local_function if_false
  | Let (NonRec, _, value, body) ->
    has_local_function value || has_local_function body
;;

let check_lifted_source source =
  let parsed =
    match LL_parser.Parser.parse_string_LL source with
    | Ok program -> program
    | Error _ -> Alcotest.fail "could not parse the CPS example"
  in
  let lifted =
    parsed
    |> Alpha.alpha
    |> Closure_conv.closure_conv
    |> Lambda_lift.lambda_lift
  in
  let is_lifted_item = function
    | StrEval expression -> not (has_local_function expression)
    | StrLet (_, _, value) ->
      let rec skip_parameters = function
        | Abs (_, body) -> skip_parameters body
        | body -> not (has_local_function body)
      in
      skip_parameters value
  in
  Alcotest.(check bool) "only top-level functions remain" true
    (List.for_all is_lifted_item lifted);
  let printed = Printer.print_program lifted in
  (match Parse.parse printed with
   | Ok _ -> ()
   | Error message -> Alcotest.fail ("lifted output is not valid source: " ^ message))
;;

let lifts_cps_factorial () =
  check_lifted_source
    "let rec fact_cps n k = if n <= 1 then k 1 else fact_cps (n - 1) (fun m -> k (n * m)) in fact_cps 5 (fun x -> x)"
;;

let lifts_cps_fibonacci () =
  check_lifted_source
    "let rec fib_cps n k = if n <= 1 then k n else fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b))) in fib_cps 10 (fun x -> x)"
;;

let lifts_local_recursion () =
  check_lifted_source
    "let f y = let rec loop i = if i = 0 then y else loop (i - 1) in loop 10"
;;

let compact_anf_aliases () =
  let open Anf_ast in
  let check expected expression =
    let actual = anf_or_fail [ StrEval expression ] in
    Alcotest.(check bool) "minimal ANF" true (actual = [ AEval expected ])
  in
  check
    (ACExpr (CBinOp (Plus, ImmInt 1, ImmInt 2)))
    (Let (NonRec, PVar "x", Int 1, BinOp (Plus, Var "x", Int 2)));
  check
    (ACExpr (CApp (ImmId "f", [ ImmInt 2 ])))
    (Let (NonRec, PVar "x", App (Var "f", Int 2), Var "x"));
  check
    (ACExpr (CApp (ImmId "f", [ ImmId "a"; ImmId "b" ])))
    (Let
       ( NonRec
       , PVar "t1"
       , App (Var "f", Var "a")
       , Let
           ( NonRec
           , PVar "t2"
           , App (Var "t1", Var "b")
           , Var "t2" ) ))
;;

let anf_preserves_shadowing_and_names () =
  let open Anf_ast in
  let source =
    Let
      ( NonRec
      , PVar "x"
      , Var "y"
      , Let
          ( NonRec
          , PVar "y"
          , App (Var "f", Int 3)
          , BinOp (Plus, Var "x", Var "y") ) )
  in
  let actual = anf_or_fail [ StrEval source ] in
  let expected =
    [ AEval
        (ALet
           ( PVar "x"
           , CImm (ImmId "y")
           , ALet
               ( PVar "y"
               , CApp (ImmId "f", [ ImmInt 3 ])
               , ACExpr (CBinOp (Plus, ImmId "x", ImmId "y")) ) ))
    ]
  in
  Alcotest.(check bool) "alias does not capture a later binding" true (actual = expected);
  let collision =
    anf_or_fail
      [ StrEval (App (Var "f", BinOp (Plus, Var "a_1", Int 2))) ]
  in
  let expected_collision =
    [ AEval
        (ALet
           ( PVar "a_2"
           , CBinOp (Plus, ImmId "a_1", ImmInt 2)
           , ACExpr (CApp (ImmId "f", [ ImmId "a_2" ])) ))
    ]
  in
  Alcotest.(check bool) "temporary avoids source names" true
    (collision = expected_collision)
;;

let anf_reports_invalid_input () =
  let recursion = Let (Rec, PVar "x", Int 1, Var "x") in
  let abstraction = Abs (PVar "x", Var "x") in
  let check expected expression =
    match Anf_conv.anf [ StrEval expression ] with
    | Error message -> Alcotest.(check string) "diagnostic" expected message
    | Ok _ -> Alcotest.fail "invalid ANF input was accepted"
  in
  check "ANF: локальный let rec должен быть убран до ANF"
    (If (Bool true, Int 0, recursion));
  check "ANF: лямбда должна быть убрана до ANF"
    (App (Var "f", abstraction));
  match BDSMl_middleend.Pipeline.run ~stop_after:BDSMl_middleend.Pipeline.Anf
          [ StrEval (Let (Rec, PVar "x", Var "x", Var "x")) ] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "pipeline accepted an unsupported recursive value"
;;

let passes_are_independent () =
  let parse source =
    match Parse.parse source with
    | Ok program -> program
    | Error message -> Alcotest.fail message
  in
  let source = parse
      "let a_1 = 4 let lifted_1 x = let y = x + a_1 in fun z -> y + z let main = lifted_1 2 3"
  in
  let other = parse "let other x = fun y -> x + y" in
  let check name transform =
    let expected = transform source in
    let _ = transform other in
    Alcotest.(check bool) name true (expected = transform source)
  in
  check "alpha" Alpha.alpha;
  check "lambda lifting" (fun source ->
    source |> Alpha.alpha |> Closure_conv.closure_conv |> Lambda_lift.lambda_lift);
  let run = BDSMl_middleend.Pipeline.run ~stop_after:BDSMl_middleend.Pipeline.Anf in
  let expected = run source in
  (match expected with
   | Ok _ -> ()
   | Error message -> Alcotest.fail message);
  let _ = run (parse "let main = let rec x = x in x") in
  Alcotest.(check bool) "ANF after failed conversion" true (expected = run source)
;;

let () =
  Alcotest.run
    "middleend"
    [ "free variables", [ Alcotest.test_case "examples" `Quick free_variables ]
    ; ( "fresh names"
      , [ Alcotest.test_case "distinct" `Quick fresh_names_are_distinct
        ; Alcotest.test_case "independent supplies" `Quick fresh_names_reset
        ; Alcotest.test_case "independent passes" `Quick passes_are_independent
        ] )
    ; ( "alpha conversion"
      , [ Alcotest.test_case "scopes and collisions" `Quick alpha_scopes
        ; Alcotest.test_case "sequential definitions" `Quick alpha_nonrecursive_binding
        ] )
    ; ( "closure conversion"
      , [ Alcotest.test_case "anonymous closure" `Quick converts_anonymous_closure
        ; Alcotest.test_case "local recursion" `Quick converts_local_recursion
        ; Alcotest.test_case "shadowing" `Quick captures_a_shadowing_local
        ] )
    ; ( "lambda lifting"
      , [ Alcotest.test_case "CPS factorial" `Quick lifts_cps_factorial
        ; Alcotest.test_case "CPS fibonacci" `Quick lifts_cps_fibonacci
        ; Alcotest.test_case "local recursion" `Quick lifts_local_recursion
        ] )
    ; ( "ANF"
      , [ Alcotest.test_case "remove aliases" `Quick compact_anf_aliases
        ; Alcotest.test_case "scope and names" `Quick anf_preserves_shadowing_and_names
        ; Alcotest.test_case "invalid input" `Quick anf_reports_invalid_input
        ] )
    ]
;;
