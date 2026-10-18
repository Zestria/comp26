open Middleend
module Cc = Closure_conversion
module Eval = Closure_eval

let p name = Ast.PVariable name
let v name = Ast.Variable name
let int value = Ast.Constant (Ast.Integer value)
let def name rhs = Ast.Definition (Ast.Nonrecursive, p name, rhs)

let parse source =
  match Parser_menhir.parse_program source with
  | Ok program -> program
  | Error error -> failwith (Error_monad.show_error error)
;;

let convert ?(externals = Eval.external_names) program =
  match Cc.convert ~externals program with
  | Error error -> failwith (Format.asprintf "%a" Cc.pp_error error)
  | Ok result ->
    (match Closure_test_invariants.check ~externals result with
     | Ok () -> result
     | Error error -> failwith error)
;;

let equal label expected actual = if expected <> actual then failwith label

let compare_behavior ?(trace = []) ?(result_name = "result") expected ast =
  let converted = convert ast in
  let source = Eval.source ~result_name ast in
  let target = Eval.source ~result_name converted in
  let wanted = { Eval.outcome = expected; trace } in
  if source <> wanted || target <> wanted
  then
    failwith
      (Printf.sprintf
         "expected %s\nsource %s\ntarget %s\nAST %s\nIR %s"
         (Eval.show_observation wanted)
         (Eval.show_observation source)
         (Eval.show_observation target)
         (Ast.show_program ast)
         (Format.asprintf "%a" Ast.pp_program converted))
;;

let integer_result source expected =
  compare_behavior (Ok (Ast.Integer expected)) (parse source)
;;

let check_error program problem =
  match Cc.convert ~externals:[] program with
  | Error error when error.problem = problem && error.path <> [] ->
    if String.length (Format.asprintf "%a" Cc.pp_error error) = 0
    then failwith "empty error"
  | _ -> failwith "expected structured conversion error"
;;

let structure () =
  equal "empty" [] (convert ~externals:[] []);
  check_error [ def "result" (v "missing") ] (Cc.Unbound_identifier "missing");
  check_error (parse "let f x = missing") (Cc.Unbound_identifier "missing");
  let closed = parse "let id x = x let add x y = x + y" in
  equal "closed parameter groups unchanged" closed (convert closed);
  let source = parse "let make x z = let f y = x + z + y in f" in
  (match Closure_test_invariants.check ~externals:Eval.external_names source with
   | Error _ -> ()
   | Ok () -> failwith "checker accepted implicit capture");
  let output = convert source in
  (match output with
   | [ Ast.Definition
         ( _
         , _
         , Ast.Lambda
             ( px
             , Ast.Lambda
                 ( pz
                 , Ast.Let_in
                     ( _
                     , _
                     , Ast.Application
                         ( Ast.Application
                             ( Ast.Lambda
                                 ( Ast.PVariable cx
                                 , Ast.Lambda (Ast.PVariable cz, Ast.Lambda (py, body)) )
                             , Ast.Variable actual_x )
                         , Ast.Variable actual_z )
                     , _ ) ) ) )
     ] ->
     equal "original params" (p "x", p "z", p "y") (px, pz, py);
     equal "capture values" ("x", "z") (actual_x, actual_z);
     equal
       "capture substitution"
       (Ast.infix "+" (Ast.infix "+" (v cx) (v cz)) (v "y"))
       body;
     assert (cx <> cz && cx <> "x" && cz <> "z")
   | _ -> failwith (Ast.show_program output));
  equal
    "external order and duplicates"
    (convert ~externals:[ "+"; "-" ] source)
    (convert ~externals:[ "-"; "+"; "+" ] source);
  let original =
    [ def
        "result"
        (Ast.application
           (v "f")
           [ Ast.Application (v "g", v "x"); Ast.Application (v "h", v "y") ])
    ]
  in
  equal
    "unary calls stay in place"
    original
    (convert ~externals:[ "f"; "g"; "h"; "x"; "y" ] original);
  ignore (convert (parse "let ignored x = fun y -> x"));
  equal "deterministic" output (convert source);
  equal
    "printer deterministic"
    (Ast.show_program output)
    (Ast.show_program (convert source));
  (* Generated names must avoid bound, free and external names alike. *)
  let collision =
    parse
      "let cc_capture_0 = 10 let cc_code_1 = 20 let f x = cc_capture_0 + cc_code_1 + x \
       let result = f 3"
  in
  compare_behavior (Ok (Ast.Integer 33)) collision
;;

let nonrecursive () =
  integer_result
    "let make x = let f y = x + y in f let a = make 10 let b = make 20 let result = a 1 \
     + b 1"
    32;
  integer_result "let make x = fun x -> x let result = make 10 3" 3;
  List.iter
    (fun (source, expected) -> integer_result source expected)
    [ "let result = let x = 1 in let x = x + 1 in x", 2
    ; "let x = 10 let f y = x + y let x = 20 let result = f 1", 11
    ; ( "let x = 10 let f y = let before = x in let x = y in before + x let result = f 2"
      , 12 )
    ; "let x = 100 let id x = x let result = id 3", 3
    ; "let result = let x = 10 in let _ = 20 in (fun _ -> x) ()", 10
    ; "let make x = fun y -> fun z -> x + y + z let result = make 10 20 3", 33
    ; "let add x y = x + y let add5 = add 5 let result = add5 3", 8
    ; "let apply f x = f x let result = apply (fun n -> n + 1) 4", 5
    ; "let result = if true then 1 else 1 / 0", 1
    ; "let result = if false then 1 / 0 else 2", 2
    ; ( "let env = 10 let self = 20 let closure_0 = 3 let f env = env + self + closure_0 \
         let result = f 1"
      , 24 )
    ; "let f x = service x let service x = 0 let result = f 1", 101
    ; "let service x = x + 2 let f x = service x let result = f 1", 3
    ; "let x = 1 let f y = x let x = 2 let g y = x let result = f () + g ()", 3
    ];
  let two =
    parse
      "let make_adder x = fun y -> x + y let a = make_adder 10 let b = make_adder 20 let \
       ra = a 1 let rb = b 1"
  in
  compare_behavior ~result_name:"ra" (Ok (Ast.Integer 11)) two;
  compare_behavior ~result_name:"rb" (Ok (Ast.Integer 21)) two;
  let shadow_operator =
    [ def "+" (Ast.Lambda (p "x", Ast.Lambda (p "y", v "x")))
    ; def "result" (Ast.infix "+" (int 1) (int 2))
    ]
  in
  compare_behavior (Ok (Ast.Integer 1)) shadow_operator;
  compare_behavior (Error Eval.Division_by_zero) (parse "let result = 1 / 0");
  compare_behavior (Ok (Ast.Boolean true)) (parse "let f x = x < 2 let result = f 1");
  compare_behavior (Ok Ast.Unit) (parse "let id x = x let result = id ()")
;;

let recursive () =
  integer_result
    "let make base = let pass f x = f x in let rec loop n = if n = 0 then base else pass \
     loop (n - 1) in loop let result = make 7 3"
    7;
  integer_result
    "let make base = let rec loop n = if n = 0 then base else (let saved x = loop x in \
     saved (n - 1)) in loop let result = make 7 3"
    7;
  integer_result
    "let make base = let rec loop n = if n = 0 then base else (let self = loop in self \
     (n - 1)) in loop let result = make 7 3"
    7;
  List.iter
    (fun (source, expected) -> integer_result source expected)
    [ ( "let make base = let rec loop n = if n = 0 then base else loop (n - 1) in loop \
         let result = make 7 3"
      , 7 )
    ; ( "let pass f x = f x let rec loop n = if n = 0 then 0 else pass loop (n - 1) let \
         result = loop 3"
      , 0 )
    ; "let rec f f = f let result = f 3", 3
    ; "let rec f n = let f x = x + 1 in f n let result = f 3", 4
    ; ( "let x = 10 let rec f n = if n = 0 then x else f (n - 1) let x = 20 let result = \
         f 3"
      , 10 )
    ; "let rec f n = if n = 0 then 7 else (fun k -> k (n - 1)) f let result = f 3", 7
    ; "let f = 100 let rec f n = if n = 0 then 1 else f (n - 1) let result = f 3", 1
    ];
  check_error (parse "let rec x = 1") (Cc.Recursive_rhs_not_lambda "x");
  check_error (parse "let result = let rec x = 1 in x") (Cc.Recursive_rhs_not_lambda "x");
  check_error
    [ Ast.Definition (Ast.Recursive, Ast.PWildcard, Ast.Lambda (p "x", v "x")) ]
    Cc.Recursive_wildcard;
  let cps =
    "let rec fac n k = if n < 2 then k 1 else fac (n - 1) (fun a -> k (a * n))\n\
     let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
     -> k (a + b)))\n\
     let id x = x\n"
  in
  List.iter
    (fun (call, expected) -> integer_result (cps ^ "let result = " ^ call) expected)
    [ "fac 0 id", 1; "fac 5 id", 120; "fib 0 id", 0; "fib 1 id", 1; "fib 10 id", 55 ];
  integer_result
    (cps ^ "let offset = 7 let result = fac 5 (fun result -> result + offset)")
    127
;;

let effects () =
  compare_behavior
    ~trace:[ 1; 2; 3; 4 ]
    (Ok (Ast.Integer 6))
    (parse
       "let f = (let _ = tick 1 in fun x -> let _ = tick 3 in fun y -> x + y) let result \
        = f (tick 2) (tick 4)");
  compare_behavior
    ~trace:[ 1; 2; 2; 3 ]
    (Ok (Ast.Integer 8))
    (parse
       "let make x = let _ = tick x in fun y -> x + tick y let a = make 1 let b = make 2 \
        let result = a 2 + b 3");
  compare_behavior
    ~trace:[ 8 ]
    (Ok (Ast.Integer 1))
    (parse "let _ = tick 8 let result = if true then 1 else tick 9")
;;

let () =
  List.iter
    (fun (name, test) ->
       test ();
       Printf.printf "closure conversion: %s passed\n" name)
    [ "structure and diagnostics", structure
    ; "lexical scope and higher-order behavior", nonrecursive
    ; "recursion and CPS", recursive
    ; "evaluation count and traces", effects
    ]
;;
