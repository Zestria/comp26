open Middleend
open Anf

let check condition message = if not condition then failwith message

let normalize expr =
  match Normalize.expression expr with
  | Ok term -> term
  | Error e -> failwith (Format.asprintf "%a" Normalize.pp_error e)
;;

let parse text =
  match Parser_menhir.parse_expression text with
  | Ok expr -> expr
  | Error e -> failwith (Format.asprintf "%a" Error_monad.pp_error e)
;;

let norm text = normalize (parse text)
let scoped text externals = check (Check.term ~externals (norm text) = Ok ()) text

let rec nodes = function
  | Result rhs -> 1 + compounds rhs
  | Let (_, rhs, body) -> 1 + compounds rhs + nodes body
  | Let_rec (_, _, body, continuation) -> 1 + nodes body + nodes continuation

and compounds = function
  | Lambda (_, body) -> nodes body
  | If (_, yes, no) -> nodes yes + nodes no
  | Value _ | Apply _ | Unary_primitive _ | Binary_primitive _ -> 0
;;

let () =
  let a = norm "f (g x)" in
  ignore (norm "fun x -> h x");
  check (norm "f (g x)" = a) "A/B/A fresh state";
  check
    (Pretty_printer.show_term a = Pretty_printer.show_term (norm "f (g x)"))
    "printer repeatability";
  scoped "f (g anf0)" [ "f"; "g"; "anf0" ];
  scoped "let x = 7 in (let x = 2 in x) + x" [];
  scoped "fun x -> fun x -> x" [];
  scoped "fun x -> f x" [ "f" ];
  check
    (match norm "f x" with
     | Let (_, _, Let (_, _, Result (Apply _))) -> true
     | _ -> false)
    "tail call";
  check
    (match norm "let f = fun x -> fun y -> x in f 1 2" with
     | Let (_, Lambda _, Let (_, Apply _, Result (Apply _))) -> true
     | _ -> false)
    "unary curried calls";
  check
    (norm "let x = 42 in x" = Result (Value (Constant (Ast.Integer 42))))
    "constant alias";
  check
    (match norm "let _ = print_int 1 in 42" with
     | Let
         ( _
         , Value (External "print_int")
         , Let (_, Apply _, Result (Value (Constant (Ast.Integer 42)))) ) -> true
     | _ -> false)
    "wildcard effect retained";
  let conditional = norm "fun c -> fun x -> fun y -> h (if c then f x else g y)" in
  check (Check.term ~externals:[ "h"; "f"; "g" ] conditional = Ok ()) "if scope";
  let rec function_body = function
    | Result (Lambda (_, body)) -> function_body body
    | body -> body
  in
  check
    (match function_body conditional with
     | Let (_, Value (External "h"), Let (_, If _, Result (Apply _))) -> true
     | _ -> false)
    "one shared continuation";
  let rec chain depth =
    if depth = 0
    then Ast.Variable "x"
    else
      Ast.Let_in
        ( Ast.Nonrecursive
        , Ast.PVariable "x"
        , Ast.If_then_else (Ast.Variable "c", Ast.Variable "x", Ast.Variable "x")
        , chain (depth - 1) )
  in
  let size n =
    nodes (normalize (Ast.function_ [ Ast.PVariable "c"; Ast.PVariable "x" ] (chain n)))
  in
  check (size 40 - size 20 = 2 * (size 20 - size 10)) "linear branch continuation growth";
  check
    (match norm "(5 + 4) - 2" with
     | Let (_, Binary_primitive (Add, _, _), Result (Binary_primitive (Subtract, _, _)))
       -> true
     | _ -> false)
    "compact arithmetic";
  check
    (match norm "(+) 1" with
     | Result (Apply (Builtin (Binary Add), _)) -> true
     | _ -> false)
    "partial builtin";
  scoped "let (+) x y = x in 1 + 2" [];
  check
    (match norm "let (+) x y = x in 1 + 2" with
     | Let (_, Lambda _, Let (_, Apply _, Result (Apply _))) -> true
     | _ -> false)
    "shadowed operator is a function";
  check
    (match norm "let plus = (+) in plus 1 2" with
     | Result (Binary_primitive (Add, _, _)) -> true
     | _ -> false)
    "arithmetic alias";
  check
    (match norm "let both = (&&) in both false (dangerous ())" with
     | Result
         (If
            ( Constant (Ast.Boolean false)
            , _
            , Result (Value (Constant (Ast.Boolean false))) )) -> true
     | _ -> false)
    "builtin alias short circuit";
  List.iter
    (fun source ->
       check
         (match norm source with
          | Result (If _) -> true
          | _ -> false)
         source)
    [ "true && dangerous ()"
    ; "false && dangerous ()"
    ; "true || dangerous ()"
    ; "false || dangerous ()"
    ];
  check
    (match norm "let (&&) x y = x in false && true" with
     | Let (_, Lambda _, Let (_, Apply _, Result (Apply _))) -> true
     | _ -> false)
    "shadowed logical operator is strict";
  scoped "let rec f x = if x = 0 then 0 else f (x - 1) in f 3" [];
  check
    (Normalize.expression (parse "let rec x = 1 in x")
     = Error (Normalize.Recursive_non_function "x"))
    "invalid recursive rhs";
  check
    (Normalize.expression (parse "let rec _ = fun x -> x in 0")
     = Error Normalize.Recursive_wildcard)
    "recursive wildcard";
  check (Normalize.program [] = Ok []) "empty program";
  let custom_bindings =
    ("+", External "custom_add") :: List.remove_assoc "+" default_bindings
  in
  let custom =
    match Normalize.expression ~bindings:custom_bindings (parse "1 + 2") with
    | Ok ir -> ir
    | Error _ -> failwith "custom bindings"
  in
  check
    (match custom with
     | Let (_, Value (External "custom_add"), Let (_, Apply _, Result (Apply _))) -> true
     | _ -> false)
    "explicit external override of a builtin";
  check (Check.term ~externals:[ "custom_add" ] custom = Ok ()) "external override scope";
  let program source =
    match Parser_menhir.parse_program source with
    | Ok p -> p
    | Error e -> failwith (Format.asprintf "%a" Error_monad.pp_error e)
  in
  let normalize_program p =
    match Normalize.program p with
    | Ok p -> p
    | Error e -> failwith (Format.asprintf "%a" Normalize.pp_error e)
  in
  let top = normalize_program (program "let x = 7\nlet x = x + 1\nlet answer = x") in
  check (Check.program ~externals:[] top = Ok ()) "top-level shadowing scope";
  check
    (match top with
     | [ Definition (x, _)
       ; Definition (y, Result (Binary_primitive (Add, Local previous, _)))
       ; Definition (_, Result (Value (Local latest)))
       ] -> x.stamp = previous.stamp && y.stamp = latest.stamp && x.stamp <> y.stamp
     | _ -> false)
    "ordered definitions";
  let cps =
    program
      "let rec fac n k = if n <= 1 then k 1 else fac (n - 1) (fun r -> k (n * r))\n\
       let rec fib n k = if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun \
       b -> k (a + b)))\n\
       let answer = fac 5 (fun x -> x)"
  in
  let cps_anf = normalize_program cps in
  check (Check.program ~externals:[] cps_anf = Ok ()) "CPS functions scope";
  check (normalize_program cps = cps_anf) "program repeatability";
  check
    (Normalize.program (program "let rec x = 1")
     = Error (Normalize.Recursive_non_function "x"))
    "top-level unsupported recursion";
  check
    (Normalize.program (program "let rec _ = fun x -> x")
     = Error Normalize.Recursive_wildcard)
    "top-level recursive wildcard";
  print_endline
    "ANF normalization: scope, tails, aliases, branches, primitives and recursion passed"
;;
