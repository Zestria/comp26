open Middleend
open Anf_test_eval

let expression text =
  match Parser_menhir.parse_expression text with
  | Ok e -> e
  | Error e -> failwith (Format.asprintf "%a" Error_monad.pp_error e)
;;

let program text =
  match Parser_menhir.parse_program text with
  | Ok p -> p
  | Error e -> failwith (Format.asprintf "%a" Error_monad.pp_error e)
;;

let normalized e =
  match Normalize.expression e with
  | Ok e -> e
  | Error e -> failwith (Format.asprintf "%a" Normalize.pp_error e)
;;

let check text expected =
  let ast = expression text in
  let ir = normalized ast in
  let before = source ast in
  let after = anf ir in
  if before <> expected || not (equivalent before after)
  then
    failwith
      (Printf.sprintf
         "%s\nAST: %s\nANF: %s\nIR: %s"
         text
         (show_observation before)
         (show_observation after)
         (Pretty_printer.show_term ir))
;;

let ok value trace = { result = Ok value; trace }
let integer n trace = ok (Ast.Integer n) trace
let boolean b trace = ok (Ast.Boolean b) trace
let runtime error trace = { result = Error (Runtime error); trace }

let check_program text expected =
  let ast = program text in
  let ir =
    match Normalize.program ast with
    | Ok p -> p
    | Error e -> failwith (Format.asprintf "%a" Normalize.pp_error e)
  in
  if Check.program ~externals:[ "print_int" ] ir <> Ok () then failwith "program scope";
  if Normalize.program ast <> Ok ir then failwith "program determinism";
  let before = source_program ast in
  let after = anf_program ir in
  if before <> expected || not (equivalent before after)
  then
    failwith
      (Printf.sprintf
         "%s\nAST: %s\nANF: %s"
         text
         (show_observation before)
         (show_observation after))
;;

let () =
  check "(5 + 4) - 2" (integer 7 []);
  check "let _ = print_int 1 in 42" (integer 42 [ 1 ]);
  check "1 / 0" (runtime "division by zero" []);
  check
    "(let _ = print_int 1 in fun x -> let _ = print_int 3 in x) (let _ = print_int 2 in \
     42)"
    (integer 42 [ 1; 2; 3 ]);
  check
    "let f x = let _ = print_int 1 in fun y -> let _ = print_int 3 in x + y in f 10 (let \
     _ = print_int 2 in 20)"
    (integer 30 [ 1; 2; 3 ]);
  check "let f x = let _ = print_int 1 in x in 42" (integer 42 []);
  check "let f x = let _ = print_int 1 in x in f 42" (integer 42 [ 1 ]);
  check "let x = 7 in (let x = 2 in x) + x" (integer 9 []);
  check "let x = 7 in let f y = x + y in let x = 2 in f 1" (integer 8 []);
  check
    "let h x = let _ = print_int 3 in x in h (if true then let _ = print_int 1 in 10 \
     else let _ = print_int 2 in 20)"
    (integer 10 [ 1; 3 ]);
  check
    "let h x = let _ = print_int 3 in x in h (if false then let _ = print_int 1 in 10 \
     else let _ = print_int 2 in 20)"
    (integer 20 [ 2; 3 ]);
  check "false && (1 / 0 = 0)" (boolean false []);
  check "true || (1 / 0 = 0)" (boolean true []);
  check "true && (let _ = print_int 1 in false)" (boolean false [ 1 ]);
  check "false || (let _ = print_int 1 in true)" (boolean true [ 1 ]);
  check
    "(let _ = print_int 1 in false) && (let _ = print_int 2 in true)"
    (boolean false [ 1 ]);
  check
    "(let _ = print_int 1 in true) || (let _ = print_int 2 in false)"
    (boolean true [ 1 ]);
  check "let both = (&&) in both false (1 / 0 = 0)" (boolean false []);
  check "let either = (||) in either true (1 / 0 = 0)" (boolean true []);
  check "let (+) x y = x in 1 + 2" (integer 1 []);
  check "let (&&) x y = x in false && (let _ = print_int 1 in true)" (boolean false [ 1 ]);
  check "let (||) x y = y in true || (let _ = print_int 1 in false)" (boolean false [ 1 ]);
  check
    "let both = (&&) false in both (let _ = print_int 1 in true)"
    (boolean false [ 1 ]);
  check "(fun op -> op false (let _ = print_int 1 in true)) (&&)" (boolean false [ 1 ]);
  check "let add = (+) 1 in add 2" (integer 3 []);
  check "missing (let _ = print_int 1 in 0)" (runtime "unbound: missing" []);
  check "let _ = missing in 42" (runtime "unbound: missing" []);
  check "let _ = 1 / 0 in 42" (runtime "division by zero" []);
  check "let rec loop x = loop x in false && loop 0" (boolean false []);
  check "let rec loop x = loop x in true || loop 0" (boolean true []);
  check "let rec f x = if x = 0 then 0 else f (x - 1) in f 3" (integer 0 []);
  check_program "let x = 7\nlet x = x + 1\nlet answer = x" (integer 8 []);
  check_program
    "let _ = print_int 1\nlet _ = print_int 2\nlet answer = 42"
    (integer 42 [ 1; 2 ]);
  check_program "let x = 7\nlet f y = x + y\nlet x = 2\nlet answer = f 1" (integer 8 []);
  check_program "let both = (&&)\nlet answer = both false (1 / 0 = 0)" (boolean false []);
  check_program
    "let both = let _ = print_int 1 in (&&)\nlet answer = both false (1 / 0 = 0)"
    (boolean false [ 1 ]);
  let factorial =
    "let rec fac n k = if n <= 1 then k 1 else fac (n - 1) (fun r -> k (n * r))\n\
     let answer = fac 5 (fun x -> x)"
  in
  let fibonacci =
    "let rec fib n k = if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
     -> k (a + b)))\n\
     let answer = fib 10 (fun x -> x)"
  in
  check_program factorial (integer 120 []);
  check_program fibonacci (integer 55 []);
  let infinite = expression "let rec loop x = loop x in loop 0" in
  let before = source ~fuel:40 infinite in
  let after = anf ~fuel:40 (normalized infinite) in
  if
    before.result <> Error Fuel_exhausted
    || after.result <> Error Fuel_exhausted
    || equivalent before after
  then failwith "fuel must not imply success";
  print_endline "ANF oracles: fixed values, trace, errors and fuel passed"
;;
