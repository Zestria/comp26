open Middleend
open Anf

let id stamp hint = { stamp; hint }
let x = id 0 "x"
let t = id 1 "t"
let fn = id 2 "f"
let value v = Result (Value v)
let check condition message = if not condition then failwith message
let valid term = check (Check.term ~externals:[ "f"; "g"; "x" ] term = Ok ()) "scope"

let () =
  let nested =
    Let (t, Apply (External "g", External "x"), Result (Apply (External "f", Local t)))
  in
  valid nested;
  valid (Result (Lambda (x, value (Local x))));
  valid (Let_rec (fn, x, Result (Apply (Local fn, Local x)), value (Local fn)));
  valid (Result (If (Constant (Ast.Boolean true), nested, value (Constant Ast.Unit))));
  check
    (Check.program
       ~externals:[]
       [ Definition (x, value (Constant (Ast.Integer 1)))
       ; Definition (t, value (Local x))
       ]
     = Ok ())
    "program scope";
  check
    (Check.term ~externals:[] (value (Local x)) = Error (Check.Unbound_local x))
    "unbound local";
  check
    (Check.term ~externals:[] (value (External "missing"))
     = Error (Check.Unbound_external "missing"))
    "unbound external";
  check
    (Check.term ~externals:[] (Let (x, Value (Local x), value (Local x)))
     = Error (Check.Unbound_local x))
    "nonrecursive rhs";
  check
    (Check.term
       ~externals:[]
       (Let (x, Value (Constant Ast.Unit), Result (Lambda (x, value (Local x)))))
     = Error (Check.Duplicate_binder x))
    "duplicate";
  check
    (Check.term ~externals:[] (Let_rec (fn, x, value (Local x), value (Local x)))
     = Error (Check.Unbound_local x))
    "parameter escapes";
  let sibling = Result (Lambda (x, value (Local x))) in
  check
    (Check.term
       ~externals:[]
       (Result (If (Constant (Ast.Boolean true), sibling, sibling)))
     = Error (Check.Duplicate_binder x))
    "duplicate IDs in disjoint branches";
  let printed = Pretty_printer.show_term nested in
  check
    (printed
     = "let t#1 = apply external(\"g\") external(\"x\") in\napply external(\"f\") t#1")
    printed;
  check
    (Pretty_printer.show_term (Result (Lambda (x, value (Local x))))
     = "(fun x#0 ->\n  x#0)")
    "lambda snapshot";
  check (Pretty_printer.show_term nested = printed) "deterministic printer";
  let other_x = id 3 "x" in
  check
    (Format.asprintf "%a %a" Pretty_printer.pp_id x Pretty_printer.pp_id other_x
     = "x#0 x#3")
    "shadowed names";
  print_endline "ANF IR: constructors, scope and printer passed"
;;
