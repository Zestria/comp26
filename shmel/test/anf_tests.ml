[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib
open Shmel_middleend
module Gen = QCheck.Gen

(* Types of the variables visible to the generator. Programs are generated
   well-typed and terminating, so both the original and the ANF version can be
   evaluated and their results compared. *)
type ty =
  | Int
  | Bool
  | Fun1 (** [int -> int] *)
  | Fun2 (** [int -> int -> int] *)

(* A small pool makes shadowing frequent; [t0], [t1] and [x_1] look like the
   names ANF generates itself. *)
let names = [ "x"; "y"; "z"; "f"; "g"; "t0"; "t1"; "x_1" ]
let extend x ty env = (x, ty) :: List.remove_assoc x env

let vars ty env =
  List.filter_map
    (fun (x, t) ->
      match t, ty with
      | Int, Int | Bool, Bool | Fun1, Fun1 | Fun2, Fun2 -> Some x
      | _ -> None)
    env
;;

let name = Gen.oneof_list names

let two_names =
  let open Gen in
  let* a = name in
  let+ b = oneof_list (List.filter (fun n -> not (String.equal n a)) names) in
  a, b
;;

let leaf env =
  let open Gen in
  let const = map (fun n -> Ast.Const n) (int_range 0 20) in
  match vars Int env with
  | [] -> const
  | xs -> oneof [ const; map (fun x -> Ast.Var x) (oneof_list xs) ]
;;

let rec gen_int env size =
  let open Gen in
  let smaller env = gen_int env (size / 2) in
  let calls ty weight = if List.is_empty (vars ty env) then [] else [ weight, return ty ] in
  if size <= 0
  then leaf env
  else
    let* kind =
      oneof_weighted
        ([ 2, return `Leaf
         ; 4, return `Arith
         ; 1, return `Div
         ; 1, return `Neg
         ; 2, return `If
         ; 3, return `Let
         ; 2, return `Let_fun
         ; 1, return `Let_fun2
         ; 1, return `Immediate_call
         ; 1, return `Higher_order
         ; 1, return `Let_rec
         ; 2, return `Let_cond
         ; 1, return `Print
         ]
         @ List.map (fun (w, g) -> w, map (fun _ -> `Call1) g) (calls Fun1 3)
         @ List.map (fun (w, g) -> w, map (fun _ -> `Call2) g) (calls Fun2 3))
    in
    match kind with
    | `Leaf -> leaf env
    | `Arith ->
      map3
        (fun op a b -> Ast.BinOp (op, a, b))
        (oneof_list Ast.[ Add; Sub; Mul ])
        (smaller env)
        (smaller env)
    | `Div -> map2 (fun a b -> Ast.BinOp (Ast.Div, a, b)) (smaller env) (smaller env)
    | `Neg -> map (fun a -> Ast.Neg a) (smaller env)
    | `If ->
      map3
        (fun c t e -> Ast.If (c, t, e))
        (gen_cond env (size / 2))
        (smaller env)
        (smaller env)
    | `Let ->
      let* x = name in
      let* e1 = smaller env in
      let+ e2 = smaller (extend x Int env) in
      Ast.Let (x, e1, e2)
    | `Let_fun ->
      let* f = name in
      let* p = name in
      let* body = smaller (extend p Int env) in
      let+ e2 = smaller (extend f Fun1 env) in
      Ast.Let (f, Ast.Abs (p, body), e2)
    | `Let_fun2 ->
      let* f = name in
      let* a, b = two_names in
      let* body = smaller (extend b Int (extend a Int env)) in
      let+ e2 = smaller (extend f Fun2 env) in
      Ast.Let (f, Ast.Abs (a, Ast.Abs (b, body)), e2)
    | `Immediate_call ->
      let* p = name in
      let* body = smaller (extend p Int env) in
      let+ arg = smaller env in
      Ast.App (Ast.Abs (p, body), arg)
    | `Higher_order ->
      (* let h = fun g x -> g (g x) in h (fun p -> body) arg *)
      let* p = name in
      let* body = smaller (extend p Int env) in
      let+ arg = smaller env in
      let twice =
        Ast.Abs ("g", Ast.Abs ("x", Ast.App (Ast.Var "g", Ast.App (Ast.Var "g", Ast.Var "x"))))
      in
      Ast.Let ("h", twice, Ast.App (Ast.App (Ast.Var "h", Ast.Abs (p, body)), arg))
    | `Let_rec ->
      (* let rec f n = ... in f k + rest; [f] is not visible in [rest] *)
      let* f, n, body = gen_rec_fun env size in
      let* k = int_range 0 6 in
      let+ rest = gen_int (List.remove_assoc f env) (size / 4) in
      Ast.LetRec (f, n, body, Ast.BinOp (Ast.Add, Ast.App (Ast.Var f, Ast.Const k), rest))
    | `Print ->
      (* output shows the order of evaluation *)
      let* a = smaller env in
      let+ b = smaller env in
      Ast.Let ("_", Ast.App (Ast.Var "print_int", a), b)
    | `Let_cond ->
      (* let c = a < b in ... if c then ... ; [c] may be tested several times *)
      let* c = name in
      let* cond = gen_cond env (size / 2) in
      let env = extend c Bool env in
      let* t = smaller env in
      let+ e = smaller env in
      Ast.Let (c, cond, Ast.If (Ast.Var c, t, e))
    | `Call1 ->
      map2
        (fun f a -> Ast.App (Ast.Var f, a))
        (oneof_list (vars Fun1 env))
        (smaller env)
    | `Call2 ->
      map3
        (fun f a b -> Ast.App (Ast.App (Ast.Var f, a), b))
        (oneof_list (vars Fun2 env))
        (smaller env)
        (smaller env)

(* let rec f n = if n <= 0 then base else f (n - 1) op step; [f] is not
   visible in [base] and [step], so it terminates for any [n >= 0]. *)
and gen_rec_fun env size =
  let open Gen in
  let* f, n = two_names in
  let body_env = extend n Int (List.remove_assoc f env) in
  let* base = gen_int body_env (size / 4) in
  let* step = gen_int body_env (size / 4) in
  let+ op = oneof_list Ast.[ Add; Sub; Mul ] in
  let recursive_call = Ast.App (Ast.Var f, Ast.BinOp (Ast.Sub, Ast.Var n, Ast.Const 1)) in
  ( f
  , n
  , Ast.If
      ( Ast.BinOp (Ast.Le, Ast.Var n, Ast.Const 0)
      , base
      , Ast.BinOp (op, recursive_call, step) ) )

and gen_cond env size =
  let comparison =
    Gen.map3
      (fun op a b -> Ast.BinOp (op, a, b))
      (Gen.oneof_list Ast.[ Eq; Lt; Gt; Le; Ge ])
      (gen_int env size)
      (gen_int env size)
  in
  match vars Bool env with
  | [] -> comparison
  | cs -> Gen.oneof_weighted [ 2, comparison; 1, Gen.map (fun c -> Ast.Var c) (Gen.oneof_list cs) ]
;;

(* Top-level declarations; a declared name shadows the previous one. *)
let rec gen_decls env size count =
  let open Gen in
  let rest env = gen_decls env size (count - 1) in
  if count = 0
  then return []
  else
    let* kind =
      oneof_weighted
        [ 3, return `Value
        ; 2, return `Fun1
        ; 1, return `Fun2
        ; 2, return `Print
        ; 1, return `Rec
        ]
    in
    match kind with
    | `Value ->
      let* x = name in
      let* e = gen_int env size in
      let+ rest = rest (extend x Int env) in
      Ast.DLet (x, e) :: rest
    | `Fun1 ->
      let* f = name in
      let* p = name in
      let* body = gen_int (extend p Int env) size in
      let+ rest = rest (extend f Fun1 env) in
      Ast.DLet (f, Ast.Abs (p, body)) :: rest
    | `Fun2 ->
      let* f = name in
      let* a, b = two_names in
      let* body = gen_int (extend b Int (extend a Int env)) size in
      let+ rest = rest (extend f Fun2 env) in
      Ast.DLet (f, Ast.Abs (a, Ast.Abs (b, body))) :: rest
    | `Print ->
      let* e = gen_int env size in
      let+ rest = rest env in
      Ast.DLet ("_", Ast.App (Ast.Var "print_int", e)) :: rest
    | `Rec ->
      (* the recursive function is called once, with a small argument *)
      let* f, n, body = gen_rec_fun env size in
      let* x = name in
      let* k = int_range 0 6 in
      let+ rest = rest (extend x Int (List.remove_assoc f env)) in
      Ast.DLetRec (f, n, body) :: Ast.DLet (x, Ast.App (Ast.Var f, Ast.Const k)) :: rest
;;

(* A third of the programs are a single expression, the menhir parser can read
   only such programs. *)
let gen_program =
  let open Gen in
  let* size = int_range 1 20 in
  let expression = map (fun e -> [ Ast.DExpr e ]) (gen_int [] size) in
  let declarations =
    let* first = oneof [ return []; expression ] in
    let* count = int_range 0 5 in
    let+ decls = gen_decls [] (size / 2) count in
    first @ decls
  in
  oneof_weighted [ 1, expression; 2, declarations ]
;;

let program = QCheck.make ~print:Printast.show_program gen_program
let anf_text p = Format.asprintf "%a" Anf.pp_program (Anf.of_program p)

let pp_result ppf = function
  | Ok v -> Eval.pp_value ppf v
  | Error e -> Format.fprintf ppf "error: %a" Eval.pp_error e
;;

let pp_outcome ppf { Eval.output; result } =
  Format.fprintf ppf "output %S, result %a" output pp_result result
;;

(* Functions cannot be compared, their results are compared instead. *)
let same_value a b =
  match a, b with
  | Eval.VInt x, Eval.VInt y -> x = y
  | Eval.VBool x, Eval.VBool y -> Bool.equal x y
  | Eval.VUnit, Eval.VUnit -> true
  | (Eval.VClosure _ | Eval.VBuiltin _), (Eval.VClosure _ | Eval.VBuiltin _) -> true
  | _ -> false
;;

let same_outcome (a : Eval.outcome) (b : Eval.outcome) =
  String.equal a.output b.output
  &&
  match a.result, b.result with
  | Ok x, Ok y -> same_value x y
  | Error Eval.Division_by_zero, Error Eval.Division_by_zero -> true
  | _ -> false
;;

let parsers_for = function
  | [ Ast.DExpr _ ] ->
    [ "top-down", Main.parse_program_with_comb; "menhir", Main.parse_program_with_menhir ]
  | _ -> [ "top-down", Main.parse_program_with_comb ]
;;

let check_parsed ~name ~parsers source f =
  List.for_all
    (fun (parser_name, parse) ->
      match parse source with
      | Ok p -> f parser_name p
      | Error (`Parsing_error msg) ->
        QCheck.Test.fail_reportf
          "%s: the %s parser rejects the ANF output:\n%s\n%s"
          name
          parser_name
          source
          msg)
    parsers
;;

(* The main property: ANF prints the same and computes the same value. The ANF
   is printed and parsed back, so the printer and the parsers are checked along
   the way. *)
let preserves_semantics =
  QCheck.Test.make ~count:1000 ~name:"anf preserves output and result" program (fun p ->
    let expected = Eval.run p in
    let direct = Eval.run (Anf.to_program (Anf.of_program p)) in
    let text = anf_text p in
    (same_outcome expected direct
     || QCheck.Test.fail_reportf
          "expected %a,\ngot %a\nANF:\n%s"
          pp_outcome
          expected
          pp_outcome
          direct
          text)
    && check_parsed ~name:"semantics" ~parsers:(parsers_for p) text (fun parser_name anf ->
      let actual = Eval.run anf in
      same_outcome expected actual
      || QCheck.Test.fail_reportf
           "%s parser: expected %a,\ngot %a\nANF:\n%s"
           parser_name
           pp_outcome
           expected
           pp_outcome
           actual
           text))
;;

let idempotent =
  QCheck.Test.make ~count:1000 ~name:"anf of anf changes nothing" program (fun p ->
    let text = anf_text p in
    check_parsed ~name:"idempotence" ~parsers:(parsers_for p) text (fun parser_name anf ->
      let again = anf_text anf in
      String.equal text again
      || QCheck.Test.fail_reportf "%s parser:\nfirst:\n%s\nsecond:\n%s" parser_name text again))
;;

let bodies program =
  List.map
    (function
      | Anf.DLet (_, _, body) | Anf.DLetRec (_, _, body) | Anf.DExpr body -> body)
    program
;;

(* Keeping ANF small: an immediate is never bound to a name. *)
let rec no_bound_immediates = function
  | Anf.AExpr c -> cexpr_ok c
  | Anf.ALet (_, Anf.CImm _, _) -> false
  | Anf.ALet (_, c, body) -> cexpr_ok c && no_bound_immediates body
  | Anf.ALetRec (_, _, fbody, body) -> no_bound_immediates fbody && no_bound_immediates body

and cexpr_ok = function
  | Anf.CIf (_, t, e) -> no_bound_immediates t && no_bound_immediates e
  | Anf.CFun (_, body) -> no_bound_immediates body
  | Anf.CImm _ | Anf.CNeg _ | Anf.CBinOp _ | Anf.CApp _ -> true
;;

let no_immediate_lets =
  QCheck.Test.make ~count:1000 ~name:"anf never binds an immediate" program (fun p ->
    List.for_all no_bound_immediates (bodies (Anf.of_program p)))
;;

(* ANF names are unique, so shadowing can be ignored. *)
let rec occurrences x = function
  | Ast.Var y -> if String.equal x y then 1 else 0
  | Ast.Const _ -> 0
  | Ast.Neg e | Ast.Abs (_, e) -> occurrences x e
  | Ast.BinOp (_, a, b) | Ast.App (a, b) | Ast.Let (_, a, b) | Ast.LetRec (_, _, a, b) ->
    occurrences x a + occurrences x b
  | Ast.If (c, t, e) -> occurrences x c + occurrences x t + occurrences x e
;;

let is_comparison = function
  | Ast.Eq | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge -> true
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> false
;;

(* Keeping ANF small: a comparison is not bound to a name only to be tested by
   the next [if], it is tested directly: [if a < b then ...]. *)
let rec no_bound_conditions = function
  | Anf.AExpr c -> cond_ok c
  | Anf.ALet (x, Anf.CBinOp (op, _, _), body) when is_comparison op ->
    let tested_next =
      match body with
      | Anf.AExpr (Anf.CIf (Anf.CondImm (Anf.ImmVar y), _, _))
      | Anf.ALet (_, Anf.CIf (Anf.CondImm (Anf.ImmVar y), _, _), _) -> String.equal x y
      | _ -> false
    in
    (not (tested_next && occurrences x (Anf.to_expr body) = 1)) && no_bound_conditions body
  | Anf.ALet (_, c, body) -> cond_ok c && no_bound_conditions body
  | Anf.ALetRec (_, _, fbody, body) -> no_bound_conditions fbody && no_bound_conditions body

and cond_ok = function
  | Anf.CIf (_, t, e) -> no_bound_conditions t && no_bound_conditions e
  | Anf.CFun (_, body) -> no_bound_conditions body
  | Anf.CImm _ | Anf.CNeg _ | Anf.CBinOp _ | Anf.CApp _ -> true
;;

let no_condition_lets =
  QCheck.Test.make ~count:1000 ~name:"anf tests comparisons directly" program (fun p ->
    List.for_all no_bound_conditions (bodies (Anf.of_program p)))
;;

(* The CPS tests of the course: manytests/typed/012faccps.ml, 012fibcps.ml,
   010faccps_ll.ml and 010fibcps_ll.ml. *)
let fac_cps =
  {|let rec fac n k =
  if n < 2
  then k 1
  else fac (n - 1) (fun a -> k (a * n))

let main = print_int (fac 6 (fun x -> x))|}
;;

let fib_cps =
  {|let rec fib n k =
  if n < 2
  then k n
  else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))

let main = print_int (fib 6 (fun x -> x))|}
;;

let fac_cps_ll =
  {|let id x = x
let fresh_1 n k p = k (p * n)

let rec fac_cps n k =
  if n = 1
  then k 1
  else fac_cps (n-1) (fresh_1 n k)

let main =
  let _ = print_int (fac_cps 4 id) in
  0|}
;;

let fib_cps_ll =
  {|let id x = x
let fresh_2 p1 k p2 =
  k (p1 + p2)

let fresh_1 n k fib p1 =
  fib (n-2) (fresh_2 p1 k)

let rec fib n k =
  if n < 2
  then k n
  else fib (n - 1) (fresh_1 n k fib)

let main =
  let z = print_int (fib 6 id)  in
  0|}
;;

let fac_expression =
  "print_int (let rec fac n k = if n <= 1 then k 1 else fac (n - 1) (fun x -> k (n * x)) in \
   fac 5 (fun x -> x))"
;;

let fib_expression =
  "print_int (let rec fib n k = if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) \
   (fun b -> k (a + b))) in fib 10 (fun x -> x))"
;;

let prints name source expected =
  QCheck.Test.make ~count:1 ~name QCheck.unit (fun () ->
    match Main.parse_program_with_comb source with
    | Error (`Parsing_error msg) -> QCheck.Test.fail_reportf "does not parse: %s" msg
    | Ok p ->
      let before = Eval.run p in
      let after = Eval.run (Result.get_ok (Main.parse_program_with_comb (anf_text p))) in
      (String.equal before.output expected && same_outcome before after)
      || QCheck.Test.fail_reportf
           "expected output %S, got %a before and %a after ANF"
           expected
           pp_outcome
           before
           pp_outcome
           after)
;;

let () =
  QCheck_base_runner.run_tests_main
    [ prints "012faccps: CPS factorial 6 = 720" fac_cps "720"
    ; prints "012fibcps: CPS fibonacci 6 = 8" fib_cps "8"
    ; prints "010faccps_ll: lambda lifted CPS factorial 4 = 24" fac_cps_ll "24"
    ; prints "010fibcps_ll: lambda lifted CPS fibonacci 6 = 8" fib_cps_ll "8"
    ; prints "CPS factorial 5 = 120 as one expression" fac_expression "120"
    ; prints "CPS fibonacci 10 = 55 as one expression" fib_expression "55"
    ; preserves_semantics
    ; idempotent
    ; no_immediate_lets
    ; no_condition_lets
    ]
;;
