[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** QCheck round-trip tests for BOTH MyLittleML parsers.

    Two generated properties check that printing a program with {!Frontend.Pp}
    and parsing it back with either the top-down or the bottom-up parser
    yields exactly the same {!Frontend.Ast.prog}.

    The AST is mirrored in this module so that [ppx_deriving_qcheck] can
    attach [@gen] overrides (valid identifiers, bounded non-negative
    integers, non-empty [fun] parameter lists) without polluting the
    production [Frontend.Ast] with test-only annotations.

    Targeted assertions cover the CPS factorial / fibonacci programs, the
    precedence ladder and a few error cases.  Sticky-mode demos live in a
    separate commit ({!Parse.opts}.{!Parse.sticky}). *)

open Mylittleml.Frontend

(* Generators ---------------------------------------------------------- *)

(** Identifiers valid in both grammars: plain names from a small pool or
    numbered ["v<i>"] names.  Never empty, never a keyword. *)
let id_gen : string QCheck.Gen.t =
  let open QCheck.Gen in
  oneof
    [ oneof_list [ "x"; "y"; "z"; "n"; "k"; "f"; "g"; "acc"; "cont"; "r" ]
    ; map (fun i -> "v" ^ string_of_int i) (int_bound 1_000_000)
    ]
;;

(** Parameter lists of [let] bindings may be empty ([let x = e]).

    Lengths are bounded explicitly: the derived generators are built on
    {!QCheck.Gen.list}, whose length follows QCheck's *ambient* size (a
    random value that can reach several thousands), which would produce
    megabytes of text and effectively hang the test. *)
let id_list_gen = QCheck.Gen.(list_size (int_bound 3) id_gen)

(** [fun] requires at least one parameter, so its list must never be empty. *)
let nonempty_id_list_gen = QCheck.Gen.(list_size (int_range 1 4) id_gen)

(** The grammar has no negative literals ([-5] is negation) and the lexers
    would overflow on huge numbers, so integers are small and non-negative. *)
let int_gen = QCheck.Gen.int_bound 1000

(* Generated AST (mirror of Frontend.Ast) ------------------------------- *)

(** The [bind] record of {!Frontend.Ast} is inlined into the [Let] constructor
    as a tuple: ppx_deriving_qcheck honours [@gen] on constructor arguments
    but silently ignores it on record fields, and identifiers must come from
    {!id_gen} to stay parsable. *)
module GenAst = struct
  type bop =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Neq
  [@@deriving qcheck]

  (* [Let (rec_, name, params, body, cont)] mirrors
     [Let { bind; cont }] from the production AST. *)
  type expr =
    | Int of (int[@gen int_gen])
    | Bool of bool
    | Var of (string[@gen id_gen])
    | App of expr * expr
    | Fun of (string list[@gen nonempty_id_list_gen]) * expr
    | Let of
        bool
        * (string[@gen id_gen])
        * (string list[@gen id_list_gen])
        * expr
        * expr option
    | If of expr * expr * expr
    | Binop of bop * expr * expr
    | Neg of expr

  and prog = expr list [@@deriving qcheck]
end

(* Conversion into the production AST ---------------------------------- *)

let to_bop = function
  | GenAst.Add -> Ast.Add
  | GenAst.Sub -> Ast.Sub
  | GenAst.Mul -> Ast.Mul
  | GenAst.Div -> Ast.Div
  | GenAst.Mod -> Ast.Mod
  | GenAst.Lt -> Ast.Lt
  | GenAst.Le -> Ast.Le
  | GenAst.Gt -> Ast.Gt
  | GenAst.Ge -> Ast.Ge
  | GenAst.Eq -> Ast.Eq
  | GenAst.Neq -> Ast.Neq
;;

let rec to_expr = function
  | GenAst.Int n -> Ast.Int n
  | GenAst.Bool b -> Ast.Bool b
  | GenAst.Var x -> Ast.Var x
  | GenAst.App (f, x) -> Ast.App (to_expr f, to_expr x)
  | GenAst.Fun (ps, body) -> Ast.Fun (ps, to_expr body)
  | GenAst.Let (rec_, name, params, body, cont) ->
    Ast.Let ({ Ast.rec_; name; params; body = to_expr body }, Option.map to_expr cont)
  | GenAst.If (c, t, e) -> Ast.If (to_expr c, to_expr t, to_expr e)
  | GenAst.Binop (op, l, r) -> Ast.Binop (to_bop op, to_expr l, to_expr r)
  | GenAst.Neg e -> Ast.Neg (to_expr e)
;;

let to_prog = List.map to_expr

(* Round-trip properties ------------------------------------------------ *)

(** Top-level generator with all sizes fixed explicitly: expressions come
    from the derived sized generator with a constant fuel (its recursive
    calls halve the fuel and never consult the ambient size), and the
    program length is bounded by {!QCheck.Gen.list_size}.  This keeps every
    printed program small no matter which ambient size QCheck picks. *)
let gen_prog_small =
  QCheck.Gen.list_size (QCheck.Gen.int_bound 4) (GenAst.gen_expr_sized 6)
;;

let arbitrary_prog = QCheck.make gen_prog_small ~print:(fun p -> Pp.to_string (to_prog p))

let roundtrip kind prog =
  let opts = { Parse.sticky = false } in
  let ast = to_prog prog in
  match Parse.parse kind opts (Pp.to_string ast) with
  | Ok parsed -> parsed = ast
  | Error _ -> false
;;

(* Targeted assertions -------------------------------------------------- *)

let kind_name = function
  | Parse.Topdown -> "topdown"
  | Parse.Bottomup -> "bottomup"
;;

let expect_parses ~sticky name kind source expected =
  let opts = { Parse.sticky } in
  match Parse.parse kind opts source with
  | Ok prog when prog = expected -> ()
  | Ok prog ->
    QCheck.Test.fail_report
      (Format.sprintf
         "[%s] %s: unexpected AST\n  got:      %s\n  expected: %s"
         (kind_name kind)
         name
         (Ast.show_prog prog)
         (Ast.show_prog expected))
  | Error msg ->
    QCheck.Test.fail_report
      (Format.sprintf "[%s] %s: unexpected parse error: %s" (kind_name kind) name msg)
;;

let expect_error ~sticky name kind source =
  let opts = { Parse.sticky } in
  match Parse.parse kind opts source with
  | Error _ -> ()
  | Ok _ ->
    QCheck.Test.fail_report
      (Format.sprintf "[%s] %s: expected a parse error" (kind_name kind) name)
;;

let expect_specific_error ~sticky name kind source expected_msg =
  let opts = { Parse.sticky } in
  match Parse.parse kind opts source with
  | Error msg when msg = expected_msg -> ()
  | Error msg ->
    QCheck.Test.fail_report
      (Format.sprintf
         "[%s] %s: unexpected error message: got %S, expected %S"
         (kind_name kind)
         name
         msg
         expected_msg)
  | Ok _ ->
    QCheck.Test.fail_report
      (Format.sprintf "[%s] %s: expected a parse error" (kind_name kind) name)
;;

(** Targeted assertions run through QCheck so that failures (exceptions) are
    reported uniformly by the runner. *)
let assert_test name f =
  QCheck.Test.make ~name (QCheck.make QCheck.Gen.unit) (fun () ->
    f ();
    true)
;;

(* CPS factorial and fibonacci ----------------------------------------- *)

let fact_src =
  "let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))"
;;

let fact_ast =
  [ Ast.Let
      ( { Ast.rec_ = true
        ; name = "fact"
        ; params = [ "n"; "k" ]
        ; body =
            Ast.If
              ( Ast.Binop (Ast.Le, Ast.Var "n", Ast.Int 1)
              , Ast.App (Ast.Var "k", Ast.Int 1)
              , Ast.App
                  ( Ast.App (Ast.Var "fact", Ast.Binop (Ast.Sub, Ast.Var "n", Ast.Int 1))
                  , Ast.Fun
                      ( [ "r" ]
                      , Ast.App
                          (Ast.Var "k", Ast.Binop (Ast.Mul, Ast.Var "r", Ast.Var "n")) )
                  ) )
        }
      , None )
  ]
;;

let fib_src =
  "let rec fib n k = if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b -> \
   k (a + b)))"
;;

let fib_ast =
  [ Ast.Let
      ( { Ast.rec_ = true
        ; name = "fib"
        ; params = [ "n"; "k" ]
        ; body =
            Ast.If
              ( Ast.Binop (Ast.Le, Ast.Var "n", Ast.Int 1)
              , Ast.App (Ast.Var "k", Ast.Var "n")
              , Ast.App
                  ( Ast.App (Ast.Var "fib", Ast.Binop (Ast.Sub, Ast.Var "n", Ast.Int 1))
                  , Ast.Fun
                      ( [ "a" ]
                      , Ast.App
                          ( Ast.App
                              (Ast.Var "fib", Ast.Binop (Ast.Sub, Ast.Var "n", Ast.Int 2))
                          , Ast.Fun
                              ( [ "b" ]
                              , Ast.App
                                  ( Ast.Var "k"
                                  , Ast.Binop (Ast.Add, Ast.Var "a", Ast.Var "b") ) ) ) )
                  ) )
        }
      , None )
  ]
;;

(* Precedence ------------------------------------------------------------ *)

let precedence_cases =
  [ ( "add over mul"
    , "1+2*3"
    , [ Ast.Binop (Ast.Add, Ast.Int 1, Ast.Binop (Ast.Mul, Ast.Int 2, Ast.Int 3)) ] )
  ; ( "mul then add"
    , "2*3+4"
    , [ Ast.Binop (Ast.Add, Ast.Binop (Ast.Mul, Ast.Int 2, Ast.Int 3), Ast.Int 4) ] )
  ; ( "application binds tighter than add"
    , "f x + g y"
    , [ Ast.Binop
          (Ast.Add, Ast.App (Ast.Var "f", Ast.Var "x"), Ast.App (Ast.Var "g", Ast.Var "y"))
      ] )
  ; ( "if-then-else"
    , "if a then b else c"
    , [ Ast.If (Ast.Var "a", Ast.Var "b", Ast.Var "c") ] )
  ]
;;

(* Errors ---------------------------------------------------------------- *)

let error_sources =
  [ "unclosed paren", "(1 + 2"
  ; "illegal character", "let x = 1 @"
  ; "incomplete expression", "1 +"
  ]
;;

(* Sticky mode demos ----------------------------------------------------- *)

let sticky_tests =
  [ assert_test "sticky letx = 1 (topdown)" (fun () ->
      expect_parses
        ~sticky:true
        "sticky letx = 1"
        Parse.Topdown
        "letx = 1"
        [ Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = Ast.Int 1 }, None)
        ])
  ; assert_test "sticky letrec (topdown)" (fun () ->
      expect_parses
        ~sticky:true
        "sticky letrec"
        Parse.Topdown
        "letrec f n = n + 1"
        [ Ast.Let
            ( { Ast.rec_ = true
              ; name = "f"
              ; params = [ "n" ]
              ; body = Ast.Binop (Ast.Add, Ast.Var "n", Ast.Int 1)
              }
            , None )
        ])
  ; assert_test "sticky funx->x+1 (topdown)" (fun () ->
      expect_parses
        ~sticky:true
        "sticky funx"
        Parse.Topdown
        "funx->x+1"
        [ Ast.Fun ([ "x" ], Ast.Binop (Ast.Add, Ast.Var "x", Ast.Int 1)) ])
  ; assert_test "sticky letter = 1 (topdown, documented limitation)" (fun () ->
      expect_parses
        ~sticky:true
        "sticky letter = 1"
        Parse.Topdown
        "letter = 1"
        [ Ast.Let ({ Ast.rec_ = false; name = "ter"; params = []; body = Ast.Int 1 }, None)
        ])
  ; assert_test "non-sticky letx stays an identifier (topdown)" (fun () ->
      expect_parses
        ~sticky:false
        "non-sticky letx"
        Parse.Topdown
        "letx = 1"
        [ Ast.Binop (Ast.Eq, Ast.Var "letx", Ast.Int 1) ])
  ; assert_test "bottomup rejects --sticky" (fun () ->
      expect_specific_error
        ~sticky:true
        "bottomup sticky"
        Parse.Bottomup
        "letx = 1"
        "sticky mode is not supported by the bottom-up parser")
  ]
;;

(* Test suite ------------------------------------------------------------ *)

let cps_tests =
  [ assert_test "cps factorial (topdown)" (fun () ->
      expect_parses ~sticky:false "cps factorial" Parse.Topdown fact_src fact_ast)
  ; assert_test "cps factorial (bottomup)" (fun () ->
      expect_parses ~sticky:false "cps factorial" Parse.Bottomup fact_src fact_ast)
  ; assert_test "cps fibonacci (topdown)" (fun () ->
      expect_parses ~sticky:false "cps fibonacci" Parse.Topdown fib_src fib_ast)
  ; assert_test "cps fibonacci (bottomup)" (fun () ->
      expect_parses ~sticky:false "cps fibonacci" Parse.Bottomup fib_src fib_ast)
  ]
;;

let precedence_tests =
  List.concat_map
    (fun (name, src, ast) ->
       [ assert_test (name ^ " (topdown)") (fun () ->
           expect_parses ~sticky:false name Parse.Topdown src ast)
       ; assert_test (name ^ " (bottomup)") (fun () ->
           expect_parses ~sticky:false name Parse.Bottomup src ast)
       ])
    precedence_cases
;;

let error_tests =
  List.concat_map
    (fun (name, src) ->
       [ assert_test (name ^ " (topdown)") (fun () ->
           expect_error ~sticky:false name Parse.Topdown src)
       ; assert_test (name ^ " (bottomup)") (fun () ->
           expect_error ~sticky:false name Parse.Bottomup src)
       ])
    error_sources
;;

let () =
  exit
    (QCheck_runner.run_tests
       ~colors:false
       ([ QCheck.Test.make
            ~name:"roundtrip topdown"
            ~count:200
            arbitrary_prog
            (roundtrip Parse.Topdown)
        ; QCheck.Test.make
            ~name:"roundtrip bottomup"
            ~count:200
            arbitrary_prog
            (roundtrip Parse.Bottomup)
        ]
        @ cps_tests
        @ precedence_tests
        @ error_tests
        @ sticky_tests))
;;
