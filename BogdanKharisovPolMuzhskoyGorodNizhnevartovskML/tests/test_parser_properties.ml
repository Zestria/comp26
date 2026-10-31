[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend
open Ast

module Make (Parser : sig
    val parse_expression : string -> (Ast.expression, string) result
    val parse_program : string -> (Ast.program, string) result
    val name : string
  end) =
struct
  let test ~name = QCheck.Test.make ~name:(Parser.name ^ ": " ^ name)
  let name = Test_support.identifier
  let integer text = Constant (Int (Test_support.integer_literal text))

  let name_source name =
    let text = Name.to_string name in
    match Name.of_identifier text with
    | Ok _ -> text
    | Error _ -> "( " ^ text ^ " )"
  ;;

  let rec pattern_source = function
    | PVar name -> name_source name
    | PWildcard -> "_"
    | PUnit -> "()"
    | PTuple (a, b, rest) ->
      "(" ^ String.concat ", " (List.map pattern_source (a :: b :: rest)) ^ ")"
  ;;

  let rec source = function
    | Constant (Int literal) -> "(" ^ Integer_literal.to_string literal ^ ")"
    | Constant (Bool value) -> "(" ^ string_of_bool value ^ ")"
    | Constant Unit -> "(())"
    | Var name -> name_source name
    | Tuple (a, b, rest) ->
      "(" ^ String.concat ", " (List.map source (a :: b :: rest)) ^ ")"
    | Apply (f, a, rest) ->
      "(" ^ String.concat " " (List.map source (f :: a :: rest)) ^ ")"
    | Fun { parameter; parameters; body } ->
      "(fun "
      ^ String.concat " " (List.map pattern_source (parameter :: parameters))
      ^ " -> "
      ^ source body
      ^ ")"
    | Let (binding, body) -> "(" ^ binding_source binding ^ " in " ^ source body ^ ")"
    | If (condition, yes, no) ->
      "(if "
      ^ source condition
      ^ " then "
      ^ source yes
      ^ (match no with
         | None -> ""
         | Some no -> " else " ^ source no)
      ^ ")"

  and binding_source { recursion; pattern; expression } =
    "let "
    ^ (match recursion with
       | Nonrecursive -> ""
       | Recursive -> "rec ")
    ^ pattern_source pattern
    ^ " = "
    ^ source expression
  ;;

  let patterns =
    [ PVar (name "x"); PWildcard; PUnit; PTuple (PVar (name "x"), PWildcard, [ PUnit ]) ]
  ;;

  let atoms =
    [ integer "0"
    ; integer "-1"
    ; integer "00__2_"
    ; Constant (Bool true)
    ; Constant (Bool false)
    ; Constant Unit
    ; Var (name "x")
    ; Var (name "class")
    ; Var (name "f")
    ]
  ;;

  let rec expression_gen depth =
    let open QCheck.Gen in
    let ( let* ) = ( >>= ) in
    let atom = oneof_list atoms in
    if depth = 0
    then atom
    else (
      let child = expression_gen (depth - 1) in
      oneof_weighted
        [ 3, atom
        ; ( 1
          , let* a = child in
            let* b = child in
            let* rest = list_small child in
            return (Tuple (a, b, rest)) )
        ; ( 1
          , let* f = child in
            let* a = child in
            let* rest = list_small child in
            return (Apply (f, a, rest)) )
        ; ( 1
          , let* parameter = oneof_list patterns in
            let* parameters = list_small (oneof_list patterns) in
            let* body = child in
            return (Fun { parameter; parameters; body }) )
        ; ( 1
          , let* pattern = oneof_list patterns in
            let* expression = child in
            let* body = child in
            let* recursion = oneof_list [ Nonrecursive; Recursive ] in
            return (Let ({ recursion; pattern; expression }, body)) )
        ; ( 1
          , let* condition = child in
            let* yes = child in
            let* no = option child in
            return (If (condition, yes, no)) )
        ])
  ;;

  let shrink = function
    | Constant _ | Var _ -> QCheck.Iter.empty
    | Tuple (a, b, rest) -> QCheck.Iter.of_list (a :: b :: rest)
    | Apply (f, a, rest) -> QCheck.Iter.of_list (f :: a :: rest)
    | Fun { body; _ } -> QCheck.Iter.return body
    | Let ({ expression; _ }, body) -> QCheck.Iter.of_list [ expression; body ]
    | If (condition, yes, no) ->
      QCheck.Iter.of_list (condition :: yes :: Option.to_list no)
  ;;

  let arbitrary =
    QCheck.make
      ~print:source
      ~shrink
      QCheck.Gen.(sized (fun size -> expression_gen (min 3 size)))
  ;;

  let roundtrip =
    test
      ~name:"parenthesized source preserves expression AST"
      ~count:1000
      arbitrary
      (fun expected ->
         match Parser.parse_expression (source expected) with
         | Ok actual when actual = expected -> true
         | Ok actual ->
           QCheck.Test.fail_reportf
             "Expected: %s\nActual: %s"
             (source expected)
             (source actual)
         | Error error -> QCheck.Test.fail_reportf "Unexpected parse error: %s" error)
  ;;

  let operator_names =
    let open QCheck.Gen in
    let ( let* ) = ( >>= ) in
    let symbolic starts alphabet =
      let* first = oneof_list starts in
      let* rest = QCheck.get_gen (Test_support.strings alphabet) in
      return (first ^ rest)
    in
    let generator =
      oneof
        [ oneof_list [ "mod"; ":=" ]
        ; symbolic [ "!"; "~!"; "?!"; "#!" ] "!$%&*+-./:<=>?@^|~#"
        ; symbolic
            [ "="; "<>"; "|+"; "&"; "$"; "@"; "^"; "+"; "-+"; "*"; "/"; "%" ]
            "!$%&*+-./:<=>?@^|~"
        ]
    in
    QCheck.make ~print:Test_support.print_string ~shrink:QCheck.Shrink.string generator
    |> QCheck.add_shrink_invariant (fun text -> Result.is_ok (Name.of_operator text))
  ;;

  let operator_values =
    Test_support.property
      (Parser.name ^ ": validated operator names parse as parenthesized values")
      operator_names
      (fun text ->
         let expected = Test_support.operator text in
         match Parser.parse_expression ("( " ^ text ^ " )") with
         | Ok (Var actual) when actual = expected -> true
         | Ok actual ->
           QCheck.Test.fail_reportf "Expected operator value; got %s" (source actual)
         | Error error -> QCheck.Test.fail_reportf "Unexpected parse error: %s" error)
  ;;

  let infix_precedence =
    let operators =
      [ ":=", 0, `Right
      ; "||", 1, `Right
      ; "&&", 2, `Right
      ; "=", 3, `Left
      ; "<>", 3, `Left
      ; "@", 4, `Right
      ; "^", 4, `Right
      ; "+", 5, `Left
      ; "-", 5, `Left
      ; "*", 6, `Left
      ; "/", 6, `Left
      ; "mod", 6, `Left
      ; "**", 7, `Right
      ; "##", 8, `Left
      ]
    in
    List.concat_map
      (fun (left, lp, assoc) ->
         List.map
           (fun (right, rp, _) ->
              let input = "a " ^ left ^ " b " ^ right ^ " c" in
              test ~name:("precedence: " ^ input) ~count:1 QCheck.unit (fun () ->
                let apply op a b = Apply (Var (Test_support.operator op), a, [ b ]) in
                let a, b, c = Var (name "a"), Var (name "b"), Var (name "c") in
                let expected =
                  if lp < rp || (lp = rp && assoc = `Right)
                  then apply left a (apply right b c)
                  else apply right (apply left a b) c
                in
                match Parser.parse_expression input with
                | Ok actual -> Test_support.equal ~print:source ~expected actual
                | Error error ->
                  QCheck.Test.fail_reportf "Unexpected parse error: %s" error))
           operators)
      operators
  ;;

  let constructors =
    QCheck.Gen.oneof_list [ "true", Bool true; "false", Bool false; "()", Unit ]
  ;;

  let constructor_nesting =
    let nested constructors =
      List.fold_right
        (fun (constructor_source, constructor) (text, ast) ->
           constructor_source ^ " (" ^ text ^ ")", Apply (Constant constructor, ast, []))
        constructors
        ("x", Var (name "x"))
    in
    let generator = QCheck.Gen.(list_size (int_range 1 50) constructors) in
    test
      ~name:"nested direct constructor applications preserve their AST"
      ~count:100
      (QCheck.make ~print:(fun constructors -> fst (nested constructors)) generator)
      (fun constructors ->
         let text, expected = nested constructors in
         match Parser.parse_expression text with
         | Ok actual -> Test_support.equal ~print:source ~expected actual
         | Error error -> QCheck.Test.fail_reportf "Unexpected parse error: %s" error)
  ;;

  type program_case =
    { items : (int * structure_item) list
    ; trailing_separators : int
    }

  let item_source = function
    | Definition binding -> binding_source binding
    | Evaluation expression -> source expression
  ;;

  let separators count = String.concat "" (List.init count (fun _ -> ";;"))

  let program_source { items; trailing_separators } =
    let item index (count, item) =
      let count =
        match index, item with
        | 0, _ | _, Definition _ -> count
        | _, Evaluation _ -> max 1 count
      in
      separators count ^ "\n" ^ item_source item
    in
    String.concat "\n" (List.mapi item items) ^ separators trailing_separators
  ;;

  let program_gen =
    let open QCheck.Gen in
    let ( let* ) = ( >>= ) in
    let item =
      let* expression = expression_gen 2 in
      let definition =
        let* pattern = oneof_list patterns in
        let* recursion = oneof_list [ Nonrecursive; Recursive ] in
        return (Definition { recursion; pattern; expression })
      in
      oneof [ definition; return (Evaluation expression) ]
    in
    let* items = list_size (int_range 0 12) (pair (int_range 0 3) item) in
    let* trailing_separators = int_range 0 3 in
    return { items; trailing_separators }
  ;;

  let shrink_program input =
    let shrink_item (separators, item) =
      let expression, rebuild =
        match item with
        | Definition binding ->
          binding.expression, fun expression -> Definition { binding with expression }
        | Evaluation expression -> expression, fun expression -> Evaluation expression
      in
      QCheck.Iter.map
        (fun expression -> separators, rebuild expression)
        (shrink expression)
    in
    QCheck.Iter.map
      (fun items -> { input with items })
      (QCheck.Shrink.list ~shrink:shrink_item input.items)
  ;;

  let program_roundtrip =
    test
      ~name:"program source preserves definitions and evaluations"
      ~count:1000
      (QCheck.make ~print:program_source ~shrink:shrink_program program_gen)
      (fun input ->
         let expected = List.map snd input.items in
         match Parser.parse_program (program_source input) with
         | Ok actual ->
           Test_support.equal
             ~print:(fun items -> String.concat ";;\n" (List.map item_source items))
             ~expected
             actual
         | Error error -> QCheck.Test.fail_reportf "Unexpected parse error: %s" error)
  ;;

  let tests =
    [ roundtrip; program_roundtrip; operator_values; constructor_nesting ]
    @ infix_precedence
  ;;
end

module Angstrom = Make (struct
    include Bkml_frontend.Parser

    let name = "Angstrom"
  end)

module Menhir = Make (struct
    include Bkml_frontend.Menhir_parser

    let name = "Menhir"
  end)

module Extended = Make (struct
    let parse_expression = Bkml_frontend.Parser.parse_expression
    let parse_program = Bkml_frontend.Parser.parse_program_with_operators
    let name = "Angstrom with operator extension"
  end)

let operator_shrinking =
  QCheck.Test.make
    ~name:"operator shrinking preserves the original failure on mod"
    ~count:1
    QCheck.unit
    (fun () ->
       let arbitrary = QCheck.set_gen (QCheck.Gen.return "mod") Angstrom.operator_names in
       let cell =
         QCheck.Test.make_cell ~count:1 arbitrary (fun text ->
           Name.to_string (Test_support.operator text) <> "mod")
       in
       match QCheck.TestResult.get_state (QCheck.Test.check_cell cell) with
       | Failed { instances = [ { instance; _ } ] } ->
         Test_support.equal ~print:Test_support.print_string ~expected:"mod" instance
       | _ -> QCheck.Test.fail_report "Expected a minimized failure on mod")
;;

let () =
  QCheck_runner.run_tests_main
    ((operator_shrinking :: Extended.program_roundtrip :: Angstrom.tests) @ Menhir.tests)
;;
