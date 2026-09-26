(* SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend
open Ast

let name text =
  match Name.of_identifier text with
  | Ok name -> name
  | Error error -> failwith (Name.error_to_string error)
;;

let integer text =
  match Integer_literal.of_string text with
  | Ok literal -> Constant (Int literal)
  | Error error -> failwith (Integer_literal.error_to_string error)
;;

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
  | Apply (f, a, rest) -> "(" ^ String.concat " " (List.map source (f :: a :: rest)) ^ ")"
  | Fun { parameter; parameters; body } ->
    "(fun "
    ^ String.concat " " (List.map pattern_source (parameter :: parameters))
    ^ " -> "
    ^ source body
    ^ ")"
  | Let ({ recursion; pattern; expression }, body) ->
    "(let "
    ^ (match recursion with
       | Nonrecursive -> ""
       | Recursive -> "rec ")
    ^ pattern_source pattern
    ^ " = "
    ^ source expression
    ^ " in "
    ^ source body
    ^ ")"
  | If (condition, yes, no) ->
    "(if "
    ^ source condition
    ^ " then "
    ^ source yes
    ^ (match no with
       | None -> ""
       | Some no -> " else " ^ source no)
    ^ ")"
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
  | If (condition, yes, no) -> QCheck.Iter.of_list (condition :: yes :: Option.to_list no)
;;

let arbitrary =
  QCheck.make
    ~print:source
    ~shrink
    QCheck.Gen.(sized (fun size -> expression_gen (min 3 size)))
;;

let roundtrip =
  QCheck.Test.make
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
    let* rest = Test_support.strings alphabet in
    return (first ^ rest)
  in
  oneof
    [ oneof_list [ "mod"; ":=" ]
    ; symbolic [ "!"; "~!"; "?!"; "#!" ] "!$%&*+-./:<=>?@^|~#"
    ; symbolic
        [ "="; "<>"; "|+"; "&"; "$"; "@"; "^"; "+"; "-+"; "*"; "/"; "%" ]
        "!$%&*+-./:<=>?@^|~"
    ]
;;

let operator_values =
  Test_support.property
    "validated operator names parse as parenthesized values"
    operator_names
    (fun text ->
       let expected = Result.get_ok (Name.of_operator text) in
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
  let generator = QCheck.Gen.(pair (oneof_list operators) (oneof_list operators)) in
  let print ((left, _, _), (right, _, _)) = "a " ^ left ^ " b " ^ right ^ " c" in
  QCheck.Test.make
    ~name:"infix chains follow precedence and associativity"
    ~count:2000
    (QCheck.make ~print generator)
    (fun (((left, lp, assoc), (right, rp, _)) as input) ->
       let apply op a b = Apply (Var (Result.get_ok (Name.of_operator op)), a, [ b ]) in
       let a, b, c = Var (name "a"), Var (name "b"), Var (name "c") in
       let expected =
         if lp < rp || (lp = rp && assoc = `Right)
         then apply left a (apply right b c)
         else apply right (apply left a b) c
       in
       match Parser.parse_expression (print input) with
       | Ok actual -> Test_support.equal ~print:source ~expected actual
       | Error error -> QCheck.Test.fail_reportf "Unexpected parse error: %s" error)
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
  QCheck.Test.make
    ~name:"nested direct constructor applications preserve their AST"
    ~count:100
    (QCheck.make ~print:(fun constructors -> fst (nested constructors)) generator)
    (fun constructors ->
       let text, expected = nested constructors in
       match Parser.parse_expression text with
       | Ok actual -> Test_support.equal ~print:source ~expected actual
       | Error error -> QCheck.Test.fail_reportf "Unexpected parse error: %s" error)
;;

let rejects parse text =
  match parse text with
  | Error _ -> true
  | Ok _ -> QCheck.Test.fail_reportf "Expected rejection of %S" text
;;

let malformed_program =
  let input count =
    String.concat "" (List.init count (fun _ -> "let x = 1 in x;;")) ^ "@"
  in
  QCheck.Test.make
    ~name:"program rejects invalid input after local let evaluations"
    ~count:30
    (QCheck.make ~print:Test_support.print_string QCheck.Gen.(map input (int_range 1 30)))
    (rejects Parser.parse_program)
;;

let malformed_constructor =
  let input constructors =
    String.concat "" (List.map (fun (source, _) -> source ^ " (") constructors) ^ "x"
  in
  let generator = QCheck.Gen.(map input (list_size (int_range 1 30) constructors)) in
  QCheck.Test.make
    ~name:"expression rejects unclosed constructor arguments"
    ~count:30
    (QCheck.make ~print:Test_support.print_string generator)
    (rejects Parser.parse_expression)
;;

let () =
  QCheck_runner.run_tests_main
    [ roundtrip
    ; operator_values
    ; infix_precedence
    ; constructor_nesting
    ; malformed_program
    ; malformed_constructor
    ]
;;
