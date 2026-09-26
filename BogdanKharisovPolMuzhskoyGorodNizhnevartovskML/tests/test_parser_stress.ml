(* SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend
open Ast

let preview input =
  let length = String.length input in
  if length <= 256
  then Printf.sprintf "%S" input
  else
    Printf.sprintf
      "%d bytes: %S ... %S"
      length
      (String.sub input 0 128)
      (String.sub input (length - 128) 128)
;;

let repeat count text = String.concat "" (List.init count (fun _ -> text))
let variable text = Var (Result.get_ok (Name.of_identifier text))
let indexed index = variable ("x" ^ string_of_int index)
let binary operator left right = Apply (Var operator, left, [ right ])

type 'a expected =
  | Parsed of 'a
  | Rejected

let application () =
  let arguments = List.init 10_000 (fun i -> "x" ^ string_of_int i) in
  ( "f " ^ String.concat " " arguments
  , Apply (variable "f", indexed 0, List.init 9_999 (fun i -> indexed (i + 1))) )
;;

let chain operator associativity () =
  let name = Result.get_ok (Name.of_operator operator) in
  let indices = List.init 10_000 Fun.id in
  let expected =
    match associativity with
    | `Left ->
      List.fold_left
        (fun left i -> binary name left (indexed (i + 1)))
        (indexed 0)
        indices
    | `Right ->
      List.fold_right
        (fun i right -> binary name (indexed i) right)
        indices
        (indexed 10_000)
  in
  ( String.concat
      (" " ^ operator ^ " ")
      (List.init 10_001 (fun i -> "x" ^ string_of_int i))
  , expected )
;;

let parentheses () = repeat 1_000 "(" ^ "x" ^ repeat 1_000 ")", variable "x"

let nested_comments () =
  repeat 2_000 "(*" ^ "comment" ^ repeat 2_000 "*)" ^ "x", variable "x"
;;

let long_comment () = "(*" ^ String.make 1_000_000 ' ' ^ "*)x", variable "x"

let long_quoted_comment body () =
  let delimiter = String.make 40_000 'a' in
  ( "(* {" ^ delimiter ^ "|" ^ String.make 40_000 body ^ "|" ^ delimiter ^ "} *) 1"
  , Constant (Int (Result.get_ok (Integer_literal.of_string "1"))) )
;;

let constructors () =
  List.fold_right
    (fun i (input, expression) ->
       let text, constructor =
         match i mod 3 with
         | 0 -> "true", Bool true
         | 1 -> "false", Bool false
         | _ -> "()", Unit
       in
       text ^ " (" ^ input ^ ")", Apply (Constant constructor, expression, []))
    (List.init 1_000 Fun.id)
    ("x", variable "x")
;;

let program () =
  let binding =
    { recursion = Nonrecursive
    ; pattern = PVar (Result.get_ok (Name.of_identifier "x"))
    ; expression = Constant (Int (Result.get_ok (Integer_literal.of_string "1")))
    }
  in
  ( repeat 2_000 "let x = 1 in x;;"
  , List.init 2_000 (fun _ -> Evaluation (Let (binding, variable "x"))) )
;;

let valid fixture () =
  let input, expected = fixture () in
  input, Parsed expected
;;

let truncated count fixture () =
  let input, _ = fixture () in
  String.sub input 0 (String.length input - count), Rejected
;;

let unfinished suffix fixture () =
  let input, _ = fixture () in
  input ^ suffix, Rejected
;;

module Make (Parser : sig
    include module type of Frontend.Parser

    val name : string
  end) =
struct
  let case label parse fixture =
    QCheck.Test.make
      ~name:(Parser.name ^ ": " ^ label)
      ~count:1
      QCheck.unit
      (fun () ->
         let input, expected = fixture () in
         match expected, parse input with
         | Parsed expected, Ok actual when actual = expected -> true
         | Rejected, Error _ -> true
         | Parsed _, Ok _ ->
           QCheck.Test.fail_reportf
             "Input: %s\nAST differs from the expected structure"
             (preview input)
         | Parsed _, Error error ->
           QCheck.Test.fail_reportf
             "Input: %s\nUnexpected error: %s"
             (preview input)
             error
         | Rejected, Ok _ ->
           QCheck.Test.fail_reportf "Input: %s\nExpected rejection" (preview input))
  ;;

  let expression label fixture = case label Parser.parse_expression fixture

  let tests =
    [ expression "10,000 ordered application arguments" (valid application)
    ; expression
        "unfinished argument after 10,000 arguments"
        (unfinished " (" application)
    ; expression "10,000 left-associative operators" (valid (chain "+" `Left))
    ; expression
        "missing operand after 10,000 left-associative operators"
        (unfinished " +" (chain "+" `Left))
    ; expression "10,000 right-associative operators" (valid (chain "**" `Right))
    ; expression
        "missing operand after 10,000 right-associative operators"
        (unfinished " **" (chain "**" `Right))
    ; expression "1,000 nested parentheses" (valid parentheses)
    ; expression "missing final closing parenthesis" (truncated 1 parentheses)
    ; expression "2,000 nested comments" (valid nested_comments)
    ; expression "missing final comment delimiter" (truncated 3 nested_comments)
    ; expression "one million characters inside a comment" (valid long_comment)
    ; expression "unterminated million-character comment" (truncated 2 long_comment)
    ; expression
        "40,000-character quoted delimiter and body"
        (valid (long_quoted_comment 'x'))
    ; expression
        "40,000-character quoted delimiter with 40,000 false closing starts"
        (valid (long_quoted_comment '|'))
    ; expression
        "missing closing brace after a 40,000-character quoted delimiter"
        (truncated 6 (long_quoted_comment 'x'))
    ; expression "1,000 nested constructor arguments" (valid constructors)
    ; expression
        "missing final closing constructor parenthesis"
        (truncated 1 constructors)
    ; expression "1,000 constructor arguments unclosed at every level" (fun () ->
        repeat 1_000 "true (" ^ "x", Rejected)
    ; case "2,000 local let evaluations" Parser.parse_program (valid program)
    ; case
        "invalid suffix after 2,000 local let evaluations"
        Parser.parse_program
        (unfinished "@" program)
    ]
  ;;
end

module Angstrom = Make (struct
    include Frontend.Parser

    let name = "Angstrom"
  end)

module Menhir = Make (struct
    include Frontend.Menhir_parser

    let name = "Menhir"
  end)

let () = QCheck_runner.run_tests_main (Angstrom.tests @ Menhir.tests)
