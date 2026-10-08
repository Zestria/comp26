[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck
open Token

let show_tokens tokens =
  Format.asprintf
    "[%a]"
    (Format.pp_print_list ~pp_sep:(fun formatter () -> Format.fprintf formatter ";@ ") pp)
    tokens
;;

let collect lexer =
  let rec loop reversed =
    match Lexer.next_token lexer with
    | Error error -> Error error
    | Ok located ->
      let reversed = located :: reversed in
      (match located.token with
       | EOF -> Ok (List.rev reversed)
       | _ -> loop reversed)
  in
  loop []
;;

let tokens located = List.map (fun located -> located.token) located

let expect_tokens source expected =
  match collect (Lexer.from_string source) with
  | Error error ->
    Test.fail_reportf
      "input: %S@,unexpected lexer error: %a"
      source
      Error_monad.pp_error
      error
  | Ok located ->
    let actual = tokens located in
    if actual <> expected
    then
      Test.fail_reportf
        "input: %S@,expected: %s@,actual:   %s"
        source
        (show_tokens expected)
        (show_tokens actual)
;;

let expect_error source ~line ~column ~message =
  match collect (Lexer.from_string source) with
  | Ok located ->
    Test.fail_reportf
      "input: %S@,expected an error, got: %s"
      source
      (show_tokens (tokens located))
  | Error error ->
    if
      error.location.line <> line
      || error.location.column <> column
      || error.message <> message
    then
      Test.fail_reportf
        "input: %S@,expected line %d, column %d: %s@,actual: %a"
        source
        line
        column
        message
        Error_monad.pp_error
        error
;;

let case name check =
  Test.make ~name ~count:1 unit (fun () ->
    check ();
    true)
;;

(** A complete recursive definition uses only tokens supported by the minimal
    AST. *)
let factorial =
  case "tokenize a recursive factorial definition" (fun () ->
    expect_tokens
      "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1)"
      [ LET
      ; REC
      ; IDENT "fact"
      ; EQUAL
      ; FUN
      ; IDENT "n"
      ; ARROW
      ; IF
      ; IDENT "n"
      ; LESS_EQUAL
      ; INT 1
      ; THEN
      ; INT 1
      ; ELSE
      ; IDENT "n"
      ; STAR
      ; IDENT "fact"
      ; LPAREN
      ; IDENT "n"
      ; MINUS
      ; INT 1
      ; RPAREN
      ; EOF
      ])
;;

(** Every supported binary operator is emitted as one explicit token. *)
let binary_operators =
  case "tokenize supported binary operators" (fun () ->
    expect_tokens
      "= <> < <= > >= + - * / && ||"
      [ EQUAL
      ; NOT_EQUAL
      ; LESS
      ; LESS_EQUAL
      ; GREATER
      ; GREATER_EQUAL
      ; PLUS
      ; MINUS
      ; STAR
      ; SLASH
      ; AND_AND
      ; OR_OR
      ; EOF
      ])
;;

(** Words belonging only to removed AST constructs are ordinary identifiers
    in the minimal language. *)
let removed_keywords =
  case "removed keywords become identifiers" (fun () ->
    expect_tokens
      "and function match with"
      [ IDENT "and"; IDENT "function"; IDENT "match"; IDENT "with"; EOF ])
;;

(** Punctuation belonging only to lists, tuples, and match is rejected. *)
let removed_punctuation =
  case "reject removed punctuation" (fun () ->
    expect_error "[" ~line:1 ~column:1 ~message:"unexpected character '['";
    expect_error "," ~line:1 ~column:1 ~message:"unexpected character ','";
    expect_error "::" ~line:1 ~column:1 ~message:"unknown operator \"::\"")
;;

(** Repeated calls consume one token at a time and keep EOF stable. *)
let incremental_string =
  case "read a string one token at a time" (fun () ->
    let lexer = Lexer.from_string "let x" in
    let next () =
      match Lexer.next_token lexer with
      | Ok located -> located
      | Error error -> Test.fail_reportf "%a" Error_monad.pp_error error
    in
    let first = next () in
    let second = next () in
    let eof = next () in
    let repeated_eof = next () in
    if
      first.token <> LET
      || second.token <> IDENT "x"
      || eof.token <> EOF
      || repeated_eof <> eof
    then Test.fail_report "next_token returned an unexpected token sequence")
;;

(** Constructing a channel lexer performs no read. Requesting one token fills
    only the internal buffer rather than loading a large file completely. *)
let lazy_channel =
  case "read a channel lazily and in bounded chunks" (fun () ->
    let source = "let " ^ String.make 20_000 'x' in
    let path = Filename.temp_file "mmml-lexer-" ".ml" in
    Fun.protect
      ~finally:(fun () -> Sys.remove path)
      (fun () ->
         let output = open_out_bin path in
         Fun.protect
           ~finally:(fun () -> close_out output)
           (fun () -> output_string output source);
         let input = open_in_bin path in
         Fun.protect
           ~finally:(fun () -> close_in input)
           (fun () ->
              let lexer = Lexer.from_channel input in
              let before = pos_in input in
              let first = Lexer.next_token lexer in
              let after = pos_in input in
              if before <> 0
              then Test.fail_reportf "lexer construction consumed %d bytes" before;
              (match first with
               | Error error -> Test.fail_reportf "%a" Error_monad.pp_error error
               | Ok located when located.token = LET -> ()
               | Ok located -> Test.fail_reportf "expected LET, got %a" pp located.token);
              if after <= 0 || after >= String.length source
              then
                Test.fail_reportf
                  "first token caused an invalid channel position: %d of %d"
                  after
                  (String.length source))))
;;

(** Line and column information is updated while whitespace is skipped. *)
let locations =
  case "track multiline token locations" (fun () ->
    let lexer = Lexer.from_string "let\n  x" in
    let first = Lexer.next_token lexer in
    let second = Lexer.next_token lexer in
    match first, second with
    | Ok { token = LET; span = first_span }, Ok { token = IDENT "x"; span = second_span }
      when first_span.start.line = 1
           && first_span.start.column = 1
           && second_span.start.line = 2
           && second_span.start.column = 3 -> ()
    | _ -> Test.fail_report "unexpected tokens or source locations")
;;

(** Lexical errors retain their exact source position and diagnostic. *)
let errors =
  case "report positioned lexical errors" (fun () ->
    expect_error
      "9name"
      ~line:1
      ~column:1
      ~message:"invalid identifier \"9name\": identifiers cannot start with a digit";
    expect_error "let x\n  (* open" ~line:2 ~column:3 ~message:"unterminated comment";
    expect_error
      (string_of_int Int.max_int ^ "0")
      ~line:1
      ~column:1
      ~message:"integer literal is out of range")
;;

let () =
  exit
    (QCheck_runner.run_tests
       [ factorial
       ; binary_operators
       ; removed_keywords
       ; removed_punctuation
       ; incremental_string
       ; lazy_channel
       ; locations
       ; errors
       ])
;;
