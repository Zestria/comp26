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

let collect_tokens lexer =
  let rec loop reversed =
    match Lexer.next_token lexer with
    | Error error -> Error error
    | Ok { token = EOF; _ } -> Ok (List.rev (EOF :: reversed))
    | Ok located -> loop (located.token :: reversed)
  in
  loop []
;;

let token_generator =
  let fixed =
    [ LET
    ; REC
    ; IN
    ; FUN
    ; IF
    ; THEN
    ; ELSE
    ; TRUE
    ; FALSE
    ; UNDERSCORE
    ; LPAREN
    ; RPAREN
    ; ARROW
    ; EQUAL
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
    ]
  in
  Gen.oneof_weighted
    [ 8, Gen.oneof_list fixed
    ; 1, Gen.map (fun value -> INT value) Gen.nat_small
    ; 1, Gen.map (fun value -> IDENT ("value_" ^ string_of_int value)) Gen.nat_small
    ]
;;

let token_lists =
  make ~print:show_tokens (Gen.list_size (Gen.int_range 0 40) token_generator)
;;

let lexemes separator tokens = tokens |> List.map to_lexeme |> String.concat separator

let check_round_trip ~source expected =
  match collect_tokens (Lexer.from_string source) with
  | Error error ->
    Test.fail_reportf
      "input: %S@,unexpected lexer error: %a"
      source
      Error_monad.pp_error
      error
  | Ok actual when actual = expected @ [ EOF ] -> true
  | Ok actual ->
    Test.fail_reportf
      "input: %S@,expected: %s@,actual:   %s"
      source
      (show_tokens (expected @ [ EOF ]))
      (show_tokens actual)
;;

(** Printing arbitrary supported tokens with spaces and lexing them again
    preserves the token stream. *)
let token_round_trip =
  Test.make
    ~name:"supported token lexemes round-trip"
    ~count:1_000
    token_lists
    (fun expected -> check_round_trip ~source:(lexemes " " expected) expected)
;;

(** A closed nested comment separates tokens exactly like whitespace. *)
let comments_are_whitespace =
  Test.make
    ~name:"nested comments behave like whitespace"
    ~count:500
    token_lists
    (fun expected ->
       let comment = "(* ignored (* nested *) comment *)" in
       let separator = " " ^ comment ^ " " in
       let source = comment ^ " " ^ lexemes separator expected ^ " " ^ comment in
       check_round_trip ~source expected)
;;

let () = exit (QCheck_runner.run_tests [ token_round_trip; comments_are_whitespace ])
