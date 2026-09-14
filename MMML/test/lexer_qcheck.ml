open QCheck
open Token

let show_tokens tokens =
  Format.asprintf
    "[%a]"
    (Format.pp_print_list ~pp_sep:(fun formatter () -> Format.fprintf formatter ";@ ") pp)
    tokens
;;

(** Arbitrary token lists contain no [EOF], because EOF is produced by the
    lexer itself. Payload generators are defined alongside [Token.t]. *)
let token_lists =
  let generator =
    Gen.map
      (List.filter (function
         | EOF -> false
         | _ -> true))
      (Gen.list_size (Gen.int_range 0 40) gen)
  in
  make ~print:show_tokens generator
;;

let lexemes separator tokens = tokens |> List.map to_lexeme |> String.concat separator

let check_round_trip ~source expected =
  let expected = expected @ [ EOF ] in
  match Lexer.tokens source with
  | Error error ->
    Test.fail_reportf
      "input: %S@,unexpected lexer error: %a"
      source
      Error_monad.pp_error
      error
  | Ok actual when List.equal equal expected actual -> true
  | Ok actual ->
    Test.fail_reportf
      "input: %S@,expected: %s@,actual:   %s"
      source
      (show_tokens expected)
      (show_tokens actual)
;;

(** Property: printing arbitrary tokens with spaces and lexing the result
    recovers the same tokens followed by [EOF]. *)
let token_round_trip =
  Test.make
    ~name:"token lexemes round-trip through the lexer"
    ~count:1_000
    token_lists
    (fun expected -> check_round_trip ~source:(lexemes " " expected) expected)
;;

(** Property: a correctly closed nested comment separates tokens exactly like
    ordinary whitespace. *)
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
