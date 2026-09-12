open QCheck
open Token

let show_tokens tokens =
  Format.asprintf
    "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun formatter () -> Format.fprintf formatter ";@ ")
       pp)
    tokens
;;

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

let lexemes separator tokens =
  tokens |> List.map to_lexeme |> String.concat separator
;;

(** Property: printing arbitrary tokens with spaces and lexing the result
    recovers the same tokens followed by [EOF]. *)
let token_round_trip =
  Test.make
    ~name:"token lexemes round-trip through the lexer"
    ~count:1_000
    token_lists
    (fun expected ->
      match Lexer.tokens (lexemes " " expected) with
      | Error _ -> false
      | Ok actual -> List.equal equal (expected @ [ EOF ]) actual)
;;

(** Property: a correctly closed nested comment separates tokens exactly like
    ordinary whitespace. *)
let comments_are_whitespace =
  Test.make
    ~name:"nested comments behave like whitespace"
    ~count:500
    token_lists
    (fun expected ->
      let separator = " (* ignored (* nested *) comment *) " in
      match Lexer.tokens (lexemes separator expected) with
      | Error _ -> false
      | Ok actual -> List.equal equal (expected @ [ EOF ]) actual)
;;
