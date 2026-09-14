open Token

let show_tokens tokens =
  Format.asprintf
    "[%a]"
    (Format.pp_print_list ~pp_sep:(fun formatter () -> Format.fprintf formatter ";@ ") pp)
    tokens
;;

let tokens_exn input =
  match Lexer.tokens input with
  | Ok tokens -> tokens
  | Error error ->
    failwith (Format.asprintf "unexpected lexer error: %a" Error_monad.pp_error error)
;;

let tokens_of_lexer_exn lexer =
  match Lexer.tokens_of_lexer lexer with
  | Ok tokens -> tokens
  | Error error ->
    failwith (Format.asprintf "unexpected lexer error: %a" Error_monad.pp_error error)
;;

let check_tokens ~name expected input =
  let actual = tokens_exn input in
  if not (List.equal equal expected actual)
  then
    failwith
      (Format.asprintf
         "%s:@ input: %S@ expected: %s@ actual:   %s"
         name
         input
         (show_tokens expected)
         (show_tokens actual))
;;

let check_error ~name ~line ~column ~message input =
  match Lexer.tokenize input with
  | Ok tokens ->
    failwith
      (Format.asprintf
         "%s: expected an error, got %s"
         name
         (show_tokens (List.map (fun x -> x.token) tokens)))
  | Error error ->
    if
      error.location.line <> line
      || error.location.column <> column
      || not (String.equal error.message message)
    then
      failwith
        (Format.asprintf
           "%s:@ expected error at line %d, column %d: %s@ actual: %a"
           name
           line
           column
           message
           Error_monad.pp_error
           error)
;;

(** Checks that reserved words are distinguished from ordinary identifiers. *)
let test_keywords () =
  check_tokens
    ~name:"keywords and identifiers"
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
    ; TRUE
    ; ELSE
    ; FALSE
    ; EOF
    ]
    "let rec fact = fun n -> if n <= 1 then true else false"
;;

(** Checks tokens used by patterns, tuples, lists, functions, and match cases. *)
let test_patterns_and_delimiters () =
  check_tokens
    ~name:"patterns and delimiters"
    [ MATCH
    ; IDENT "xs"
    ; WITH
    ; LBRACKET
    ; RBRACKET
    ; ARROW
    ; LPAREN
    ; RPAREN
    ; BAR
    ; IDENT "x"
    ; CONS
    ; IDENT "xs"
    ; ARROW
    ; LPAREN
    ; IDENT "x"
    ; COMMA
    ; IDENT "xs"
    ; RPAREN
    ; EOF
    ]
    "match xs with [] -> () | x :: xs -> (x, xs)"
;;

(** Checks a representative expression containing built-in operators. *)
let test_operators () =
  check_tokens
    ~name:"built-in operators"
    [ IDENT "x"
    ; NOT_EQUAL
    ; IDENT "y"
    ; AND_AND
    ; IDENT "x"
    ; GREATER_EQUAL
    ; INT 0
    ; OR_OR
    ; IDENT "not"
    ; FALSE
    ; EOF
    ]
    "x <> y && x >= 0 || not false"
;;

(** Checks each operator spelling, longest-match behavior, and operator names. *)
let test_operator_tokens () =
  let cases =
    [ ( "operator names in parentheses"
      , "( + ) ( - ) ( * ) ( / )"
      , [ LPAREN
        ; PLUS
        ; RPAREN
        ; LPAREN
        ; MINUS
        ; RPAREN
        ; LPAREN
        ; STAR
        ; RPAREN
        ; LPAREN
        ; SLASH
        ; RPAREN
        ; EOF
        ] )
    ; ( "comparison operator names in parentheses"
      , "( = ) ( <> ) ( < ) ( <= ) ( > ) ( >= )"
      , [ LPAREN
        ; EQUAL
        ; RPAREN
        ; LPAREN
        ; NOT_EQUAL
        ; RPAREN
        ; LPAREN
        ; LESS
        ; RPAREN
        ; LPAREN
        ; LESS_EQUAL
        ; RPAREN
        ; LPAREN
        ; GREATER
        ; RPAREN
        ; LPAREN
        ; GREATER_EQUAL
        ; RPAREN
        ; EOF
        ] )
    ; ( "boolean operator names in parentheses"
      , "( && ) ( || )"
      , [ LPAREN; AND_AND; RPAREN; LPAREN; OR_OR; RPAREN; EOF ] )
    ; ( "minus and plus keep no unary/infix distinction in the lexer"
      , "-x + +y - z"
      , [ MINUS; IDENT "x"; PLUS; PLUS; IDENT "y"; MINUS; IDENT "z"; EOF ] )
    ; ( "explicit OCaml unary operator names"
      , "~-x ~+ y"
      , [ TILDE_MINUS; IDENT "x"; TILDE_PLUS; IDENT "y"; EOF ] )
    ; ( "not is an ordinary identifier"
      , "let not = fun x -> x in not false"
      , [ LET
        ; IDENT "not"
        ; EQUAL
        ; FUN
        ; IDENT "x"
        ; ARROW
        ; IDENT "x"
        ; IN
        ; IDENT "not"
        ; FALSE
        ; EOF
        ] )
    ; ( "longest supported operators are single tokens"
      , "x <= y || x <> z && head :: tail"
      , [ IDENT "x"
        ; LESS_EQUAL
        ; IDENT "y"
        ; OR_OR
        ; IDENT "x"
        ; NOT_EQUAL
        ; IDENT "z"
        ; AND_AND
        ; IDENT "head"
        ; CONS
        ; IDENT "tail"
        ; EOF
        ] )
    ; "structural symbols stay distinct tokens", "| -> ::", [ BAR; ARROW; CONS; EOF ]
    ]
  in
  List.iter (fun (name, input, expected) -> check_tokens ~name expected input) cases
;;

(** Checks that correctly closed nested comments are ignored as whitespace. *)
let test_comments () =
  check_tokens
    ~name:"nested comments"
    [ LET; IDENT "answer"; EQUAL; INT 42; EOF ]
    "let (* outer (* nested *) comment *) answer = 42"
;;

(** Checks valid boundaries around identifiers and numeric literals. *)
let test_identifier_boundaries () =
  check_tokens
    ~name:"a number and an identifier separated by whitespace"
    [ INT 9; IDENT "ui"; EOF ]
    "9 ui";
  check_tokens
    ~name:"digits are allowed after the start of an identifier"
    [ IDENT "ui9"; EOF ]
    "ui9";
  check_tokens
    ~name:"apostrophes and underscores inside identifiers"
    [ IDENT "_value"; IDENT "value'"; IDENT "value''9"; EOF ]
    "_value value' value''9";
  check_tokens
    ~name:"keyword prefixes remain identifiers"
    [ IDENT "letx"; IDENT "recursive"; IDENT "trueValue"; IDENT "not"; EOF ]
    "letx recursive trueValue not";
  check_tokens
    ~name:"a comment separates a number from an identifier"
    [ INT 9; IDENT "ui"; EOF ]
    "9(* separator *)ui"
;;

(** Checks that balanced delimiters are parser concerns, not lexer state. *)
let test_unclosed_delimiters_are_tokens () =
  check_tokens
    ~name:"unclosed delimiters remain tokens for the parser"
    [ LPAREN; IDENT "x"; LBRACKET; INT 1; SEMICOLON; INT 2; EOF ]
    "(x [1; 2"
;;

(** Checks half-open byte ranges attached to ordinary tokens and EOF. *)
let test_locations () =
  match Lexer.tokenize "let x" with
  | Error error -> failwith (Error_monad.show_error error)
  | Ok located ->
    let point offset line column = Location.{ offset; line; column } in
    let span start finish = Location.{ start; finish } in
    let expected =
      [ { token = LET; span = span (point 0 1 1) (point 3 1 4) }
      ; { token = IDENT "x"; span = span (point 4 1 5) (point 5 1 6) }
      ; { token = EOF; span = span (point 5 1 6) (point 5 1 6) }
      ]
    in
    if not (List.equal equal_located expected located)
    then
      failwith
        (Format.asprintf
           "locations:@ expected: %a@ actual:   %a"
           (Format.pp_print_list pp_located)
           expected
           (Format.pp_print_list pp_located)
           located)
;;

let next_token_exn lexer =
  match Lexer.next_token lexer with
  | Ok result -> result
  | Error error -> failwith (Error_monad.show_error error)
;;

(** Checks sequential [next_token] consumption and stable EOF behavior. *)
let test_incremental_lexer () =
  let lexer = Lexer.create "let x" in
  let first = next_token_exn lexer in
  let second = next_token_exn lexer in
  let eof = next_token_exn lexer in
  let repeated_eof = next_token_exn lexer in
  if
    (not (equal first.token LET))
    || (not (equal second.token (IDENT "x")))
    || (not (equal eof.token EOF))
    || not (equal_located eof repeated_eof)
  then failwith "next_token did not advance correctly or keep EOF stable"
;;

(** Checks buffered, multiline tokenization from an input channel. *)
let test_buffered_channel () =
  let source = "let x = 1\nlet y = x + 2\n" in
  let expected =
    [ LET; IDENT "x"; EQUAL; INT 1; LET; IDENT "y"; EQUAL; IDENT "x"; PLUS; INT 2; EOF ]
  in
  let path = Filename.temp_file "mml-lexer-" ".ml" in
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
            let actual = tokens_of_lexer_exn (Lexer.from_channel input) in
            if not (List.equal equal expected actual)
            then
              failwith
                (Format.asprintf
                   "buffered channel:@ input: %S@ expected: %s@ actual:   %s"
                   source
                   (show_tokens expected)
                   (show_tokens actual))))
;;

(** Checks that digit-leading identifier-shaped lexemes are rejected whole. *)
let test_invalid_identifiers () =
  check_error
    ~name:"identifier starting with a digit"
    ~line:1
    ~column:1
    ~message:"invalid identifier \"9ui\": identifiers cannot start with a digit"
    "9ui";
  check_error
    ~name:"identifier starting with several digits"
    ~line:1
    ~column:1
    ~message:"invalid identifier \"123abc456\": identifiers cannot start with a digit"
    "123abc456";
  check_error
    ~name:"identifier with a leading digit after a newline"
    ~line:2
    ~column:3
    ~message:"invalid identifier \"7value\": identifiers cannot start with a digit"
    "let x =\n  7value";
  check_error
    ~name:"identifier starting with a digit and underscore"
    ~line:1
    ~column:1
    ~message:"invalid identifier \"9_name\": identifiers cannot start with a digit"
    "9_name"
;;

(** Checks that maximal unsupported operator spellings produce lexer errors. *)
let test_unknown_operators () =
  check_error
    ~name:"unknown operator"
    ~line:1
    ~column:3
    ~message:"unknown operator \"++\""
    "a ++ b";
  check_error
    ~name:"longest unknown operator"
    ~line:1
    ~column:2
    ~message:"unknown operator \"+-=\""
    "x+-=y";
  check_error
    ~name:"unsupported single-character operator"
    ~line:1
    ~column:3
    ~message:"unknown operator \"@\""
    "f @ x"
;;

(** Checks unsupported non-operator characters and their exact positions. *)
let test_unexpected_characters () =
  check_error
    ~name:"unexpected character"
    ~line:1
    ~column:5
    ~message:"unexpected character '`'"
    "let `";
  check_error
    ~name:"unexpected character inside an identifier-shaped input"
    ~line:1
    ~column:2
    ~message:"unexpected character '#'"
    "x#field";
  check_error
    ~name:"unexpected opening brace"
    ~line:1
    ~column:1
    ~message:"unexpected character '{'"
    "{x}";
  check_error
    ~name:"multiline unexpected character location"
    ~line:2
    ~column:3
    ~message:"unexpected character '`'"
    "let x = 1\n  `"
;;

(** Checks that EOF inside simple and nested comments reports their opening. *)
let test_unterminated_comments () =
  check_error
    ~name:"unterminated comment"
    ~line:1
    ~column:3
    ~message:"unterminated comment"
    "x (* never closed";
  check_error
    ~name:"unterminated nested multiline comment"
    ~line:2
    ~column:1
    ~message:"unterminated comment"
    "let x = 1\n(* outer\n   (* inner *)"
;;

(** Checks that an integer outside the runtime [int] range is rejected. *)
let test_integer_overflow () =
  check_error
    ~name:"integer overflow"
    ~line:1
    ~column:1
    ~message:"integer literal is out of range"
    (string_of_int Int.max_int ^ "0")
;;

let () =
  test_keywords ();
  test_patterns_and_delimiters ();
  test_operators ();
  test_operator_tokens ();
  test_comments ();
  test_identifier_boundaries ();
  test_unclosed_delimiters_are_tokens ();
  test_locations ();
  test_incremental_lexer ();
  test_buffered_channel ();
  test_invalid_identifiers ();
  test_unknown_operators ();
  test_unexpected_characters ();
  test_unterminated_comments ();
  test_integer_overflow ()
;;
