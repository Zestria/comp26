open Token
open Error_monad
open Ast

type state =
  { lexer : Lexer.t
  ; current : Token.located
  }

type 'a parser = state -> ('a * state) Error_monad.t

let ( let* ) parser continuation =
  fun state ->
  match parser state with
  | Error error -> Error error
  | Ok (value, state) -> continuation value state
;;

let ( let+ ) parser transform =
  fun state ->
  match parser state with
  | Error error -> Error error
  | Ok (value, state) -> Ok (transform value, state)
;;

let return value state = Ok (value, state)

let make_state lexer =
  match Lexer.next_token lexer with
  | Error error -> Error error
  | Ok current -> Ok { lexer; current }
;;

let peek state = return state.current state

let advance state =
  match Lexer.next_token state.lexer with
  | Error error -> Error error
  | Ok current -> Ok ((), { state with current })
;;

let unexpected state =
  match state.current.token with
  | EOF -> error state.current.span.start "unexpected end of input"
  | token ->
    error
      state.current.span.start
      (Printf.sprintf "unexpected token %S" (to_lexeme token))
;;

let expect expected state =
  if state.current.token = expected then advance state else unexpected state
;;

let binary_level operators operand state =
  let rec loop left state =
    match List.assoc_opt state.current.token operators with
    | None -> Ok (left, state)
    | Some operator ->
      (match advance state with
       | Error error -> Error error
       | Ok ((), state) ->
         (match operand state with
          | Error error -> Error error
          | Ok (right, state) -> loop (infix operator left right) state))
  in
  match operand state with
  | Error error -> Error error
  | Ok (first, state) -> loop first state
;;

let comparison_operators =
  [ EQUAL, "="
  ; NOT_EQUAL, "<>"
  ; LESS, "<"
  ; LESS_EQUAL, "<="
  ; GREATER, ">"
  ; GREATER_EQUAL, ">="
  ]
;;

let rec expr state = lambda_let_if state

and lambda_let_if state =
  match state.current.token with
  | FUN ->
    (let* () = advance in
     let* parameters = collect_parameters in
     match parameters with
     | [] -> unexpected
     | _ :: _ ->
       let* () = expect ARROW in
       let+ body = expr in
       function_ parameters body)
      state
  | IF ->
    (let* () = advance in
     let* condition = expr in
     let* () = expect THEN in
     let* if_true = expr in
     let* () = expect ELSE in
     let+ if_false = expr in
     If_then_else (condition, if_true, if_false))
      state
  | LET ->
    (let* () = advance in
     let* flag = rec_flag in
     let* pattern, value = binding in
     let* () = expect IN in
     let+ body = expr in
     Let_in (flag, pattern, value, body))
      state
  | _ -> disjunction state

and binding state =
  (let* pattern = binding_pattern in
   let* parameters = collect_parameters in
   let* () = expect EQUAL in
   let+ value = expr in
   match parameters with
   | [] -> pattern, value
   | _ :: _ -> pattern, function_ parameters value)
    state

and structure_item state =
  (let* () = expect LET in
   let* flag = rec_flag in
   let* pattern, value = binding in
   return (Definition (flag, pattern, value)))
    state

and program state =
  let rec loop reversed state =
    match state.current.token with
    | EOF -> Ok (List.rev reversed, state)
    | _ ->
      (match structure_item state with
       | Error error -> Error error
       | Ok (item, state) -> loop (item :: reversed) state)
  in
  loop [] state

and rec_flag state =
  if state.current.token = REC
  then
    (let+ () = advance in
     Recursive)
      state
  else Ok (Nonrecursive, state)

and collect_parameters state =
  let rec loop reversed state =
    match parameter_opt state with
    | Error error -> Error error
    | Ok (None, state) -> Ok (List.rev reversed, state)
    | Ok (Some parameter, state) -> loop (parameter :: reversed) state
  in
  loop [] state

and parameter_opt state =
  match state.current.token with
  | UNDERSCORE | IDENT _ | LPAREN ->
    (match parameter state with
     | Error error -> Error error
     | Ok (value, state) -> Ok (Some value, state))
  | _ -> Ok (None, state)

and parameter state =
  match state.current.token with
  | UNDERSCORE ->
    (let+ () = advance in
     PWildcard)
      state
  | IDENT name ->
    (let+ () = advance in
     PVariable name)
      state
  | LPAREN ->
    (let* () = advance in
     let* name = expect_identifier in
     let+ () = expect RPAREN in
     PVariable name)
      state
  | _ -> unexpected state

and expect_identifier state =
  match state.current.token with
  | IDENT name ->
    (let+ () = advance in
     name)
      state
  | _ -> unexpected state

and binding_pattern state =
  match state.current.token with
  | UNDERSCORE ->
    (let+ () = advance in
     PWildcard)
      state
  | IDENT name ->
    (let+ () = advance in
     PVariable name)
      state
  | LPAREN ->
    (let* () = advance in
     let* name = expect_identifier in
     let+ () = expect RPAREN in
     PVariable name)
      state
  | _ -> unexpected state

and disjunction state = binary_level [ OR_OR, "||" ] conjunction state
and conjunction state = binary_level [ AND_AND, "&&" ] comparison state
and comparison state = binary_level comparison_operators additive state
and additive state = binary_level [ PLUS, "+"; MINUS, "-" ] multiplicative state
and multiplicative state = binary_level [ STAR, "*"; SLASH, "/" ] unary state

and unary state =
  match state.current.token with
  | MINUS ->
    (let* () = advance in
     let+ operand = unary in
     match operand with
     | Constant (Integer value) -> Constant (Integer (-value))
     | operand -> prefix "~-" operand)
      state
  | PLUS ->
    (let* () = advance in
     let+ operand = unary in
     match operand with
     | Constant (Integer _) -> operand
     | operand -> prefix "~+" operand)
      state
  | _ -> application state

and application state =
  (let* first = atom in
   let+ rest = many atom_opt in
   List.fold_left (fun function_ argument -> Application (function_, argument)) first rest)
    state

and atom_opt state =
  match state.current.token with
  | INT _ | TRUE | FALSE | IDENT _ | LPAREN ->
    (match atom state with
     | Error error -> Error error
     | Ok (value, state) -> Ok (Some value, state))
  | _ -> Ok (None, state)

and many parser state =
  let rec loop reversed state =
    match parser state with
    | Error error -> Error error
    | Ok (None, state) -> Ok (List.rev reversed, state)
    | Ok (Some value, state) -> loop (value :: reversed) state
  in
  loop [] state

and atom state =
  (let* token = peek in
   match token.token with
   | INT value ->
     let+ () = advance in
     Constant (Integer value)
   | TRUE ->
     let+ () = advance in
     Constant (Boolean true)
   | FALSE ->
     let+ () = advance in
     Constant (Boolean false)
   | IDENT name ->
     let+ () = advance in
     Variable name
   | LPAREN ->
     let* () = advance in
     let* inner = peek in
     (match inner.token with
      | RPAREN ->
        let+ () = advance in
        Constant Unit
      | _ ->
        let* body = expr in
        let+ () = expect RPAREN in
        body)
   | _ -> unexpected)
    state
;;

let run parser state =
  match parser state with
  | Error error -> Error error
  | Ok (value, _state) -> Ok value
;;

let parse_expression source =
  match make_state (Lexer.from_string source) with
  | Error error -> Error error
  | Ok state ->
    run
      (let* body = expr in
       let+ () = expect EOF in
       body)
      state
;;

let parse_program source =
  match make_state (Lexer.from_string source) with
  | Error error -> Error error
  | Ok state ->
    run
      (let* body = program in
       let+ () = expect EOF in
       body)
      state
;;
