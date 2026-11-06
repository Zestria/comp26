[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Backtracking parser combinators implemented with State_error.
    Failed alternatives rewind only the cursor; farthest diagnostics are
    threaded explicitly into the next alternative. *)
type state =
  { source : string
  ; cursor : int
  ; furthest : int
  ; diagnostic : (int * string) option
  }

type 'a t = (state, 'a, string) State_error.t

let return = State_error.return
let ( >>= ) = State_error.bind
let ( >>| ) action f = action >>= fun x -> return (f x)
let ( *> ) a b = a >>= fun _ -> b
let ( <* ) a b = a >>= fun x -> b *> return x
let ( <*> ) f a = f >>= fun f -> a >>| f
let fail = State_error.fail

let ( <|> ) left right state =
  match left state with
  | Ok _ as success -> success
  | Error (_, failed) -> right { failed with cursor = state.cursor }
;;

let tracked action =
  action
  >>= fun value ->
  State_error.modify (fun state ->
    { state with furthest = max state.furthest state.cursor })
  *> return value
;;

let fail_at message =
  State_error.get
  >>= fun state ->
  let diagnostic =
    match state.diagnostic with
    | Some (pos, _) when pos > state.cursor -> state.diagnostic
    | _ -> Some (state.cursor, message)
  in
  State_error.put { state with diagnostic } *> fail message
;;

let peek_char =
  State_error.get
  >>| fun state ->
  if state.cursor < String.length state.source
  then Some state.source.[state.cursor]
  else None
;;

let advance count =
  State_error.get
  >>= fun state ->
  if count < 0 || count > String.length state.source - state.cursor
  then fail "unexpected end of input"
  else State_error.put { state with cursor = state.cursor + count }
;;

let satisfy predicate =
  peek_char
  >>= function
  | Some c when predicate c -> advance 1 *> return c
  | _ -> fail "unexpected character"
;;

let char wanted = satisfy (Char.equal wanted)

let string wanted =
  State_error.get
  >>= fun state ->
  let length = String.length wanted in
  if
    length <= String.length state.source - state.cursor
    && String.sub state.source state.cursor length = wanted
  then advance length *> return wanted
  else fail "unexpected token"
;;

let take_while1 predicate =
  State_error.get
  >>= fun state ->
  let rec finish index =
    if index < String.length state.source && predicate state.source.[index]
    then finish (index + 1)
    else index
  in
  let stop = finish state.cursor in
  if stop = state.cursor
  then fail "expected token"
  else
    advance (stop - state.cursor)
    *> return (String.sub state.source state.cursor (stop - state.cursor))
;;

let fix build =
  let rec parser state = build parser state in
  parser
;;

let many parser =
  let rec loop values state =
    match parser state with
    | Error (_, failed) -> Ok (List.rev values, { failed with cursor = state.cursor })
    | Ok (_, next) when next.cursor = state.cursor ->
      Error ("repeated parser accepted empty input", next)
    | Ok (value, next) -> loop (value :: values) next
  in
  loop []
;;

let many1 parser = parser >>= fun x -> many parser >>| fun xs -> x :: xs
let skip_many parser = many parser >>| fun _ -> ()
let option fallback parser = parser <|> return fallback
let choice parsers = List.fold_right ( <|> ) parsers (fail "no alternative")

let line_col source offset =
  let rec loop index line col =
    if index >= offset || index >= String.length source
    then line, col
    else if source.[index] = '\n'
    then loop (index + 1) (line + 1) 1
    else loop (index + 1) line (col + 1)
  in
  loop 0 1 1
;;

let run parser source =
  let complete =
    parser
    <* (peek_char
        >>= function
        | None -> return ()
        | Some _ -> fail "syntax error")
  in
  match complete { source; cursor = 0; furthest = 0; diagnostic = None } with
  | Ok (value, _) -> Ok value
  | Error (_, state) ->
    let offset, message =
      match state.diagnostic with
      | Some (pos, message) when pos >= state.furthest -> pos, message
      | _ -> state.furthest, "syntax error"
    in
    let line, col = line_col source offset in
    Error (Printf.sprintf "%d:%d: %s" line col message)
;;
