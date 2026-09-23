[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Tokens

type error = [ `lexer_error of string ]

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_ident_start s = is_letter s || s = '_'
let is_ident_char s = is_letter s || s = '_' || is_digit s

let keyword_of_string = function
  | "true" -> Some TTrue
  | "false" -> Some TFalse
  | "if" -> Some TIF
  | "then" -> Some TThen
  | "else" -> Some TElse
  | "let" -> Some TLet
  | "rec" -> Some TRec
  | "in" -> Some TIn
  | "fun" -> Some TFun
  | "not" -> Some TNot
  | "_" -> Some TUnderscore
  | _ -> None
;;

let take_while src len pred i =
  let j = ref i in
  while !j < len && pred src.[!j] do
    incr j
  done;
  String.sub src i (!j - i), !j
;;

type lex_state =
  { pos : int
  ; line : int
  ; col : int
  }

let init_state = { pos = 0; line = 1; col = 1 }

exception Lexer_error of string

let error_at st fmt =
  Printf.ksprintf
    (fun msg -> raise (Lexer_error (Printf.sprintf "%d:%d: %s" st.line st.col msg)))
    fmt
;;

let rec skip src len st =
  if st.pos >= len
  then st
  else (
    match src.[st.pos] with
    | '\n' -> skip src len { pos = st.pos + 1; line = st.line + 1; col = 1 }
    | '\r' ->
      if st.pos + 1 < len && src.[st.pos + 1] = '\n'
      then skip src len { pos = st.pos + 2; line = st.line + 1; col = 1 }
      else skip src len { pos = st.pos + 1; line = st.line + 1; col = 1 }
    | ' ' -> skip src len { st with pos = st.pos + 1; col = st.col + 1 }
    | '\t' -> skip src len { st with pos = st.pos + 1; col = st.col + 1 }
    | '\012' -> skip src len { st with pos = st.pos + 1; col = st.col + 1 }
    | '(' when st.pos + 1 < len && src.[st.pos + 1] = '*' ->
      skip_comment src len st { st with pos = st.pos + 2; col = st.col + 2 } 1
    | _ -> st)

and skip_comment src len start st depth =
  if st.pos >= len
  then error_at start "unterminated comment"
  else if st.pos + 1 < len && src.[st.pos] = '(' && src.[st.pos + 1] = '*'
  then skip_comment src len start { st with pos = st.pos + 2; col = st.col + 2 } (depth + 1)
  else if st.pos + 1 < len && src.[st.pos] = '*' && src.[st.pos + 1] = ')'
  then (
    let st = { st with pos = st.pos + 2; col = st.col + 2 } in
    if depth = 1 then skip src len st else skip_comment src len start st (depth - 1))
  else (
    match src.[st.pos] with
    | '\r' when st.pos + 1 < len && src.[st.pos + 1] = '\n' ->
      skip_comment src len start { pos = st.pos + 2; line = st.line + 1; col = 1 } depth
    | '\r' | '\n' ->
      skip_comment src len start { pos = st.pos + 1; line = st.line + 1; col = 1 } depth
    | _ -> skip_comment src len start { st with pos = st.pos + 1; col = st.col + 1 } depth)
;;

let advance st n = { st with pos = st.pos + n; col = st.col + n }

let next_token src len st : (token * lex_state) option =
  let st = skip src len st in
  let i = st.pos in
  if i >= len
  then None
  else (
    let c = src.[i] in
    if is_digit c
    then (
      let s, j = take_while src len is_digit i in
      let consumed = j - i in
      match int_of_string_opt s with
      | Some n -> Some (TInt n, advance st consumed)
      | None -> error_at st "integer literal '%s' out of range" s)
    else if is_ident_start c
    then (
      let s, j = take_while src len is_ident_char i in
      let consumed = j - i in
      let tok =
        match keyword_of_string s with
        | Some t -> t
        | None -> TIdent s
      in
      Some (tok, advance st consumed))
    else (
      match c with
      | '(' ->
        if i + 1 < len && src.[i + 1] = ')'
        then Some (TUnit, advance st 2)
        else Some (TLParen, advance st 1)
      | ')' -> Some (TRParen, advance st 1)
      | '+' -> Some (TPlus, advance st 1)
      | '*' -> Some (TStar, advance st 1)
      | '/' -> Some (TSlash, advance st 1)
      | '-' ->
        if i + 1 < len && src.[i + 1] = '>'
        then Some (TArrow, advance st 2)
        else Some (TMinus, advance st 1)
      | '=' -> Some (TEqual, advance st 1)
      | '<' ->
        if i + 1 < len && src.[i + 1] = '>'
        then Some (TNotEqual, advance st 2)
        else if i + 1 < len && src.[i + 1] = '='
        then Some (TELess, advance st 2)
        else Some (TLess, advance st 1)
      | '>' ->
        if i + 1 < len && src.[i + 1] = '='
        then Some (TEMore, advance st 2)
        else Some (TMore, advance st 1)
      | '&' ->
        if i + 1 < len && src.[i + 1] = '&'
        then Some (TAnd, advance st 2)
        else error_at st "unexpected '&' (expected '&&')"
      | '|' ->
        if i + 1 < len && src.[i + 1] = '|'
        then Some (TOr, advance st 2)
        else error_at st "unexpected '|' (expected '||')"
      | ';' ->
        if i + 1 < len && src.[i + 1] = ';'
        then Some (TSemisemi, advance st 2)
        else error_at st "unexpected character ';'"
      | ch -> error_at st "unexpected character '%c'" ch))
;;

let rec tokens_seq src len st () =
  match next_token src len st with
  | exception Lexer_error msg -> Seq.Cons (Error (`lexer_error msg), fun () -> Seq.Nil)
  | None -> Seq.Cons (Ok TEOF, fun () -> Seq.Nil)
  | Some (tok, st') -> Seq.Cons (Ok tok, tokens_seq src len st')
;;

let tokenize_lazy src : (token, [> error ]) result Seq.t =
  tokens_seq src (String.length src) init_state
;;

let tokenize src : (token list, [> error ]) result =
  let rec collect seq acc =
    match seq () with
    | Seq.Nil -> Ok (List.rev acc)
    | Seq.Cons (Error e, _) -> Error e
    | Seq.Cons (Ok tok, rest) -> collect rest (tok :: acc)
  in
  collect (tokens_seq src (String.length src) init_state) []
;;
