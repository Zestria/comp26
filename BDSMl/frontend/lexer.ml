[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type token =
  | TUnit
  | TInt of int
  | TIdent of string
  | TTrue
  | TFalse
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TEqual
  | TNotEqual
  | TMore
  | TLess
  | TEMore
  | TELess
  | TAnd
  | TOr
  | TNot
  | TLParen
  | TRParen
  | TUnderscore
  | TSemisemi (* ;; *)
  | TEOF
  | TIF
  | TThen
  | TElse
  | TLet
  | TRec
  | TIn
  | TFun
  | TArrow (* -> *)

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

let tokenize src : (token list, [> error ]) result =
  let len = String.length src in
  let rec skip i line col =
    if i >= len
    then i, line, col
    else (
      match src.[i] with
      | '\n' -> skip (i + 1) (line + 1) 1
      | '\r' ->
        if i + 1 < len && src.[i + 1] = '\n'
        then skip (i + 2) (line + 1) 1
        else skip (i + 1) (line + 1) 1
      | ' ' | '\t' -> skip (i + 1) line (col + 1)
      | _ -> i, line, col)
  in
  let error line col fmt =
    Printf.ksprintf
      (fun msg -> Error (`lexer_error (Printf.sprintf "%d:%d: %s" line col msg)))
      fmt
  in
  let rec go i line col acc =
    let i, line, col = skip i line col in
    if i >= len
    then Ok (List.rev (TEOF :: acc))
    else (
      let c = src.[i] in
      if is_digit c
      then (
        let s, j = take_while src len is_digit i in
        let consumed = j - i in
        match int_of_string_opt s with
        | Some n -> go j line (col + consumed) (TInt n :: acc)
        | None -> error line col "integer literal '%s' out of range" s)
      else if is_ident_start c
      then (
        let s, j = take_while src len is_ident_char i in
        let consumed = j - i in
        let tok =
          match keyword_of_string s with
          | Some t -> t
          | None -> TIdent s
        in
        go j line (col + consumed) (tok :: acc))
      else (
        match c with
        | '(' ->
          if i + 1 < len && src.[i + 1] = ')'
          then go (i + 2) line (col + 2) (TUnit :: acc)
          else go (i + 1) line (col + 1) (TLParen :: acc)
        | ')' -> go (i + 1) line (col + 1) (TRParen :: acc)
        | '+' -> go (i + 1) line (col + 1) (TPlus :: acc)
        | '*' -> go (i + 1) line (col + 1) (TStar :: acc)
        | '/' -> go (i + 1) line (col + 1) (TSlash :: acc)
        | '-' ->
          if i + 1 < len && src.[i + 1] = '>'
          then go (i + 2) line (col + 2) (TArrow :: acc)
          else go (i + 1) line (col + 1) (TMinus :: acc)
        | '=' -> go (i + 1) line (col + 1) (TEqual :: acc)
        | '<' ->
          if i + 1 < len && src.[i + 1] = '>'
          then go (i + 2) line (col + 2) (TNotEqual :: acc)
          else if i + 1 < len && src.[i + 1] = '='
          then go (i + 2) line (col + 2) (TELess :: acc)
          else go (i + 1) line (col + 1) (TLess :: acc)
        | '>' ->
          if i + 1 < len && src.[i + 1] = '='
          then go (i + 2) line (col + 2) (TEMore :: acc)
          else go (i + 1) line (col + 1) (TMore :: acc)
        | '&' ->
          if i + 1 < len && src.[i + 1] = '&'
          then go (i + 2) line (col + 2) (TAnd :: acc)
          else error line col "unexpected '&' (expected '&&')"
        | '|' ->
          if i + 1 < len && src.[i + 1] = '|'
          then go (i + 2) line (col + 2) (TOr :: acc)
          else error line col "unexpected '|' (expected '||')"
        | ';' ->
          if i + 1 < len && src.[i + 1] = ';'
          then go (i + 2) line (col + 2) (TSemisemi :: acc)
          else error line col "unexpected character ';'"
        | ch -> error line col "unexpected character '%c'" ch))
  in
  go 0 1 1 []
;;
