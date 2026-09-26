[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t = string

type error =
  | Invalid_identifier of string
  | Invalid_operator of string

let unary_minus = "~-"
let is_word_operator name = String.equal name (Keyword.to_string Keyword.Mod)

type operator_kind =
  | Prefix
  | Hash
  | Comparison
  | Concatenation
  | Additive
  | Multiplicative
  | Power
  | Conjunction
  | Disjunction
  | Assignment

module Syntax = struct
  let is_identifier_start = function
    | 'a' .. 'z' | '_' -> true
    | _ -> false
  ;;

  let is_identifier_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  ;;

  let is_symbol_char = function
    | '!'
    | '$'
    | '%'
    | '&'
    | '*'
    | '+'
    | '-'
    | '.'
    | '/'
    | ':'
    | '<'
    | '='
    | '>'
    | '?'
    | '@'
    | '^'
    | '|'
    | '~' -> true
    | _ -> false
  ;;

  let is_symbol_char_or_hash char = is_symbol_char char || char = '#'
end

open Syntax

let of_identifier name =
  if
    String.length name > 0
    && is_identifier_start name.[0]
    && String.for_all is_identifier_char name
    && name <> "_"
    && Option.is_none (Keyword.of_string name)
  then Ok name
  else Error (Invalid_identifier name)
;;

let is_symbol_operator name =
  match name with
  | "" | "->" | "<-" | "|" -> false
  | ":=" -> true
  | _ ->
    (match name.[0] with
     | '!' -> String.for_all is_symbol_char_or_hash name
     | '~' | '?' | '#' ->
       String.length name > 1 && String.for_all is_symbol_char_or_hash name
     | '=' | '<' | '>' | '|' | '&' | '$' | '@' | '^' | '+' | '-' | '*' | '/' | '%' ->
       String.for_all is_symbol_char name
     | _ -> false)
;;

let of_operator name =
  if is_word_operator name || is_symbol_operator name
  then Ok name
  else Error (Invalid_operator name)
;;

let operator_kind name =
  match name with
  | "&" | "&&" -> Some Conjunction
  | "||" -> Some Disjunction
  | ":=" -> Some Assignment
  | "!=" -> Some Comparison
  | _ when is_word_operator name -> Some Multiplicative
  | _ ->
    (match name.[0] with
     | '!' | '~' | '?' -> Some Prefix
     | '#' -> Some Hash
     | '=' | '<' | '>' | '|' | '&' | '$' -> Some Comparison
     | '@' | '^' -> Some Concatenation
     | '+' | '-' -> Some Additive
     | '*' when String.starts_with ~prefix:"**" name -> Some Power
     | '*' | '/' | '%' -> Some Multiplicative
     | _ -> None)
;;

let to_string name = name

let error_to_string = function
  | Invalid_identifier name -> Printf.sprintf "Invalid identifier: %S" name
  | Invalid_operator name -> Printf.sprintf "Invalid operator name: %S" name
;;
