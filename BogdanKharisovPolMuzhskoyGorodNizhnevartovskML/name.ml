(* SPDX-License-Identifier: LGPL-3.0-or-later *)

type t = string

type error =
  | Invalid_identifier of string
  | Invalid_operator of string

let is_word_operator name = String.equal name "mod"

let is_keyword = function
  | "else" | "false" | "fun" | "if" | "in" | "let" | "rec" | "then" | "true" -> true
  | name -> is_word_operator name
;;

let is_identifier_start = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let of_identifier name =
  if
    String.length name > 0
    && is_identifier_start name.[0]
    && String.for_all is_identifier_char name
    && name <> "_"
    && not (is_keyword name)
  then Ok name
  else Error (Invalid_identifier name)
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

let to_string name = name

let error_to_string = function
  | Invalid_identifier name -> Printf.sprintf "Invalid identifier: %S" name
  | Invalid_operator name -> Printf.sprintf "Invalid operator name: %S" name
;;
