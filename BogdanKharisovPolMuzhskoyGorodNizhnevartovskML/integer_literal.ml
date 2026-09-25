(* SPDX-License-Identifier: LGPL-3.0-or-later *)

type t = string
type error = Invalid_integer_literal of string

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let of_string text =
  let length = String.length text in
  let first = if length > 0 && text.[0] = '-' then 1 else 0 in
  if
    first < length
    && is_digit text.[first]
    && String.for_all
         (fun char -> is_digit char || char = '_')
         (String.sub text first (length - first))
  then Ok text
  else Error (Invalid_integer_literal text)
;;

let to_string literal = literal

let error_to_string (Invalid_integer_literal text) =
  Printf.sprintf "Invalid integer literal: %S" text
;;
