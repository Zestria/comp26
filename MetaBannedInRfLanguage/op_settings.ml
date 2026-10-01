[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type fixity =
  | Infix of Syntax.associativity
  | Prefix

let words s =
  let blank = function
    | '\t' | '\n' | '\r' -> ' '
    | c -> c
  in
  String.map blank s |> String.split_on_char ' ' |> List.filter (fun word -> word <> "")
;;

let fixity_of_word = function
  | "left" -> Some (Infix Syntax.Left)
  | "right" -> Some (Infix Syntax.Right)
  | "prefix" -> Some Prefix
  | _ -> None
;;

let settings_error fmt = Printf.ksprintf (fun message -> Error message) fmt

let check_duplicates kind overrides =
  let rec go seen = function
    | [] -> Ok ()
    | override :: rest ->
      let operator = override.Syntax.operator in
      if List.exists (String.equal operator) seen
      then settings_error "operator %S is overridden twice (in %s settings)" operator kind
      else go (operator :: seen) rest
  in
  go [] overrides
;;

let settings_of_string input =
  let groups = List.map words (String.split_on_char ';' input) in
  let rec go infix prefix = function
    | [] -> Ok (infix, prefix)
    | [] :: rest -> go infix prefix rest
    | [ word ] :: _ -> settings_error "settings group %S lacks a number" word
    | [ kind; number ] :: _ ->
      settings_error "settings group %S %S has no operators" kind number
    | (kind :: number :: operators) :: rest ->
      (match fixity_of_word kind, int_of_string_opt number with
       | None, _ -> settings_error "expected left, right or prefix, got %S" kind
       | _, None -> settings_error "expected a number after %S, got %S" kind number
       | Some (Infix associativity), Some precedence ->
         go
           (infix
            @ List.map
                (fun operator -> { Syntax.operator; precedence; associativity })
                operators)
           prefix
           rest
       | Some Prefix, Some arity ->
         go
           infix
           (prefix @ List.map (fun operator -> { Syntax.operator; arity }) operators)
           rest)
  in
  match go [] [] groups with
  | Error _ as error -> error
  | Ok (infix, prefix) ->
    (match check_duplicates "infix" infix with
     | Error _ as error -> error
     | Ok () ->
       (match check_duplicates "prefix" prefix with
        | Error _ as error -> error
        | Ok () -> Ok (infix, prefix)))
;;

let table_of_string settings =
  match settings_of_string settings with
  | Error _ as error -> error
  | Ok (infix, prefix) ->
    Ok
      (Syntax.default_table
       |> Syntax.apply_infix_overrides infix
       |> Syntax.apply_prefix_overrides prefix)
;;
