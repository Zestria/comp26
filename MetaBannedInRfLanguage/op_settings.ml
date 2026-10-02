[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let words s =
  let blank = function
    | '\t' | '\n' | '\r' -> ' '
    | c -> c
  in
  String.map blank s |> String.split_on_char ' ' |> List.filter (fun word -> word <> "")
;;

let settings_error fmt = Printf.ksprintf (fun message -> Error message) fmt

let check_duplicates kind overrides operator_of =
  let rec go seen = function
    | [] -> Ok ()
    | override :: rest ->
      let operator = operator_of override in
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
      (match kind, int_of_string_opt number with
       | "prefix", Some arity ->
         go infix (prefix @ List.map (fun operator -> operator, arity) operators) rest
       | "left", Some precedence ->
         go
           (infix @ List.map (fun operator -> operator, precedence, Syntax.Left) operators)
           prefix
           rest
       | "right", Some precedence ->
         go
           (infix
            @ List.map (fun operator -> operator, precedence, Syntax.Right) operators)
           prefix
           rest
       | _, None -> settings_error "expected a number after %S, got %S" kind number
       | _ -> settings_error "expected left, right or prefix, got %S" kind)
  in
  match go [] [] groups with
  | Error _ as error -> error
  | Ok (infix, prefix) ->
    (match check_duplicates "infix" infix (fun (operator, _, _) -> operator) with
     | Error _ as error -> error
     | Ok () ->
       (match check_duplicates "prefix" prefix (fun (operator, _) -> operator) with
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
