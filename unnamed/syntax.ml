[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let reserved =
  [ "let"
  ; "rec"
  ; "and"
  ; "in"
  ; "if"
  ; "then"
  ; "else"
  ; "match"
  ; "with"
  ; "function"
  ; "true"
  ; "false"
  ]
;;

let is_reserved name = List.exists (String.equal name) reserved

type associativity =
  | Left
  | Right

let infix_operators =
  [ Right, [ "||" ]
  ; Right, [ "&&" ]
  ; Left, [ "="; "<>"; "<="; ">="; "<"; ">" ]
  ; Left, [ "+"; "-" ]
  ; Left, [ "*"; "/" ]
  ]
;;

let prefix_operators = [ 1, [ "+"; "-"; "not" ] ]

let lookup_infix name =
  List.find_mapi
    (fun priority (associativity, names) ->
       if List.exists (String.equal name) names
       then Some (priority, associativity)
       else None)
    infix_operators
;;

let lookup_prefix name =
  List.find_mapi
    (fun priority (arity, names) ->
       if List.exists (String.equal name) names then Some (priority, arity) else None)
    prefix_operators
;;

let infix_names = List.concat_map (fun (_, names) -> names) infix_operators
let prefix_names = List.concat_map (fun (_, names) -> names) prefix_operators
let operator_names = infix_names @ prefix_names

let is_operator name =
  Option.is_some (lookup_infix name) || Option.is_some (lookup_prefix name)
;;
