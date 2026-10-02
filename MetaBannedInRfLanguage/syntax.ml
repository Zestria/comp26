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

type assoc =
  | Left
  | Right

type fixity =
  | Prefix of int
  | Infix of int * assoc

type entry =
  { operators : string list
  ; fixity : fixity
  }

type table =
  { infix : entry list
  ; prefix : entry list
  }

let default_table =
  { infix =
      [ { fixity = Infix (0, Right); operators = [ "||" ] }
      ; { fixity = Infix (1, Right); operators = [ "&&" ] }
      ; { fixity = Infix (2, Left); operators = [ "="; "<>"; "<="; ">="; "<"; ">" ] }
      ; { fixity = Infix (3, Left); operators = [ "+"; "-" ] }
      ; { fixity = Infix (4, Left); operators = [ "*"; "/" ] }
      ]
  ; prefix = [ { fixity = Prefix 1; operators = [ "+"; "-"; "not" ] } ]
  }
;;

let apply_infix_overrides overrides table =
  let overridden = List.map (fun (operator, _, _) -> operator) overrides in
  let updated =
    List.filter_map
      (fun entry ->
         let operators =
           List.filter
             (fun operator -> not (List.exists (String.equal operator) overridden))
             entry.operators
         in
         match operators with
         | [] -> None
         | operators -> Some { entry with operators })
      table.infix
  in
  let moved =
    List.map
      (fun (operator, precedence, associativity) ->
         { operators = [ operator ]; fixity = Infix (precedence, associativity) })
      overrides
  in
  let precedence_of = function
    | { fixity = Infix (precedence, _); _ } -> precedence
    | { fixity = Prefix _; _ } -> 0
  in
  let infix =
    List.sort
      (fun a b -> Int.compare (precedence_of a) (precedence_of b))
      (updated @ moved)
  in
  { table with infix }
;;

let apply_prefix_overrides overrides table =
  let overridden = List.map (fun (operator, _) -> operator) overrides in
  let updated =
    List.filter_map
      (fun entry ->
         let operators =
           List.filter
             (fun operator -> not (List.exists (String.equal operator) overridden))
             entry.operators
         in
         match operators with
         | [] -> None
         | operators -> Some { entry with operators })
      table.prefix
  in
  let moved =
    List.map
      (fun (operator, arity) -> { operators = [ operator ]; fixity = Prefix arity })
      overrides
  in
  let arity_of = function
    | { fixity = Prefix arity; _ } -> arity
    | { fixity = Infix _; _ } -> 0
  in
  let prefix =
    List.sort (fun a b -> Int.compare (arity_of a) (arity_of b)) (updated @ moved)
  in
  { table with prefix }
;;

let infix_entries table = table.infix
let prefix_entries table = table.prefix

let operator_names table =
  List.concat_map (fun entry -> entry.operators) (table.infix @ table.prefix)
;;

let lookup_infix table name =
  List.find_map
    (fun entry ->
       if List.exists (String.equal name) entry.operators
       then (
         match entry.fixity with
         | Infix (precedence, associativity) -> Some (precedence, associativity)
         | Prefix _ -> None)
       else None)
    table.infix
;;

let lookup_prefix table name =
  List.find_mapi
    (fun priority entry ->
       if List.exists (String.equal name) entry.operators
       then (
         match entry.fixity with
         | Prefix arity -> Some (priority, arity)
         | Infix _ -> None)
       else None)
    table.prefix
;;

let is_operator table name =
  Option.is_some (lookup_infix table name) || Option.is_some (lookup_prefix table name)
;;
