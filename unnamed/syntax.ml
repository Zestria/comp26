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

type infix_override =
  { operator : string
  ; precedence : int
  ; associativity : associativity
  }

type prefix_override =
  { operator : string
  ; arity : int
  }

type infix_entry =
  { precedence : int
  ; associativity : associativity
  ; operators : string list
  }

type prefix_entry =
  { arity : int
  ; operators : string list
  }

type table =
  { infix : infix_entry list
  ; prefix : prefix_entry list
  }

let default_table : table =
  { infix =
      [ { precedence = 0; associativity = Right; operators = [ "||" ] }
      ; { precedence = 1; associativity = Right; operators = [ "&&" ] }
      ; { precedence = 2
        ; associativity = Left
        ; operators = [ "="; "<>"; "<="; ">="; "<"; ">" ]
        }
      ; { precedence = 3; associativity = Left; operators = [ "+"; "-" ] }
      ; { precedence = 4; associativity = Left; operators = [ "*"; "/" ] }
      ]
  ; prefix = [ { arity = 1; operators = [ "+"; "-"; "not" ] } ]
  }
;;

let apply_infix_overrides overrides table =
  let overridden = List.map (fun override -> override.operator) overrides in
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
      (fun override ->
         { precedence = override.precedence
         ; associativity = override.associativity
         ; operators = [ override.operator ]
         })
      overrides
  in
  let infix =
    List.sort (fun a b -> Int.compare a.precedence b.precedence) (updated @ moved)
  in
  { table with infix }
;;

let apply_prefix_overrides overrides table =
  let overridden = List.map (fun override -> override.operator) overrides in
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
      (fun override -> { arity = override.arity; operators = [ override.operator ] })
      overrides
  in
  let prefix = List.sort (fun a b -> Int.compare a.arity b.arity) (updated @ moved) in
  { table with prefix }
;;

let infix_entries table = table.infix
let prefix_entries table = table.prefix
let infix_names table = List.concat_map (fun entry -> entry.operators) table.infix
let prefix_names table = List.concat_map (fun entry -> entry.operators) table.prefix
let operator_names table = infix_names table @ prefix_names table

let lookup_infix table name =
  List.find_map
    (fun entry ->
       if List.exists (String.equal name) entry.operators
       then Some (entry.precedence, entry.associativity)
       else None)
    table.infix
;;

let lookup_prefix table name =
  List.find_mapi
    (fun priority (entry : prefix_entry) ->
       if List.exists (String.equal name) entry.operators
       then Some (priority, entry.arity)
       else None)
    table.prefix
;;

let is_operator table name =
  Option.is_some (lookup_infix table name) || Option.is_some (lookup_prefix table name)
;;
