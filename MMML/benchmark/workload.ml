[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type t =
  { definitions : int
  ; program : program
  ; source : string
  }

let advance seed = ((seed * 25_173) + 13_849) land 0x3FFF_FFFF
let choose seed bound = advance seed mod bound
let variable name = Variable name
let integer value = Constant (Integer value)
let pattern name = PVariable name
let name prefix seed depth = Printf.sprintf "%s_%d_%d" prefix depth (choose seed 10_000)

let variable_from scope seed =
  match scope with
  | [] -> variable "input"
  | _ ->
    (match List.nth_opt scope (choose seed (List.length scope)) with
     | Some name -> variable name
     | None -> variable "input")
;;

let leaf scope seed =
  match choose seed 5 with
  | 0 -> integer (choose seed 101 - 50)
  | 1 -> Constant (Boolean (choose seed 2 = 0))
  | 2 -> Constant Unit
  | _ -> variable_from scope seed
;;

let operator seed =
  let operators = [| "+"; "-"; "*"; "/"; "<"; "<="; ">"; ">="; "="; "<>"; "&&"; "||" |] in
  operators.(choose seed (Array.length operators))
;;

let rec expression ~depth ~seed ~scope =
  if depth = 0
  then leaf scope seed
  else (
    let next = advance seed in
    match choose seed 10 with
    | 0 -> leaf scope seed
    | 1 ->
      infix
        (operator seed)
        (expression ~depth:(depth - 1) ~seed:next ~scope)
        (expression ~depth:(depth - 1) ~seed:(advance next) ~scope)
    | 2 ->
      let parameter = name "argument" seed depth in
      Lambda
        ( pattern parameter
        , expression ~depth:(depth - 1) ~seed:next ~scope:(parameter :: scope) )
    | 3 ->
      let parameter = name "applied" seed depth in
      Application
        ( Lambda
            ( pattern parameter
            , expression ~depth:(depth - 1) ~seed:next ~scope:(parameter :: scope) )
        , expression ~depth:(depth - 1) ~seed:(advance next) ~scope )
    | 4 ->
      If_then_else
        ( infix "<" (variable_from scope seed) (integer (choose next 20))
        , expression ~depth:(depth - 1) ~seed:next ~scope
        , expression ~depth:(depth - 1) ~seed:(advance next) ~scope )
    | 5 ->
      let local = name "local" seed depth in
      Let_in
        ( Nonrecursive
        , pattern local
        , expression ~depth:(depth - 1) ~seed:next ~scope
        , expression ~depth:(depth - 1) ~seed:(advance next) ~scope:(local :: scope) )
    | 6 ->
      Let_in
        ( Nonrecursive
        , PWildcard
        , expression ~depth:(depth - 1) ~seed:next ~scope
        , expression ~depth:(depth - 1) ~seed:(advance next) ~scope )
    | 7 ->
      let function_name = name "recursive" seed depth in
      let parameter = name "parameter" next depth in
      Let_in
        ( Recursive
        , pattern function_name
        , Lambda
            ( pattern parameter
            , expression
                ~depth:(depth - 1)
                ~seed:(advance next)
                ~scope:(parameter :: function_name :: scope) )
        , Application (variable function_name, variable_from scope next) )
    | 8 ->
      application
        (variable_from scope seed)
        [ expression ~depth:(depth - 1) ~seed:next ~scope ]
    | _ ->
      application
        (variable_from scope seed)
        [ expression ~depth:(depth - 1) ~seed:next ~scope
        ; expression ~depth:(depth - 1) ~seed:(advance next) ~scope
        ])
;;

let factorial index =
  let function_name = Printf.sprintf "bench_fact_%d" index in
  let n = variable "n" in
  let k = variable "k" in
  let body =
    If_then_else
      ( infix "<" n (integer 2)
      , application k [ integer 1 ]
      , application
          (variable function_name)
          [ infix "-" n (integer 1)
          ; function_
              [ pattern "value" ]
              (application k [ infix "*" (variable "value") n ])
          ] )
  in
  Definition
    (Recursive, pattern function_name, function_ [ pattern "n"; pattern "k" ] body)
;;

let fibonacci index =
  let function_name = Printf.sprintf "bench_fib_%d" index in
  let recursive_call offset continuation =
    application
      (variable function_name)
      [ infix "-" (variable "n") (integer offset); continuation ]
  in
  let sum = infix "+" (variable "left") (variable "right") in
  let body =
    If_then_else
      ( infix "<" (variable "n") (integer 2)
      , application (variable "k") [ variable "n" ]
      , recursive_call
          1
          (function_
             [ pattern "left" ]
             (recursive_call
                2
                (function_ [ pattern "right" ] (application (variable "k") [ sum ])))) )
  in
  Definition
    (Recursive, pattern function_name, function_ [ pattern "n"; pattern "k" ] body)
;;

let generated_definition index =
  let seed = advance (index + 1) in
  let depth = 3 + (index mod 3) in
  let definition_name = Printf.sprintf "bench_generated_%d" index in
  let parameters = [ pattern "input"; pattern "continuation" ] in
  let scope = [ "continuation"; "input" ] in
  let definition_pattern =
    if index mod 13 = 0 then PWildcard else pattern definition_name
  in
  Definition
    ( Nonrecursive
    , definition_pattern
    , function_ parameters (expression ~depth ~seed ~scope) )
;;

let definition index =
  match index mod 11 with
  | 0 -> factorial index
  | 1 -> fibonacci index
  | _ -> generated_definition index
;;

let create definitions =
  if definitions <= 0
  then invalid_arg "Workload.create: definitions must be positive"
  else (
    let program = List.init definitions definition in
    { definitions; program; source = Pretty_printer.program_to_string program })
;;

let default_sizes = [ 1_000; 5_000; 10_000 ]
