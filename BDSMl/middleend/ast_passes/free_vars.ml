[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend
open Ast

module String_set = Set.Make (String)

let remove_pattern pattern variables =
  match pattern with
  | PVar name -> String_set.remove name variables
  | PUnit | PUnderscore -> variables
;;

let rec free_vars = function
  | Unit | Int _ | Bool _ -> String_set.empty
  | Var name -> String_set.singleton name
  | BinOp (_, left, right) | App (left, right) ->
    String_set.union (free_vars left) (free_vars right)
  | UnOp (_, expression) -> free_vars expression
  | If (condition, if_true, if_false) ->
    String_set.union
      (free_vars condition)
      (String_set.union (free_vars if_true) (free_vars if_false))
  | Abs (parameter, body) -> remove_pattern parameter (free_vars body)
  | Let (NonRec, pattern, value, body) ->
    String_set.union (free_vars value) (remove_pattern pattern (free_vars body))
  | Let (Rec, pattern, value, body) ->
    remove_pattern pattern (String_set.union (free_vars value) (free_vars body))
;;
