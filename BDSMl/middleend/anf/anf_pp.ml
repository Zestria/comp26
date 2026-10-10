[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend
open Printer
open Anf_ast

let pp_imm = function
  | ImmInt x -> string_of_int x
  | ImmBool b -> string_of_bool b
  | ImmUnit -> "()"
  | ImmId id -> id
;;

let rec pp_app_list = function
  | [] -> ""
  | [ imm ] -> pp_imm imm
  | imm :: xs -> pp_imm imm ^ " " ^ pp_app_list xs
;;

let pad n = String.make n ' '

let rec pp_cexpr ind = function
  | CImm immexpr -> pp_imm immexpr
  | CBinOp (op, c1, c2) ->
    Printf.sprintf "(%s %s %s)" (pp_imm c1) (print_binop op) (pp_imm c2)
  | CUnOp (op, c) -> Printf.sprintf "(%s %s)" (print_unop op) (pp_imm c)
  | CApp (f, []) -> pp_imm f
  | CApp (c1, c2) -> Printf.sprintf "(%s %s)" (pp_imm c1) (pp_app_list c2)
  | CIf (b, a1, a2) ->
    Printf.sprintf
      "if %s then (\n%s%s)\n%selse (\n%s%s)"
      (pp_imm b)
      (pad (ind + 2))
      (pp_aexpr (ind + 2) a1)
      (pad ind)
      (pad (ind + 2))
      (pp_aexpr (ind + 2) a2)

and pp_aexpr ind = function
  | ALet (p, ce, ae) ->
    Printf.sprintf
      "let %s = %s in\n%s%s"
      (print_pattern p)
      (pp_cexpr ind ce)
      (pad ind)
      (pp_aexpr ind ae)
  | ACExpr ce -> pp_cexpr ind ce
;;

let rec pp_pattern_list = function
  | [] -> ""
  | [ p ] -> print_pattern p
  | p :: tl -> print_pattern p ^ " " ^ pp_pattern_list tl
;;

let pp_decl = function
  | AEval aexpr -> pp_aexpr 0 aexpr
  | ADecl (rec_flag, p, pl, ae) ->
    let rec_str =
      match rec_flag with
      | Rec -> "let rec"
      | NonRec -> "let"
    in
    let params = String.concat "" (List.map (fun p -> " " ^ print_pattern p) pl) in
    Printf.sprintf "%s %s%s =\n  %s" rec_str (print_pattern p) params (pp_aexpr 2 ae)
;;

let rec anf_pp = function
  | [] -> ""
  | [ prog ] -> pp_decl prog
  | prog :: tl -> pp_decl prog ^ ";;\n" ^ anf_pp tl
;;
