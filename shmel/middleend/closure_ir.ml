[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib

type imm = Anf.imm =
  | ImmInt of int
  | ImmVar of string

type cond = Anf.cond =
  | CondImm of imm
  | CondCmp of Ast.binop * imm * imm

type cexpr =
  | CImm of imm
  | CNeg of imm
  | CBinOp of Ast.binop * imm * imm
  | CIf of cond * aexpr * aexpr
  | MakeClosure of code * imm list
  | GetEnv of string * int
  | ApplyClosure of imm * imm list

and aexpr =
  | ALet of string * cexpr * aexpr
  | AExpr of cexpr

and code =
  { label : string
  ; self : string
  ; params : string list
  ; captures : string list
  ; body : aexpr
  }

type decl =
  | DLet of string * aexpr
  | DExpr of aexpr

type program = decl list

let pp_imm ppf = function
  | ImmInt n -> Format.fprintf ppf "%d" n
  | ImmVar x -> Format.pp_print_string ppf x
;;

let pp_list pp = Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") pp
let pp_name = Format.pp_print_string

let op = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Eq -> "="
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.Le -> "<="
  | Ast.Ge -> ">="
;;

let pp_cond ppf = function
  | CondImm i -> pp_imm ppf i
  | CondCmp (o, a, b) -> Format.fprintf ppf "%a %s %a" pp_imm a (op o) pp_imm b
;;

let rec pp_expr ppf = function
  | AExpr c -> pp_complex ppf c
  | ALet (x, c, body) ->
    Format.fprintf ppf "@[<v>let %s = %a in@,%a@]" x pp_complex c pp_expr body

and pp_complex ppf = function
  | CImm i -> pp_imm ppf i
  | CNeg i -> Format.fprintf ppf "neg(%a)" pp_imm i
  | CBinOp (o, a, b) -> Format.fprintf ppf "%a %s %a" pp_imm a (op o) pp_imm b
  | GetEnv (self, i) -> Format.fprintf ppf "env(%s)[%d]" self i
  | ApplyClosure (f, args) ->
    Format.fprintf ppf "@[<h>apply(%a; %a)@]" pp_imm f (pp_list pp_imm) args
  | CIf (c, t, e) ->
    Format.fprintf
      ppf
      "@[<v>if %a then@,  @[<v>%a@]@,else@,  @[<v>%a@]@]"
      pp_cond
      c
      pp_expr
      t
      pp_expr
      e
  | MakeClosure (code, fields) ->
    Format.fprintf
      ppf
      "@[<v>closure %s [%a] {@,  @[<v>code (%s; %a) captures [%a] =@,  @[<v>%a@]@]@,}@]"
      code.label
      (pp_list pp_imm)
      fields
      code.self
      (pp_list pp_name)
      code.params
      (pp_list pp_name)
      code.captures
      pp_expr
      code.body
;;

let pp_decl ppf = function
  | DExpr e -> pp_expr ppf e
  | DLet (x, e) -> Format.fprintf ppf "@[<v 2>let %s =@,%a@]" x pp_expr e
;;

let pp_program ppf program =
  Format.fprintf
    ppf
    "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,@,") pp_decl)
    program
;;
