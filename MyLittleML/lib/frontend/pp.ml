[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Canonical pretty-printer for the MyLittleML AST.

    The output is parenthesized everywhere it matters (operands of binary
    operations, negation, applications, let/fun/if bodies, and decl-only
    lets), so that parsing the printed text back with either parser yields
    exactly the same AST.  This is the QCheck round-trip property. *)

let bop_to_string = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "mod"
  | Ast.Lt -> "<"
  | Ast.Le -> "<="
  | Ast.Gt -> ">"
  | Ast.Ge -> ">="
  | Ast.Eq -> "="
  | Ast.Neq -> "<>"
;;

(** [pp_atom] prints an expression that can safely appear as an operand of a
    binary operation / application / negation: atoms (ints, bools, variables)
    as-is, everything else fully parenthesized. *)
let rec pp_atom ppf = function
  | (Ast.Int _ | Ast.Bool _ | Ast.Var _) as e -> pp_expr ppf e
  | e -> Format.fprintf ppf "(%a)" pp_expr e

and pp_expr ppf = function
  | Ast.Int n -> Format.fprintf ppf "%d" n
  | Ast.Bool b -> Format.fprintf ppf "%b" b
  | Ast.Var x -> Format.fprintf ppf "%s" x
  | Ast.App (f, x) -> Format.fprintf ppf "(%a %a)" pp_atom f pp_atom x
  | Ast.Fun (params, body) ->
    Format.fprintf ppf "fun %s -> %a" (String.concat " " params) pp_atom body
  | Ast.Let (bind, None) -> Format.fprintf ppf "(%a)" pp_bind bind
  | Ast.Let (bind, Some body) -> Format.fprintf ppf "(%a in %a)" pp_bind bind pp_atom body
  | Ast.If (cond, thn, els) ->
    Format.fprintf ppf "if %a then %a else %a" pp_atom cond pp_atom thn pp_atom els
  | Ast.Binop (op, l, r) ->
    Format.fprintf ppf "(%a %s %a)" pp_atom l (bop_to_string op) pp_atom r
  | Ast.Neg e -> Format.fprintf ppf "(-%a)" pp_atom e

and pp_bind ppf { Ast.rec_; name; params; body } =
  Format.fprintf
    ppf
    "let %s%s %s = %a"
    (if rec_ then "rec " else "")
    name
    (String.concat " " params)
    pp_atom
    body
;;

let pp_prog ppf prog =
  List.iter (fun item -> Format.fprintf ppf "%a;;\n" pp_expr item) prog
;;

let to_string = Format.asprintf "%a" pp_prog
