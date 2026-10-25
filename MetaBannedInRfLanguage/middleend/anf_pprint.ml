[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Anf

let is_infix name = Option.is_some (Syntax.lookup_infix Syntax.default_table name)
let is_prefix name = Option.is_some (Syntax.lookup_prefix Syntax.default_table name)

(* Spaces keep [( * )] from being read as a comment. *)
let pp_name ppf name =
  if Syntax.is_operator Syntax.default_table name
  then fprintf ppf "( %s )" name
  else pp_print_string ppf name
;;

let pp_imm ppf = function
  | ImmInt n when n < 0 -> fprintf ppf "(%d)" n
  | ImmInt n -> pp_print_int ppf n
  | ImmBool b -> pp_print_bool ppf b
  | ImmUnit -> pp_print_string ppf "()"
  | ImmVar name -> pp_name ppf name
;;

let pp_list pp_sep pp_item =
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf pp_sep) pp_item
;;

let pp_tuple ppf items = fprintf ppf "(%a)" (pp_list ", " pp_imm) items
let pp_params = pp_list " " pp_name

(* Fits on one line: no nested bindings and no branches. *)
let rec is_simple = function
  | CImm _ | CApp _ | CTuple _ | CMatchFailure -> true
  | CFun (_, body) -> is_simple_aexpr body
  | CIte _ -> false

and is_simple_aexpr = function
  | ACExpr cexpr -> is_simple cexpr
  | ALet _ | ALetTuple _ | ALetRec _ -> false
;;

let rec pp_cexpr ppf = function
  | CImm imm -> pp_imm ppf imm
  | CApp (ImmVar (("~+" | "~-") as op), [ arg ]) -> fprintf ppf "%c %a" op.[1] pp_imm arg
  | CApp (ImmVar op, [ left; right ]) when is_infix op ->
    fprintf ppf "%a %s %a" pp_imm left op pp_imm right
  | CApp (ImmVar op, [ arg ]) when is_prefix op && not (is_infix op) ->
    fprintf ppf "%s %a" op pp_imm arg
  | CApp (fn, args) ->
    fprintf ppf "@[<hov 2>%a@ %a@]" pp_imm fn (pp_list "@ " pp_imm) args
  | CTuple (first, second, rest) -> pp_tuple ppf (first :: second :: rest)
  | CIte (cond, then_branch, else_branch) ->
    fprintf
      ppf
      "@[<v>if %a@,%a@,%a@]"
      pp_imm
      cond
      (pp_branch "then")
      then_branch
      (pp_branch "else")
      else_branch
  | CFun (params, body) when is_simple_aexpr body ->
    fprintf ppf "fun %a -> %a" pp_params params pp_aexpr body
  | CFun (params, body) ->
    fprintf ppf "@[<v 2>fun %a ->@,%a@]" pp_params params pp_aexpr body
  | CMatchFailure -> pp_print_string ppf "assert false"

and pp_branch keyword ppf branch =
  if is_simple_aexpr branch
  then fprintf ppf "%s %a" keyword pp_aexpr branch
  else fprintf ppf "@[<v 2>%s@,%a@]" keyword pp_aexpr branch

(* [let name params = body] without the trailing [in] or [;;]. *)
and pp_binding keyword ppf (name, value) =
  match value with
  | CFun (params, body) when is_simple_aexpr body ->
    fprintf ppf "%s %a %a = %a" keyword pp_name name pp_params params pp_aexpr body
  | CFun (params, body) ->
    fprintf
      ppf
      "@[<v 2>%s %a %a =@,%a@]"
      keyword
      pp_name
      name
      pp_params
      params
      pp_aexpr
      body
  | value when is_simple value ->
    fprintf ppf "%s %a = %a" keyword pp_name name pp_cexpr value
  | value -> fprintf ppf "@[<v 2>%s %a =@,%a@]" keyword pp_name name pp_cexpr value

and pp_rec_bindings ppf bindings =
  List.iteri
    (fun index binding ->
       if index > 0 then fprintf ppf "@,";
       pp_binding (if index = 0 then "let rec" else "and") ppf binding)
    bindings

and pp_aexpr ppf aexpr = fprintf ppf "@[<v>%a@]" pp_aexpr_lines aexpr

and pp_aexpr_lines ppf = function
  | ALet (name, value, body) when is_simple value ->
    fprintf ppf "%a in@,%a" (pp_binding "let") (name, value) pp_aexpr_lines body
  | ALet (name, value, body) ->
    fprintf ppf "%a@,in@,%a" (pp_binding "let") (name, value) pp_aexpr_lines body
  | ALetTuple (names, subject, body) ->
    let pp_slot ppf = function
      | Some name -> pp_name ppf name
      | None -> pp_print_string ppf "_"
    in
    fprintf
      ppf
      "let (%a) = %a in@,%a"
      (pp_list ", " pp_slot)
      names
      pp_imm
      subject
      pp_aexpr_lines
      body
  | ALetRec (bindings, body) ->
    fprintf ppf "%a@,in@,%a" pp_rec_bindings bindings pp_aexpr_lines body
  | ACExpr value -> pp_cexpr ppf value
;;

let pp_item ppf = function
  | AValue (name, ACExpr value) when is_simple value ->
    fprintf ppf "%a;;" (pp_binding "let") (name, value)
  | AValue (name, ACExpr value) -> fprintf ppf "%a@,;;" (pp_binding "let") (name, value)
  | AValue (name, body) ->
    fprintf ppf "@[<v 2>let %a =@,%a@]@,;;" pp_name name pp_aexpr body
  | ARec bindings -> fprintf ppf "%a@,;;" pp_rec_bindings bindings
  | AEval (ACExpr value) when is_simple value -> fprintf ppf "let _ = %a;;" pp_cexpr value
  | AEval body -> fprintf ppf "@[<v 2>let _ =@,%a@]@,;;" pp_aexpr body
;;

let pp_program ppf program = fprintf ppf "@[<v>%a@]" (pp_list "@," pp_item) program
let string_of_program program = asprintf "%a@." pp_program program
