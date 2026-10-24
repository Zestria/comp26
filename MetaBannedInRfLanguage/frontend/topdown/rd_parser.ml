[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Angstrom
open Base

(* ==================== auxiliary ==================== *)

let ws = skip_while Char.is_whitespace
let ws1 = skip Char.is_whitespace *> ws
let token str = ws *> string str

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let keyword str =
  token str *> peek_char
  >>= function
  | Some c when is_ident_char c -> fail ("There is no separator after the keyword " ^ str)
  | _ -> return str
;;

(* Word operators go through [keyword]; symbolic ones through [token]. *)
let p_operator_token name =
  match String.unsafe_get name 0 with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> keyword name
  | _ -> token name
;;

let chain_left p_op p_expr =
  let rec go acc =
    p_op
    >>= (fun op ->
    p_expr >>= fun argument -> go (EApply (EApply (EIdent op, acc), argument)))
    <|> return acc
  in
  p_expr >>= go
;;

let chain_right p_op p_expr =
  let rec go () =
    p_expr
    >>= fun e ->
    p_op
    >>= (fun op -> go () >>= fun r -> return (EApply (EApply (EIdent op, e), r)))
    <|> return e
  in
  go ()
;;

(* ==================== constants ==================== *)

let parse_int =
  take_while1 Char.is_digit
  >>= fun n ->
  peek_char
  >>= function
  | Some c when is_ident_char c -> fail "There is no separator after an integer"
  | _ -> return (CInt (Int.of_string n))
;;

let parse_unit = token "()" *> return CUnit

let parse_bool =
  choice
    [ keyword "true" *> return (CBool true); keyword "false" *> return (CBool false) ]
;;

let parse_const = ws *> choice [ parse_int; parse_unit; parse_bool ]

(* ==================== identifier ==================== *)

let parse_ident =
  ws
  *> satisfy (function
    | 'a' .. 'z' | '_' -> true
    | _ -> false)
  >>| String.of_char
  >>= fun first ->
  take_while is_ident_char
  >>= fun rest ->
  let name = first ^ rest in
  if Syntax.is_reserved name then fail (name ^ " is a reserved word") else return name
;;

(* ==================== pattern ==================== *)

let parse_pattern =
  fix (fun self ->
    let p_const = parse_const >>| fun c -> PConstant c in
    let p_any = keyword "_" *> return PAny in
    let p_var = parse_ident >>| fun name -> PVar name in
    let p_paren_inner =
      self
      >>= fun first ->
      many (token "," *> self)
      >>= function
      | [] -> return first
      | second :: tail -> return (PTuple (first, second, tail))
    in
    let p_paren = token "(" *> p_paren_inner <* token ")" in
    choice [ p_const; p_any; p_var; p_paren ])
;;

(* ==================== expressions ==================== *)

let parse_rec_flag = keyword "rec" *> return Rec <|> return NonRec

let parse_value_binding self =
  parse_pattern
  >>= fun pattern ->
  many parse_pattern
  >>= fun patterns ->
  token "=" *> self
  >>| fun expression ->
  { vb_pat = pattern
  ; vb_expr =
      List.fold_right ~f:(fun pat acc -> EFun (pat, acc)) patterns ~init:expression
  }
;;

let parse_case self =
  token "|" *> parse_pattern
  >>= fun case_pat -> token "->" *> self >>| fun case_expr -> { case_pat; case_expr }
;;

let parse_cases self =
  parse_case self
  >>= fun first -> many (ws1 *> parse_case self) >>| fun rest -> first, rest
;;

(* Parses [()], [(e)], [(a, b)], and [( + )]. *)
let parse_expr_paren table self =
  let operator_value =
    choice
      (List.map (Syntax.operator_names table) ~f:(fun name ->
         p_operator_token name >>| fun _ -> EIdent name))
  in
  let tuple =
    self
    >>= fun first ->
    many (token "," *> self)
    >>= function
    | [] -> return first
    | second :: tail -> return (ETuple (first, second, tail))
  in
  token "(" *> (operator_value <* token ")" <|> (tuple <* token ")"))
;;

let parse_atom table self =
  choice
    [ (parse_const >>| fun c -> EConst c)
    ; (parse_ident >>| fun name -> EIdent name)
    ; parse_expr_paren table self
    ]
;;

let parse_expr_application atom =
  let rec go acc = ws1 *> atom >>= (fun arg -> go (EApply (acc, arg))) <|> return acc in
  atom >>= go
;;

(* Prefix ops bind tighter than infix but looser than application:
   [-a * b] is [(-a) * b], while [-f x] is [-(f x)]. *)
let parse_expr_prefix table atom application =
  let operators =
    List.concat_map
      ~f:(fun entry ->
        match entry.fixity with
        | Syntax.Prefix arity -> List.map ~f:(fun name -> name, arity) entry.operators
        | Syntax.Infix _ -> [])
      (Syntax.prefix_entries table)
  in
  let p_op =
    choice
      (List.map operators ~f:(fun (name, arity) ->
         p_operator_token name >>| fun _ -> name, arity))
  in
  let p_applied name = application >>| fun operand -> EApply (EIdent name, operand) in
  let p_arity name arity =
    let rec go n operands =
      if n = 0
      then return (List.rev operands)
      else atom >>= fun operand -> go (n - 1) (operand :: operands)
    in
    go arity []
    >>| fun operands ->
    List.fold_left operands ~init:(EIdent name) ~f:(fun acc operand ->
      EApply (acc, operand))
  in
  p_op
  >>= (fun (name, arity) -> if arity = 1 then p_applied name else p_arity name arity)
  <|> application
;;

let parse_expr_infix table lower =
  List.fold_right (Syntax.infix_entries table) ~init:lower ~f:(fun entry inner ->
    let p_op =
      choice
        (List.map entry.operators ~f:(fun name -> p_operator_token name >>| fun _ -> name))
    in
    match entry.fixity with
    | Syntax.Infix (_, associativity) ->
      (match associativity with
       | Syntax.Left -> chain_left p_op inner
       | Syntax.Right -> chain_right p_op inner)
    | Syntax.Prefix _ -> inner)
;;

let parse_expr_if self =
  keyword "if"
  >>= fun _ ->
  self
  >>= fun condition ->
  ws1 *> keyword "then" *> self
  >>= fun then_branch ->
  let else_branch = ws1 *> keyword "else" *> self >>| (fun e -> Some e) <|> return None in
  else_branch >>| fun e -> EIf (condition, then_branch, e)
;;

let parse_expr_match self =
  keyword "match" *> self
  <* ws1
  <* keyword "with"
  >>= fun subject ->
  parse_cases self >>| fun (first, rest) -> EMatch (subject, first, rest)
;;

let parse_expr_function self =
  keyword "function" *> parse_cases self >>| fun (first, rest) -> EFunction (first, rest)
;;

let parse_expr_fun self =
  keyword "fun" *> parse_pattern
  >>= fun parameter ->
  many parse_pattern
  >>= fun parameters ->
  token "->" *> self
  >>| fun body ->
  EFun
    (parameter, List.fold_right ~f:(fun pat acc -> EFun (pat, acc)) parameters ~init:body)
;;

let parse_expr_let self =
  keyword "let" *> parse_rec_flag
  >>= fun rec_flag ->
  parse_value_binding self
  >>= fun first ->
  many (ws1 *> keyword "and" *> parse_value_binding self)
  >>= fun rest ->
  ws1 *> keyword "in" *> self >>| fun body -> ELet (rec_flag, first, rest, body)
;;

let make_expression table =
  fix (fun self ->
    let atom = parse_atom table self in
    let application = parse_expr_application atom in
    let infix = parse_expr_infix table (parse_expr_prefix table atom application) in
    parse_expr_fun self
    <|> (parse_expr_let self
         <|> (parse_expr_function self
              <|> (parse_expr_match self <|> (parse_expr_if self <|> infix)))))
;;

(* ==================== structure ==================== *)

let parse_structure_value self =
  keyword "let" *> parse_rec_flag
  >>= fun rec_flag ->
  parse_value_binding self
  >>= fun first ->
  many (ws1 *> keyword "and" *> parse_value_binding self)
  >>| fun rest -> SValue (rec_flag, first, rest)
;;

let parse_structure_item self = self >>| (fun e -> SEval e) <|> parse_structure_value self

let p_structure parse_expression =
  let psemicolon = many (token ";;") in
  sep_by psemicolon (parse_structure_item parse_expression) <* psemicolon <* ws
;;

(* ==================== entry points ==================== *)

let parse_structure ?(table = Syntax.default_table) input =
  parse_string ~consume:All (p_structure (make_expression table)) input
;;
