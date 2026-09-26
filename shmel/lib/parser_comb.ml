[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Top-down (combinator) parser built with Angstrom. *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let spaces1 = take_while1 is_space *> return ()

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keywords = [ "if"; "then"; "else"; "let"; "rec"; "in"; "fun" ]

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let chainl1 p op =
  p
  >>= fun x ->
  many (lift2 (fun f y -> f, y) op p) >>| List.fold_left (fun acc (f, y) -> f acc y) x
;;

let make_parser ~extended =
  let identifier =
    lift2
      (fun first rest -> String.make 1 first ^ rest)
      (satisfy is_lower)
      (take_while is_ident_char)
    >>= fun s -> if List.mem s keywords then fail "keyword" else return s
  in
  let binder_name =
    lift2
      (fun first rest -> String.make 1 first ^ rest)
      (satisfy is_lower)
      (take_while is_ident_char)
    >>= fun s -> if (not extended) && List.mem s keywords then fail "keyword" else return s
  in
  let integer = take_while1 is_digit >>| int_of_string in
  let keyword s =
    spaces
    *> string s
    *>
    if extended
    then spaces
    else
      peek_char
      >>= function
      | Some c when is_ident_char c -> fail "keyword followed by identifier char"
      | _ -> spaces
  in
  fix (fun expr ->
    let rec unary_atom ~var_name () =
      spaces
      *> choice
           [ (char '-' *> peek_char
              >>= function
              | Some c when (not (is_space c)) && c <> ')' ->
                unary_atom ~var_name () >>| fun e -> Ast.Neg e
              | _ -> fail "not a unary minus")
           ; atom ~var_name
           ]
    and atom ~var_name =
      spaces
      *> choice
           [ char '(' *> spaces *> expr <* spaces <* char ')'
           ; (integer >>| fun n -> Ast.Const n)
           ; (keyword "if" *> expr
              >>= fun cond ->
              keyword "then" *> expr
              >>= fun t1 -> keyword "else" *> expr >>| fun t2 -> Ast.If (cond, t1, t2))
           ; (keyword "let" *> keyword "rec" *> binder_name
              >>= fun fname ->
              many (spaces1 *> binder_name)
              >>= fun params ->
              keyword "=" *> expr
              >>= fun body ->
              keyword "in" *> expr
              >>| fun in_expr ->
              match params with
              | [] -> Ast.Let (fname, body, in_expr)
              | param :: rest ->
                let full_body = List.fold_right (fun p b -> Ast.Abs (p, b)) rest body in
                Ast.LetRec (fname, param, full_body, in_expr))
           ; (keyword "let" *> binder_name
              >>= fun name ->
              many (spaces1 *> binder_name)
              >>= fun params ->
              keyword "=" *> expr
              >>= fun e1 ->
              keyword "in" *> expr
              >>| fun e2 ->
              let full_body = List.fold_right (fun p b -> Ast.Abs (p, b)) params e1 in
              Ast.Let (name, full_body, e2))
           ; (var_name >>| fun v -> Ast.Var v)
           ]
      <* spaces
    in
    let mult_op =
      spaces
      *> choice
           [ string "*" *> return (fun l r -> Ast.BinOp (Ast.Mul, l, r))
           ; string "/" *> return (fun l r -> Ast.BinOp (Ast.Div, l, r))
           ]
      <* spaces
    in
    let add_op =
      spaces
      *> choice
           [ string "+" *> return (fun l r -> Ast.BinOp (Ast.Add, l, r))
           ; string "-" *> return (fun l r -> Ast.BinOp (Ast.Sub, l, r))
           ]
      <* spaces
    in
    let cmp_op =
      spaces
      *> choice
           [ string "<=" *> return (fun l r -> Ast.BinOp (Ast.Le, l, r))
           ; string ">=" *> return (fun l r -> Ast.BinOp (Ast.Ge, l, r))
           ; string "=" *> return (fun l r -> Ast.BinOp (Ast.Eq, l, r))
           ; string "<" *> return (fun l r -> Ast.BinOp (Ast.Lt, l, r))
           ; string ">" *> return (fun l r -> Ast.BinOp (Ast.Gt, l, r))
           ]
      <* spaces
    in
    let app =
      lift2
        (fun head args -> List.fold_left (fun l r -> Ast.App (l, r)) head args)
        (atom ~var_name:binder_name)
        (many (unary_atom ~var_name:identifier ()))
    in
    let unary =
      fix (fun unary ->
        choice [ (spaces *> char '-' *> unary >>| fun e -> Ast.Neg e); app ])
    in
    let mult = chainl1 unary mult_op in
    let add = chainl1 mult add_op in
    let cmp = chainl1 add cmp_op in
    choice
      [ (keyword "fun"
         *> many1 (spaces *> binder_name <* spaces)
         >>= fun vars ->
         string "->" *> cmp >>| fun body -> List.fold_right (fun v b -> Ast.Abs (v, b)) vars body)
      ; cmp
      ])
;;

let parser_impl = make_parser ~extended:false
let parser_impl_extended = make_parser ~extended:true

let parse_gen p str =
  match Angstrom.parse_string p ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;

let parse str = parse_gen parser_impl str
let parse_extended str = parse_gen parser_impl_extended str
