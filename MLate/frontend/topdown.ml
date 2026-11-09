[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let keywords = [ "let"; "rec"; "in"; "fun"; "if"; "then"; "else" ]
let is_keyword name = List.mem name keywords

let first_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keyword_ahead =
  choice
    (List.map
       (fun word ->
          peek_string (String.length word)
          >>= fun prefix -> if prefix = word then return true else fail "not a keyword")
       keywords)
  <|> return false
;;

let keyword ~glued_keywords s =
  string s *> peek_char
  >>= function
  | Some c when (not glued_keywords) && is_identifier_char c ->
    fail (Printf.sprintf "keyword %S cannot be followed by an identifier character" s)
  | _ -> spaces
;;

let identifier ~glued_keywords =
  let letter predicate =
    (if glued_keywords
     then keyword_ahead >>= fun found -> if found then fail "keyword" else return ()
     else return ())
    *> satisfy predicate
  in
  let rest =
    if glued_keywords
    then
      many (letter is_identifier_char) >>| fun chars -> String.of_seq (List.to_seq chars)
    else take_while is_identifier_char
  in
  letter first_letter
  >>= fun first_c ->
  rest
  >>= fun rest ->
  let name = String.make 1 first_c ^ rest in
  if is_keyword name
  then fail (Printf.sprintf "keyword %S cannot be used as an identifier" name)
  else spaces *> return name
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let number ~glued_keywords =
  take_while1 is_digit
  >>= fun digits ->
  (if glued_keywords then keyword_ahead else return false)
  >>= fun followed_by_keyword ->
  peek_char
  >>= function
  | Some c when is_identifier_char c && not followed_by_keyword ->
    fail "integer literal cannot be followed by an identifier character"
  | _ ->
    (match int_of_string_opt digits with
     | Some n -> spaces *> return n
     | None -> fail "integer literal is out of range")
;;

let symbol s = string s *> spaces

let chain_left term op =
  term
  >>= fun first ->
  many (op >>= fun o -> term >>| fun right -> o, right)
  >>| fun rest ->
  List.fold_left (fun left (o, right) -> Binop (o, left, right)) first rest
;;

let desugar_fun args body = List.fold_right (fun arg acc -> Fun (arg, acc)) args body

let binding ~glued_keywords expr =
  let keyword = keyword ~glued_keywords in
  let identifier = identifier ~glued_keywords in
  keyword "let" *> option Nonrec (keyword "rec" *> return Rec)
  >>= fun flag ->
  identifier
  >>= fun name ->
  many identifier
  >>= fun args ->
  symbol "=" *> expr >>| fun value -> { flag; name; value = desugar_fun args value }
;;

let make_expr ~glued_keywords =
  let keyword = keyword ~glued_keywords in
  let identifier = identifier ~glued_keywords in
  let number = number ~glued_keywords in
  fix (fun parser ->
    let atom =
      choice
        [ (number >>| fun n -> Int n)
        ; (identifier >>| fun name -> Var name)
        ; symbol "(" *> parser <* symbol ")"
        ]
    in
    let application =
      atom
      >>= fun head ->
      many atom >>| fun args -> List.fold_left (fun fn arg -> App (fn, arg)) head args
    in
    let multiplicative = chain_left application (choice [ symbol "*" *> return Times ]) in
    let additive =
      chain_left
        multiplicative
        (choice [ symbol "+" *> return Plus; symbol "-" *> return Minus ])
    in
    let comparison =
      additive
      >>= fun left ->
      option left (symbol "<" *> additive >>| fun right -> Binop (Lt, left, right))
    in
    let conditional =
      keyword "if" *> parser
      >>= fun cond ->
      keyword "then" *> parser
      >>= fun then_ -> keyword "else" *> parser >>| fun else_ -> If (cond, then_, else_)
    in
    let abstraction =
      keyword "fun" *> many1 identifier
      >>= fun args -> symbol "->" *> parser >>| fun body -> desugar_fun args body
    in
    let let_expr =
      binding ~glued_keywords parser
      >>= fun declaration -> keyword "in" *> parser >>| fun body -> Let (declaration, body)
    in
    choice [ let_expr; conditional; abstraction; comparison ])
;;

let program = many (binding ~glued_keywords:false (make_expr ~glued_keywords:false))
let glued_program = many (binding ~glued_keywords:true (make_expr ~glued_keywords:true))

let parse_with ~glued_keywords str =
  let program = if glued_keywords then glued_program else program in
  match Angstrom.parse_string (spaces *> program) ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;

let parse = parse_with ~glued_keywords:false
