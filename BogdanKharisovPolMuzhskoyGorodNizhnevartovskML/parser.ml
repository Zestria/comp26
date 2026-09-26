[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom
open Ast

let ( let* ) = ( >>= )

let of_result print_error = function
  | Ok value -> return value
  | Error error -> fail (print_error error)
;;

module Lexeme = struct
  let starts text =
    peek_string (String.length text) >>| String.equal text <|> return false
  ;;

  let quoted =
    char '"'
    *> fix (fun rest ->
      let* next = any_char in
      match next with
      | '"' -> return ()
      | '\\' -> any_char *> rest
      | _ -> rest)
  ;;

  let comment =
    let is_delimiter_char = function
      | 'a' .. 'z' | '_' -> true
      | _ -> false
    in
    let character =
      let digit =
        satisfy (function
          | '0' .. '9' -> true
          | _ -> false)
      in
      let octal =
        satisfy (function
          | '0' .. '7' -> true
          | _ -> false)
      in
      let escape =
        char '\\'
        *> (satisfy (function
              | '\\' | '"' | '\'' | 'n' | 't' | 'b' | 'r' | ' ' -> true
              | _ -> false)
            <|> digit *> digit *> digit
            <|> char 'o'
                *> satisfy (function
                  | '0' .. '3' -> true
                  | _ -> false)
                *> octal
                *> octal)
      in
      let contents =
        escape
        <|> skip_while (Char.equal '\r') *> char '\n'
        <|> satisfy (function
          | '\\' | '\'' | '\n' | '\r' -> false
          | _ -> true)
      in
      string "''" *> return () <|> char '\'' *> contents *> char '\'' *> return ()
    in
    let quoted_start =
      let extension_name =
        satisfy (fun c -> Name.Syntax.is_identifier_start c || (c >= 'A' && c <= 'Z'))
        *> skip_while Name.Syntax.is_identifier_char
      in
      let extension =
        char '%'
        *> option () (char '%' *> return ())
        *> extension_name
        *> skip_many (char '.' *> extension_name)
        *> skip_while (function
          | ' ' | '\t' | '\012' -> true
          | _ -> false)
      in
      char '{' *> option () extension *> take_while is_delimiter_char <* char '|'
    in
    let quoted_contents delimiter =
      fix (fun rest ->
        skip_while (fun c -> c <> '|')
        *> char '|'
        *> let* candidate = take_while is_delimiter_char in
           let* next = peek_char in
           if next = Some '}' && String.equal candidate delimiter
           then char '}' *> return ()
           else rest)
    in
    fix (fun comment ->
      string "(*"
      *> fix (fun rest ->
        let* close = starts "*)" in
        if close
        then string "*)" *> return ()
        else
          let* nested = starts "(*" in
          if nested
          then comment *> rest
          else
            let* next = peek_char in
            match next with
            | None -> fail "Unclosed comment"
            | Some '"' -> quoted *> rest
            | Some '\'' -> (character <|> any_char *> return ()) *> rest
            | Some '{' ->
              let* delimiter = option None (quoted_start >>| Option.some) in
              (match delimiter with
               | None -> any_char *> rest
               | Some delimiter -> quoted_contents delimiter *> rest)
            | Some c when Name.Syntax.is_identifier_char c && not (c >= '0' && c <= '9')
              -> skip_while Name.Syntax.is_identifier_char *> rest
            | Some _ -> any_char *> rest))
  ;;

  let spaces =
    fix (fun rest ->
      skip_while (function
        | ' ' | '\t' | '\n' | '\r' | '\012' -> true
        | _ -> false)
      *> let* opens = starts "(*" in
         if opens then comment *> rest else return ())
  ;;

  let token parser = spaces *> parser
  let word = token (take_while1 Name.Syntax.is_identifier_char)

  let exact_word expected =
    let* actual = word in
    if actual = expected then return () else fail ("Expected " ^ expected)
  ;;

  let keyword keyword = exact_word (Keyword.to_string keyword)
  let punctuation text = token (string text) *> return ()

  let symbol =
    token
      (let* first = peek_char_fail in
       match first with
       | ':' -> string ":="
       | '!' | '~' | '?' | '#' -> take_while1 Name.Syntax.is_symbol_char_or_hash
       | _ -> take_while1 Name.Syntax.is_symbol_char)
  ;;

  let exact_symbol expected =
    let* actual = symbol in
    if actual = expected then return () else fail ("Expected " ^ expected)
  ;;

  let identifier =
    let* text = word in
    of_result Name.error_to_string (Name.of_identifier text)
  ;;

  let operator =
    let* text =
      keyword Keyword.Mod *> return (Keyword.to_string Keyword.Mod) <|> symbol
    in
    of_result Name.error_to_string (Name.of_operator text)
  ;;

  let integer =
    let* text = word in
    of_result Integer_literal.error_to_string (Integer_literal.of_string text)
  ;;
end

let value_name =
  Lexeme.identifier
  <|> (Lexeme.punctuation "(" *> Lexeme.operator <* Lexeme.punctuation ")")
;;

let tuple make element =
  let* first = element in
  let* rest = many (Lexeme.punctuation "," *> element) in
  return
    (match rest with
     | [] -> first
     | second :: rest -> make first second rest)
;;

let simple_pattern, pattern =
  let tuple_pattern = tuple (fun a b rest -> PTuple (a, b, rest)) in
  let simple =
    fix (fun simple ->
      Lexeme.exact_word "_" *> return PWildcard
      <|> (value_name >>| fun name -> PVar name)
      <|> Lexeme.punctuation "("
          *> (Lexeme.punctuation ")" *> return PUnit
              <|> (tuple_pattern simple <* Lexeme.punctuation ")")))
  in
  simple, tuple_pattern simple
;;

let function_body parameters body =
  match parameters with
  | [] -> body
  | parameter :: parameters -> Fun { parameter; parameters; body }
;;

let binding expression =
  Lexeme.keyword Keyword.Let
  *> commit
  *>
  let* recursion =
    Lexeme.keyword Keyword.Rec *> return Recursive <|> return Nonrecursive
  in
  let named =
    let* name = value_name in
    let* parameters = many simple_pattern in
    Lexeme.exact_symbol "=" *> return (PVar name, parameters)
  in
  let destructuring =
    let* pattern = pattern in
    Lexeme.exact_symbol "=" *> return (pattern, [])
  in
  let* pattern, parameters = named <|> destructuring in
  let* body = commit *> (expression <?> "binding expression") in
  return { recursion; pattern; expression = function_body parameters body }
;;

module Operators = struct
  let max_precedence = 9
  let hash_precedence = max_precedence + 1

  type associativity =
    | Left
    | Right

  type fixity =
    | Prefix of int
    | Infix of int * associativity

  module Table = Map.Make (struct
      type t = Name.t

      let compare a b = String.compare (Name.to_string a) (Name.to_string b)
    end)

  let default name =
    match Name.operator_kind name with
    | Some Prefix -> Some (Prefix 1)
    | Some Hash -> Some (Infix (hash_precedence, Left))
    | Some Power -> Some (Infix (8, Right))
    | Some Multiplicative -> Some (Infix (7, Left))
    | Some Additive -> Some (Infix (6, Left))
    | Some Concatenation -> Some (Infix (5, Right))
    | Some Comparison -> Some (Infix (4, Left))
    | Some Conjunction -> Some (Infix (3, Right))
    | Some Disjunction -> Some (Infix (2, Right))
    | Some Assignment -> Some (Infix (0, Right))
    | None -> None
  ;;

  let find table name =
    match Table.find_opt name table with
    | Some fixity -> Some fixity
    | None -> default name
  ;;

  let infix table precedence =
    let* name = Lexeme.operator in
    match find table name with
    | Some (Infix (actual, associativity)) when actual = precedence ->
      return (name, associativity)
    | _ -> fail "Expected infix operator"
  ;;

  let prefix table =
    let* name = Lexeme.operator in
    match find table name with
    | Some (Prefix arity) -> return (name, arity)
    | _ -> fail "Expected prefix operator"
  ;;

  let unary_minus table =
    let* name = Lexeme.operator in
    match Name.to_string name, find table name with
    | "-", Some (Infix _) -> return ()
    | _ -> fail "Expected unary minus"
  ;;

  let declaration_start =
    option
      None
      (Lexeme.exact_word "infixl" *> return (Some (`Infix Left))
       <|> Lexeme.exact_word "infixr" *> return (Some (`Infix Right))
       <|> Lexeme.exact_word "prefix" *> return (Some `Prefix))
  ;;

  let declaration kind =
    let* literal = Lexeme.integer in
    let* number =
      match int_of_string_opt (Integer_literal.to_string literal) with
      | Some number -> return number
      | None -> fail "Operator declaration number is out of range"
    in
    let* fixity =
      match kind with
      | `Prefix when number >= 1 -> return (Prefix number)
      | `Prefix -> fail "Prefix arity must be positive"
      | `Infix associativity when number >= 0 && number <= max_precedence ->
        return (Infix (number, associativity))
      | `Infix _ ->
        fail (Printf.sprintf "Infix precedence must be between 0 and %d" max_precedence)
    in
    let* name = Lexeme.punctuation "(" *> Lexeme.operator <* Lexeme.punctuation ")" in
    Lexeme.punctuation ";;" *> return (name, fixity)
  ;;
end

let chain combine operand operator =
  let* first = operand in
  let* rest =
    many
      (let* name, associativity = operator in
       let* right = operand in
       return (name, associativity, right))
  in
  match rest with
  | [] -> return first
  | (_, associativity, _) :: _ ->
    if List.exists (fun (_, next, _) -> next <> associativity) rest
    then
      fail "Operators with the same precedence need matching associativity or parentheses"
    else (
      match associativity with
      | Operators.Left ->
        return
          (List.fold_left
             (fun left (name, _, right) -> combine name left right)
             first
             rest)
      | Operators.Right ->
        let last, reversed =
          List.fold_left
            (fun (left, reversed) (name, _, right) -> right, (name, left) :: reversed)
            (first, [])
            rest
        in
        return
          (List.fold_left
             (fun right (name, left) -> combine name left right)
             last
             reversed))
;;

let make_expression operators =
  fix (fun expression ->
    let local_let =
      let* binding = binding expression in
      Lexeme.keyword Keyword.In
      *> commit
      *>
      let* body = expression <?> "expression after in" in
      return (Let (binding, body))
    in
    let function_ =
      Lexeme.keyword Keyword.Fun
      *> commit
      *>
      let* parameter = simple_pattern in
      let* parameters = many simple_pattern in
      Lexeme.exact_symbol "->"
      *>
      let* body = expression <?> "function body" in
      return (Fun { parameter; parameters; body })
    in
    let conditional =
      Lexeme.keyword Keyword.If
      *> commit
      *>
      let* condition = expression <?> "if condition" in
      Lexeme.keyword Keyword.Then
      *>
      let* yes = expression <?> "expression after then" in
      let* no =
        option
          None
          (Lexeme.keyword Keyword.Else
           *> commit
           *> (expression <?> "expression after else")
           >>| Option.some)
      in
      return (If (condition, yes, no))
    in
    let open_expression = local_let <|> function_ <|> conditional in
    let constructor =
      Lexeme.keyword Keyword.True *> return (Bool true)
      <|> Lexeme.keyword Keyword.False *> return (Bool false)
      <|> Lexeme.punctuation "(" *> Lexeme.punctuation ")" *> return Unit
    in
    let to_expression = function
      | `Constructor constant -> Constant constant
      | `Expression expression -> expression
    in
    let atom =
      constructor
      >>| (fun constant -> `Constructor constant)
      <|> (Lexeme.integer >>| fun literal -> `Expression (Constant (Int literal)))
      <|> (value_name >>| fun name -> `Expression (Var name))
      <|> (Lexeme.punctuation "(" *> expression
           <* Lexeme.punctuation ")"
           >>| fun expression -> `Expression expression)
    in
    let prefix =
      fix (fun prefix ->
        (let* name, arity = Operators.prefix operators in
         let* first = prefix in
         let rec arguments remaining accumulated =
           if remaining = 0
           then return (List.rev accumulated)
           else
             let* argument = prefix in
             arguments (remaining - 1) (to_expression argument :: accumulated)
         in
         let* rest = arguments (arity - 1) [] in
         return (`Expression (Apply (Var name, to_expression first, rest))))
        <|> atom)
    in
    let hash =
      chain
        (fun name left right ->
           `Expression (Ast_helper.binary name (to_expression left) (to_expression right)))
        prefix
        (Operators.infix operators Operators.hash_precedence)
    in
    let application =
      let* function_ = hash in
      let argument = hash >>| to_expression in
      let* arguments =
        match function_ with
        | `Constructor _ -> option [] (argument >>| fun argument -> [ argument ])
        | `Expression _ -> many argument
      in
      let function_ = to_expression function_ in
      return
        (match arguments with
         | [] -> function_
         | first :: rest -> Apply (function_, first, rest))
    in
    let unary =
      fix (fun unary ->
        Operators.unary_minus operators *> unary
        >>| Ast_helper.negate
        <|> open_expression
        <|> application)
    in
    let rec infix precedence =
      if precedence > Operators.max_precedence
      then unary
      else
        chain
          Ast_helper.binary
          (infix (precedence + 1))
          (Operators.infix operators precedence)
    in
    let tuple_ = tuple (fun a b rest -> Tuple (a, b, rest)) (infix 1) in
    chain Ast_helper.binary tuple_ (Operators.infix operators 0))
;;

let program ~operator_extension =
  let next_item expression allow_evaluation =
    let definition_or_local =
      let* binding = binding expression in
      let local =
        if allow_evaluation
        then
          Lexeme.keyword Keyword.In *> commit *> (expression <?> "expression after in")
          >>| fun body -> Evaluation (Let (binding, body))
        else fail "Expected a top-level definition"
      in
      local <|> return (Definition binding)
    in
    let evaluation =
      if allow_evaluation
      then expression >>| fun expression -> Evaluation expression
      else fail "Expected let or ;; before a top-level expression"
    in
    Lexeme.punctuation ";;" *> return None
    <|> (definition_or_local <|> evaluation >>| Option.some)
  in
  let rec items operators expression allow_evaluation accumulated =
    Lexeme.spaces
    *>
    let* next = peek_char in
    match next with
    | None -> return (List.rev accumulated)
    | Some _ ->
      let* declaration =
        if operator_extension then Operators.declaration_start else return None
      in
      (match declaration with
       | Some kind ->
         let* name, fixity = Operators.declaration kind in
         let operators = Operators.Table.add name fixity operators in
         items operators (make_expression operators) true accumulated
       | None ->
         let* item = next_item expression allow_evaluation in
         (match item with
          | None -> items operators expression true accumulated
          | Some item -> items operators expression false (item :: accumulated)))
  in
  items Operators.Table.empty (make_expression Operators.Table.empty) true []
;;

let parse parser source = parse_string ~consume:All (parser <* Lexeme.spaces) source
let parse_expression = parse (make_expression Operators.Table.empty)
let parse_program = parse (program ~operator_extension:false)
let parse_program_with_operators = parse (program ~operator_extension:true)
