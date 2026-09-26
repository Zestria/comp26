(* SPDX-License-Identifier: LGPL-3.0-or-later *)

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
      char '{'
      *> option () extension
      *> take_while (function
        | 'a' .. 'z' | '_' -> true
        | _ -> false)
      <* char '|'
    in
    let quoted_contents delimiter =
      let closing = "|" ^ delimiter ^ "}" in
      fix (fun rest ->
        let* closes = starts closing in
        if closes then string closing *> return () else any_char *> rest)
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

  let keyword expected =
    let* actual = word in
    if actual = expected then return () else fail ("Expected " ^ expected)
  ;;

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
    let* text = keyword "mod" *> return "mod" <|> symbol in
    of_result Name.error_to_string (Name.of_operator text)
  ;;

  let operator_where predicate =
    let* name = operator in
    if predicate (Name.to_string name) then return name else fail "Expected operator"
  ;;

  let value_name = identifier <|> (punctuation "(" *> operator <* punctuation ")")

  let integer =
    let* text = word in
    of_result Integer_literal.error_to_string (Integer_literal.of_string text)
  ;;
end

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
      Lexeme.keyword "_" *> return PWildcard
      <|> (Lexeme.value_name >>| fun name -> PVar name)
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
  Lexeme.keyword "let"
  *>
  let* recursion = Lexeme.keyword "rec" *> return Recursive <|> return Nonrecursive in
  let named =
    let* name = Lexeme.value_name in
    let* parameters = many simple_pattern in
    Lexeme.exact_symbol "=" *> return (PVar name, parameters)
  in
  let destructuring =
    let* pattern = pattern in
    Lexeme.exact_symbol "=" *> return (pattern, [])
  in
  let* pattern, parameters = named <|> destructuring in
  let* body = expression in
  return { recursion; pattern; expression = function_body parameters body }
;;

let binary name left right = Apply (Var name, left, [ right ])

let chain_left_with combine operand operator =
  let rec rest left =
    (let* name = operator in
     let* right = operand in
     rest (combine name left right))
    <|> return left
  in
  operand >>= rest
;;

let chain_left = chain_left_with binary

let chain_right operand operator =
  fix (fun expression ->
    let* left = operand in
    (let* name = operator in
     let* right = expression in
     return (binary name left right))
    <|> return left)
;;

let negate = function
  | Constant (Int literal) ->
    let text = Integer_literal.to_string literal in
    let text =
      if text.[0] = '-' then String.sub text 1 (String.length text - 1) else "-" ^ text
    in
    let* literal =
      of_result Integer_literal.error_to_string (Integer_literal.of_string text)
    in
    return (Constant (Int literal))
  | expression ->
    let* name = of_result Name.error_to_string (Name.of_operator "~-") in
    return (Apply (Var name, expression, []))
;;

let expression =
  fix (fun expression ->
    let local_let =
      let* binding = binding expression in
      Lexeme.keyword "in"
      *>
      let* body = expression in
      return (Let (binding, body))
    in
    let function_ =
      Lexeme.keyword "fun"
      *>
      let* parameter = simple_pattern in
      let* parameters = many simple_pattern in
      Lexeme.exact_symbol "->"
      *>
      let* body = expression in
      return (Fun { parameter; parameters; body })
    in
    let conditional =
      Lexeme.keyword "if"
      *>
      let* condition = expression in
      Lexeme.keyword "then"
      *>
      let* yes = expression in
      let* no = option None (Lexeme.keyword "else" *> expression >>| Option.some) in
      return (If (condition, yes, no))
    in
    let open_expression = local_let <|> function_ <|> conditional in
    let constructor =
      Lexeme.keyword "true" *> return (Bool true)
      <|> Lexeme.keyword "false" *> return (Bool false)
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
      <|> (Lexeme.value_name >>| fun name -> `Expression (Var name))
      <|> (Lexeme.punctuation "(" *> expression
           <* Lexeme.punctuation ")"
           >>| fun expression -> `Expression expression)
    in
    let prefix =
      fix (fun prefix ->
        (let* name =
           Lexeme.operator_where (fun s ->
             s <> "!=" && (s.[0] = '!' || s.[0] = '~' || s.[0] = '?'))
         in
         let* argument = prefix in
         return (`Expression (Apply (Var name, to_expression argument, []))))
        <|> atom)
    in
    let hash =
      chain_left_with
        (fun name left right ->
           `Expression (binary name (to_expression left) (to_expression right)))
        prefix
        (Lexeme.operator_where (fun s -> s.[0] = '#'))
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
        Lexeme.exact_symbol "-" *> unary >>= negate <|> open_expression <|> application)
    in
    let power =
      chain_right unary (Lexeme.operator_where (String.starts_with ~prefix:"**"))
    in
    let product =
      chain_left
        power
        (Lexeme.operator_where (fun s ->
           s = "mod"
           || (s.[0] = '*' && not (String.starts_with ~prefix:"**" s))
           || s.[0] = '/'
           || s.[0] = '%'))
    in
    let sum =
      chain_left product (Lexeme.operator_where (fun s -> s.[0] = '+' || s.[0] = '-'))
    in
    let concatenation =
      chain_right sum (Lexeme.operator_where (fun s -> s.[0] = '@' || s.[0] = '^'))
    in
    let comparison =
      chain_left
        concatenation
        (Lexeme.operator_where (fun s ->
           s = "!="
           || (s <> "&"
               && s <> "&&"
               && s <> "||"
               &&
               match s.[0] with
               | '=' | '<' | '>' | '|' | '&' | '$' -> true
               | _ -> false)))
    in
    let conjunction =
      chain_right comparison (Lexeme.operator_where (fun s -> s = "&&" || s = "&"))
    in
    let disjunction =
      chain_right conjunction (Lexeme.operator_where (String.equal "||"))
    in
    let tuple_ = tuple (fun a b rest -> Tuple (a, b, rest)) disjunction in
    chain_right tuple_ (Lexeme.operator_where (String.equal ":=")))
;;

let program =
  let next_item allow_evaluation =
    let definition_or_local =
      let* binding = binding expression in
      let local =
        if allow_evaluation
        then
          Lexeme.keyword "in" *> expression
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
  let rec items allow_evaluation accumulated =
    Lexeme.spaces
    *>
    let* next = peek_char in
    match next with
    | None -> return (List.rev accumulated)
    | Some _ ->
      let* item = next_item allow_evaluation in
      (match item with
       | None -> items true accumulated
       | Some item -> items false (item :: accumulated))
  in
  items true []
;;

let parse parser source = parse_string ~consume:All (parser <* Lexeme.spaces) source
let parse_expression = parse expression
let parse_program = parse program
