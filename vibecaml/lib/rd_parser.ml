(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type token_kind =
  | Int of string
  | Ident of string
  | Underscore
  | Operator of string
  | Let
  | Rec
  | And
  | In
  | Fun
  | If
  | Then
  | Else
  | True
  | False
  | Lparen
  | Rparen
  | Comma
  | Equal
  | Arrow
  | Eof

type token =
  { kind : token_kind
  ; offset : int
  }

(* Смещение в исходнике и описание. В строку «строка:колонка» смещение
   переводит [run]: так формат ошибок совпадает с menhir-парсером. *)
exception Parse_error of int * string

let fail offset message = raise (Parse_error (offset, message))

let is_space = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false
;;

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let is_operator_char = function
  | '!'
  | '$'
  | '%'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '|'
  | '~' -> true
  | _ -> false
;;

let word_kind offset = function
  | "let" -> Let
  | "rec" -> Rec
  | "and" -> And
  | "in" -> In
  | "fun" -> Fun
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "true" -> True
  | "false" -> False
  | word when List.mem word Lexicon.reserved -> fail offset (Lexicon.reserved_error word)
  | "_" -> Underscore
  | word when List.mem word Lexicon.operators -> Operator word
  | word -> Ident word
;;

(* Оператор читается целиком, как в OCaml: [+-] --- один неизвестный
   оператор, а не [+] и [-]. *)
let operator_kind offset = function
  | "=" -> Equal
  | "->" -> Arrow
  | name when List.mem name Lexicon.operators -> Operator name
  | name -> fail offset (Lexicon.operator_error name)
;;

(* Ключевые слова по убыванию длины: первое подходящее даёт самое длинное разбиение. *)
let glued_keywords_desc =
  [ "false", False
  ; "then", Then
  ; "else", Else
  ; "true", True
  ; "let", Let
  ; "rec", Rec
  ; "and", And
  ; "fun", Fun
  ; "in", In
  ; "if", If
  ]
;;

let is_keyword_kind = function
  | Let | Rec | And | In | Fun | If | Then | Else | True | False -> true
  | _ -> false
;;

(* Слипшиеся с ключевыми словами идентификаторы: [letx] --- это [let] [x].
   Слово разделяется по самому длинному ключевому слову-префиксу.
   Остаток разбирается рекурсивно: [letrecx] --- это
   [let] [rec] [x]. Результат --- список токенов в прямом порядке. *)
let rec split_glued_front offset word =
  let word_length = String.length word in
  match
    List.find_opt
      (fun (keyword, _) ->
         let length = String.length keyword in
         length < word_length
         && String.sub word 0 length = keyword
         &&
         match word.[length] with
         | 'a' .. 'z' | '_' -> true
         | _ -> false)
      glued_keywords_desc
  with
  | Some (keyword, kind) ->
    let length = String.length keyword in
    let rest = String.sub word length (word_length - length) in
    { kind; offset } :: split_glued_front (offset + length) rest
  | None -> [ { kind = word_kind offset word; offset } ]
;;

(* То же с конца: [ythen] --- это [y] [then]. Токены в прямом порядке. *)
let rec split_glued_back offset word =
  let word_length = String.length word in
  match
    List.find_opt
      (fun (keyword, _) ->
         let length = String.length keyword in
         length < word_length
         && String.sub word (word_length - length) length = keyword
         &&
         match word.[word_length - length - 1] with
         | 'a' .. 'z' | '_' -> true
         | _ -> false)
      glued_keywords_desc
  with
  | Some (keyword, kind) ->
    let length = String.length keyword in
    let rest = String.sub word 0 (word_length - length) in
    let keyword_token = { kind; offset = offset + word_length - length } in
    split_glued_back offset rest @ [ keyword_token ]
  | None -> [ { kind = word_kind offset word; offset } ]
;;

(* Регион --- непрерывный кусок исходника; кандидаты --- его возможные
   токенизации. У обычных токенов и слов без ключевых слов внутри один
   кандидат, у «склеенных» слов --- несколько. *)
type region =
  { offset : int
  ; candidates : token list list
  }

(* Допустимые разбиения одного слова: самое жадное с начала, потом с конца,
   потом целиком идентификатором. Неудачные варианты (остаток оказался
   зарезервированным словом и т. п.) пропускаются. Если не годится ни один,
   сообщение об ошибке берём от классификации всего слова. *)
let word_candidates offset word =
  let front =
    try split_glued_front offset word with
    | Parse_error _ -> []
  in
  let back =
    try split_glued_back offset word with
    | Parse_error _ -> []
  in
  let whole =
    try [ { kind = word_kind offset word; offset } ] with
    | Parse_error _ -> []
  in
  let rec dedup seen = function
    | [] -> List.rev seen
    | candidate :: rest when List.mem candidate seen -> dedup seen rest
    | candidate :: rest -> dedup (candidate :: seen) rest
  in
  match dedup [] [ front; back; whole ] with
  | [] ->
    (match word_kind offset word with
     | _ -> assert false
     | exception Parse_error (_, message) -> fail offset message)
  | candidates -> candidates
;;

(* В режиме склейки после числа можно оставить ключевое слово: [1in] --- это
   [1] [in]. Если хвост с ключевого слова не начинается, литерал остаётся
   ошибкой, как и без расширения. *)
let number_glued_keyword_tokens glue_kw finish glued source =
  if not glue_kw
  then None
  else (
    let tail = String.sub source finish (glued - finish) in
    match split_glued_front finish tail with
    | exception Parse_error _ -> None
    | [] -> None
    | first :: _ as tokens -> if is_keyword_kind first.kind then Some tokens else None)
;;

(* Лексер отдельным проходом выкидывает пробелы и комментарии, грамматика
   видит только токены. При [glue_kw] слова расщепляются по ключевым словам:
   [letx] --- это [let] [x], [1in] --- это [1] [in]. Расщепление обязательным
   не бывает: слово, которое можно оставить идентификатором ([android]), так
   и остаётся, потому что разбор перебирает кандидатов каждого слова.
   Завершающий [Eof] отдаётся отдельно от регионов: у него нет кандидатов, а
   состояние разбора строится от него, поэтому непустое по типу. *)
let lex glue_kw source =
  let length = String.length source in
  let pair_at index =
    if index + 1 < length then Some (source.[index], source.[index + 1]) else None
  in
  let rec skip_comment start depth index =
    if index >= length
    then fail start Lexicon.unclosed_comment_error
    else (
      match pair_at index with
      | Some ('(', '*') -> skip_comment start (depth + 1) (index + 2)
      | Some ('*', ')') ->
        if depth = 1 then index + 2 else skip_comment start (depth - 1) (index + 2)
      | _ -> skip_comment start depth (index + 1))
  in
  let rec take_while predicate index =
    if index < length && predicate source.[index]
    then take_while predicate (index + 1)
    else index
  in
  let rec loop index regions =
    if index >= length
    then List.rev regions, { kind = Eof; offset = length }
    else if is_space source.[index]
    then loop (index + 1) regions
    else if pair_at index = Some ('(', '*')
    then loop (skip_comment index 1 (index + 2)) regions
    else (
      let single kind finish =
        let token = { kind; offset = index } in
        loop finish ({ offset = index; candidates = [ [ token ] ] } :: regions)
      in
      let text finish = String.sub source index (finish - index) in
      match source.[index] with
      | '(' -> single Lparen (index + 1)
      | ')' -> single Rparen (index + 1)
      | ',' -> single Comma (index + 1)
      | '0' .. '9' ->
        let finish = take_while Lexicon.is_digit index in
        (* [1x], [1L], [0x1F], [1_000] и [1.5] для OCaml --- один литерал, а не
           число и что-то следом. *)
        let glued = take_while (fun c -> is_identifier_char c || c = '.') finish in
        if glued > finish
        then (
          let literal = text glued in
          match number_glued_keyword_tokens glue_kw finish glued source with
          | Some tokens ->
            let digits = String.sub source index (finish - index) in
            let digit = { kind = Int digits; offset = index } in
            let candidates = [ [ digit ] @ tokens ] in
            loop glued ({ offset = index; candidates } :: regions)
          | None -> fail index (Lexicon.literal_error literal))
        else single (Int (text finish)) finish
      | 'a' .. 'z' | '_' ->
        let finish = take_while is_identifier_char index in
        let word = text finish in
        let region =
          if glue_kw
          then { offset = index; candidates = word_candidates index word }
          else (
            let token = { kind = word_kind index word; offset = index } in
            { offset = index; candidates = [ [ token ] ] })
        in
        loop finish (region :: regions)
      | 'A' .. 'Z' ->
        let finish = take_while is_identifier_char index in
        fail index (Lexicon.constructor_error (text finish))
      | character when is_operator_char character ->
        let finish = take_while is_operator_char index in
        single (operator_kind index (text finish)) finish
      | character -> fail index (Lexicon.unexpected_char_error character))
  in
  loop 0 []
;;

(* Состояние разбора --- оставшиеся токены: текущий и хвост. Непустота
   задана типом; последний токен всегда [Eof], его добавляет [run]. *)
type tokens = token * token list

let current ((token, _) : tokens) = token

(* Токен на [distance] впереди текущего. За [Eof] ничего нет, там снова [Eof]. *)
let rec lookahead ((token, rest) : tokens) distance =
  match rest with
  | next :: rest when distance > 0 -> lookahead (next, rest) (distance - 1)
  | _ -> token.kind
;;

let next_kind tokens = lookahead tokens 1

(* Чтение за [Eof] снова даёт [Eof]: последний токен не снимается. *)
let take ((token, rest) : tokens) =
  match rest with
  | next :: rest -> token, (next, rest)
  | [] -> token, (token, [])
;;

let skip tokens =
  let (_ : token), tokens = take tokens in
  tokens
;;

let expect tokens kind =
  let token, tokens = take tokens in
  if token.kind <> kind then fail token.offset "получен неожиданный токен";
  tokens
;;

let binding pattern expression =
  { binding_pattern = pattern; binding_expression = expression }
;;

let int_constant offset digits =
  match int_of_string_opt digits with
  | Some value -> Const_int value
  | None -> fail offset (Lexicon.int_overflow_error digits)
;;

(* Отрицательная константа из цифр целиком: так проходит [min_int], модуль
   которого не помещается в [int]. *)
let negative_int_constant offset digits =
  match int_of_string_opt ("-" ^ digits) with
  | Some value -> Const_int value
  | None -> fail offset (Lexicon.int_overflow_error digits)
;;

(* Приоритеты — данные, а не ветви грамматики. В #5 эту таблицу можно будет
   заменить после чтения объявлений операторов. Уровни и ассоциативность ---
   как в OCaml. *)
let operator_table : (string list * int * Lexicon.associativity) list =
  [ [ "||" ], 10, Right
  ; [ "&&" ], 20, Right
  ; [ "="; "<>"; "<"; "<="; ">"; ">=" ], 30, Left
  ; [ "+"; "-"; "+."; "-." ], 40, Left
  ; [ "*"; "/"; "*."; "/."; "mod"; "land"; "lor"; "lxor" ], 50, Left
  ; [ "**"; "lsl"; "lsr"; "asr" ], 60, Right
  ]
;;

let operator_info name =
  List.find_map
    (fun (names, precedence, associativity) ->
       if List.mem name names then Some (precedence, associativity) else None)
    operator_table
;;

let starts_pattern = function
  | Int _ | Ident _ | Underscore | True | False | Lparen -> true
  | _ -> false
;;

let starts_atom = function
  | Int _ | Ident _ | True | False | Lparen -> true
  | _ -> false
;;

(* Оператор в скобках, [( + )], --- это имя. Только минус смотрит на токен
   вперёд: [(-1)] начинается так же, как [( - )]. После остальных операторов
   обязана идти скобка, как у menhir-парсера: ошибка --- там же, где у него. *)
let parenthesized_operator tokens =
  match (current tokens).kind, next_kind tokens with
  | Operator "-", Rparen -> Some ("-", skip (skip tokens))
  | Operator "-", _ -> None
  | Operator name, _ ->
    let tokens = expect (skip tokens) Rparen in
    Some (name, tokens)
  | Equal, _ ->
    let tokens = expect (skip tokens) Rparen in
    Some ("=", tokens)
  | _ -> None
;;

(* Простой паттерн: без кортежа на верхнем уровне. *)
let rec parse_pattern tokens =
  match take tokens with
  | { kind = Int digits; offset }, tokens ->
    Pat_constant (int_constant offset digits), tokens
  | { kind = True; _ }, tokens -> Pat_constant (Const_bool true), tokens
  | { kind = False; _ }, tokens -> Pat_constant (Const_bool false), tokens
  | { kind = Underscore; _ }, tokens -> Pat_wildcard, tokens
  | { kind = Ident name; _ }, tokens -> Pat_var name, tokens
  | { kind = Lparen; _ }, tokens -> parse_parenthesized_pattern tokens
  | token, _ -> fail token.offset "ожидался паттерн"

(* Отрицательная константа в паттерне --- только в скобках: [(-1)]. *)
and parse_parenthesized_pattern tokens =
  match parenthesized_operator tokens with
  | Some (name, tokens) -> Pat_var name, tokens
  | None ->
    (match (current tokens).kind, next_kind tokens with
     | Rparen, _ -> Pat_constant Const_unit, skip tokens
     | Operator "-", Int digits ->
       let token, tokens = take (skip tokens) in
       let tokens = expect tokens Rparen in
       Pat_constant (negative_int_constant token.offset digits), tokens
     | _ ->
       let first, tokens = parse_pattern tokens in
       let pattern, tokens = parse_pattern_tuple tokens first in
       let tokens = expect tokens Rparen in
       pattern, tokens)

(* Кортеж без скобок из простых паттернов, если после [first] идёт запятая. *)
and parse_pattern_tuple tokens first =
  let rec rest values tokens =
    match (current tokens).kind with
    | Comma ->
      let pattern, tokens = parse_pattern (skip tokens) in
      rest (pattern :: values) tokens
    | _ -> List.rev values, tokens
  in
  match rest [] tokens with
  | [], tokens -> first, tokens
  | second :: others, tokens -> Pat_tuple (first, second, others), tokens
;;

let parse_rec_flag tokens =
  match (current tokens).kind with
  | Rec -> Recursive, skip tokens
  | _ -> Non_recursive, tokens
;;

let is_operator_name = function
  | Operator _ | Equal -> true
  | _ -> false
;;

(* Имя слева от параметров: [f] или [( + )], как [val_ident] в OCaml. *)
let parse_value_name tokens =
  match (current tokens).kind with
  | Ident name -> Some (name, skip tokens)
  | Lparen when is_operator_name (next_kind tokens) && lookahead tokens 2 = Rparen ->
    parenthesized_operator (skip tokens)
  | _ -> None
;;

(* Кортеж без скобок: [let], [fun] и [else] тянутся через запятую, поэтому
   элементы разбираются без кортежа, а тело [let]/[fun]/[else] --- с ним. *)
let rec parse_expression tokens =
  let first, tokens = parse_expr_no_tuple tokens in
  let rec rest values tokens =
    match (current tokens).kind with
    | Comma ->
      let expression, tokens = parse_expr_no_tuple (skip tokens) in
      rest (expression :: values) tokens
    | _ -> List.rev values, tokens
  in
  match rest [] tokens with
  | [], tokens -> first, tokens
  | second :: others, tokens -> Expr_tuple (first, second, others), tokens

(* [let], [fun] и [if] не бывают операндом оператора: [1 + if ...] вне ядра. *)
and parse_expr_no_tuple tokens =
  match (current tokens).kind with
  | Let -> parse_let tokens
  | Fun -> parse_fun tokens
  | If -> parse_if tokens
  | _ -> parse_infix tokens 0

and parse_let_bindings tokens =
  let flag, tokens = parse_rec_flag tokens in
  let first, tokens = parse_value_binding tokens in
  let rec rest values tokens =
    match (current tokens).kind with
    | And ->
      let binding, tokens = parse_value_binding (skip tokens) in
      rest (binding :: values) tokens
    | _ -> List.rev values, tokens
  in
  let others, tokens = rest [] tokens in
  (flag, first, others), tokens

and parse_let tokens =
  let (flag, first, others), tokens = parse_let_bindings (skip tokens) in
  let tokens = expect tokens In in
  let body, tokens = parse_expression tokens in
  Expr_let (flag, first, others, body), tokens

(* [let f x y = e] --- сахар для [let f = fun x y -> e]. Параметры бывают
   только после имени, иначе слева паттерн, возможно кортеж. *)
and parse_value_binding tokens =
  let (pattern, parameters), tokens =
    match parse_value_name tokens with
    | Some (name, tokens) when starts_pattern (current tokens).kind ->
      let parameters, tokens = parse_parameters tokens Equal in
      (Pat_var name, parameters), tokens
    | Some (name, tokens) ->
      let pattern, tokens = parse_pattern_tuple tokens (Pat_var name) in
      (pattern, []), tokens
    | None ->
      let first, tokens = parse_pattern tokens in
      let pattern, tokens = parse_pattern_tuple tokens first in
      (pattern, []), tokens
  in
  let tokens = expect tokens Equal in
  let expression, tokens = parse_expression tokens in
  let expression =
    match parameters with
    | [] -> expression
    | first :: rest -> Expr_fun (first, rest, expression)
  in
  binding pattern expression, tokens

and parse_parameters tokens stop =
  let rec parameters values tokens =
    match (current tokens).kind with
    | kind when kind = stop -> List.rev values, tokens
    | kind when starts_pattern kind ->
      let pattern, tokens = parse_pattern tokens in
      parameters (pattern :: values) tokens
    | _ -> fail (current tokens).offset "ожидался параметр"
  in
  parameters [] tokens

and parse_fun tokens =
  match parse_parameters (skip tokens) Arrow with
  | [], tokens -> fail (current tokens).offset "ожидался параметр"
  | first :: rest, tokens ->
    let tokens = expect tokens Arrow in
    let body, tokens = parse_expression tokens in
    Expr_fun (first, rest, body), tokens

and parse_if tokens =
  let condition, tokens = parse_expression (skip tokens) in
  let tokens = expect tokens Then in
  let then_branch, tokens = parse_expression tokens in
  let tokens = expect tokens Else in
  let else_branch, tokens = parse_expression tokens in
  Expr_if (condition, then_branch, else_branch), tokens

and parse_infix tokens minimum =
  let left, tokens = parse_unary tokens in
  let rec continue left tokens =
    let token = current tokens in
    let name =
      match token.kind with
      | Operator name -> Some name
      | Equal -> Some "="
      | _ -> None
    in
    match Option.bind name operator_info with
    | Some (precedence, association) when precedence >= minimum ->
      let next =
        match association with
        | Left -> precedence + 1
        | Right -> precedence
      in
      let right, tokens = parse_infix (skip tokens) next in
      continue (Expr_app (Expr_app (Expr_ident (Option.get name), left), right)) tokens
    | _ -> left, tokens
  in
  continue left tokens

(* Унарный минус сильнее бинарных операторов, но слабее применения: [-f x]
   --- это [~- (f x)]. Минус перед целой константой сворачивается в
   константу, как в OCaml; скобки прозрачны, [-(1)] тоже число. *)
and parse_unary tokens =
  match (current tokens).kind, next_kind tokens with
  | Operator "-", Int digits when Option.is_none (int_of_string_opt digits) ->
    let token, tokens = take (skip tokens) in
    Expr_constant (negative_int_constant token.offset digits), tokens
  | Operator "-", _ ->
    let operand, tokens = parse_unary (skip tokens) in
    let expression =
      match operand with
      | Expr_constant (Const_int value) -> Expr_constant (Const_int (-value))
      | expression -> Expr_app (Expr_ident "~-", expression)
    in
    expression, tokens
  | _ -> parse_application tokens

(* [true], [false] и [()] для OCaml --- конструкторы, их применение к
   нескольким аргументам не разбирается. Поэтому без скобок они не бывают
   функцией: [true x] вне ядра, [(true) x] --- можно. *)
and parse_application tokens =
  let constructor =
    match (current tokens).kind, next_kind tokens with
    | (True | False), _ | Lparen, Rparen -> true
    | _ -> false
  in
  let first, tokens = parse_atom tokens in
  if constructor && starts_atom (current tokens).kind
  then fail (current tokens).offset "конструктор нельзя применить";
  let rec arguments function_ tokens =
    if starts_atom (current tokens).kind
    then (
      let argument, tokens = parse_atom tokens in
      arguments (Expr_app (function_, argument)) tokens)
    else function_, tokens
  in
  arguments first tokens

and parse_atom tokens =
  match take tokens with
  | { kind = Int digits; offset }, tokens ->
    Expr_constant (int_constant offset digits), tokens
  | { kind = True; _ }, tokens -> Expr_constant (Const_bool true), tokens
  | { kind = False; _ }, tokens -> Expr_constant (Const_bool false), tokens
  | { kind = Ident name; _ }, tokens -> Expr_ident name, tokens
  | { kind = Lparen; _ }, tokens -> parse_parenthesized_expression tokens
  | token, _ -> fail token.offset "ожидалось выражение"

and parse_parenthesized_expression tokens =
  match parenthesized_operator tokens with
  | Some (name, tokens) -> Expr_ident name, tokens
  | None ->
    (match (current tokens).kind with
     | Rparen -> Expr_constant Const_unit, skip tokens
     | _ ->
       let expression, tokens = parse_expression tokens in
       let tokens = expect tokens Rparen in
       expression, tokens)
;;

(* Позиция как у menhir-парсера: строка и колонка с единицы. *)
let location source offset =
  let before = String.sub source 0 (min offset (String.length source)) in
  let line = List.length (String.split_on_char '\n' before) in
  let line_start =
    match String.rindex_opt before '\n' with
    | Some index -> index + 1
    | None -> 0
  in
  Printf.sprintf "%d:%d" line (offset - line_start + 1)
;;

(* Упёрлись в конец ввода --- сообщение то же, что у menhir-парсера. *)
let run glue_kw parse source =
  let error offset message =
    let message =
      if offset >= String.length source then Lexicon.end_of_input_error else message
    in
    Error (Printf.sprintf "%s: %s" (location source offset) message)
  in
  match lex glue_kw source with
  | exception Parse_error (offset, message) -> error offset message
  | lexed ->
    let regions, eof = lexed in
    let parse_tokens tokens =
      try
        let result, tokens = parse tokens in
        match current tokens with
        | { kind = Eof; _ } -> Ok result
        | { offset; _ } -> error offset "лишний текст после программы"
      with
      | Parse_error (offset, message) -> error offset message
    in
    (* Первая комбинация --- жадная, первый кандидат каждого слова. Если она
       не разбирается, перебираем остальные и берём первый успешный разбор.
       Перебор полный, для «обычных» слов кандидат один, так что обычно это
       ровно один проход. При полном провале отдаём ошибку жадной комбинации:
       каждый уровень сохраняет ошибку первого кандидата, поэтому наверх доходит
       ошибка самого первого листа обхода. *)
    let rec search acc_tokens pending =
      match pending with
      | [] ->
        (* [acc_tokens] развёрнут, так что свёртка собирает состояние с конца,
           начиная с [Eof]. *)
        List.fold_left
          (fun (current, rest) token -> token, current :: rest)
          (eof, [])
          acc_tokens
        |> parse_tokens
      | region :: rest ->
        let rec try_candidates = function
          | [] -> Error "нет кандитатов для разбора"
          | candidate :: more ->
            (match search (List.rev_append candidate acc_tokens) rest with
             | Ok _ as ok -> ok
             | Error _ as first_error ->
               (match try_candidates more with
                | Ok _ as ok -> ok
                | Error _ -> first_error))
        in
        try_candidates region.candidates
    in
    search [] regions
;;

let check_extensions extensions =
  if extensions.Parser_intf.operator_declarations
  then Error "расширение --ext-operator-declarations пока не реализовано"
  else Ok ()
;;

let with_extensions extensions parse source =
  match check_extensions extensions with
  | Error _ as error -> error
  | Ok () -> run extensions.Parser_intf.glued_keywords parse source
;;

let parse_expr extensions source = with_extensions extensions parse_expression source

let parse_structure extensions source =
  with_extensions
    extensions
    (fun tokens ->
       let rec items values tokens =
         match (current tokens).kind with
         | Eof -> List.rev values, tokens
         | Let ->
           let (flag, first, rest), tokens = parse_let_bindings (skip tokens) in
           items (Structure_value (flag, first, rest) :: values) tokens
         | _ -> fail (current tokens).offset "в структуре ожидалось let"
       in
       items [] tokens)
    source
;;
