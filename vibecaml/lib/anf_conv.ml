[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Anf

type function_info =
  { arity : int option
  ; builtin : bool
  }

exception Invalid_input of string

let unknown = { arity = None; builtin = false }

let builtin_environment =
  List.map (fun name -> name, { arity = Some 2; builtin = true }) Lexicon.operators
  @ [ "~-", { arity = Some 1; builtin = true }
    ; "print_int", { arity = Some 1; builtin = true }
    ]
;;

let info environment name =
  match List.assoc_opt name environment with
  | Some info -> info
  | None -> unknown
;;

let rec pattern_names = function
  | Pat_var name -> [ name ]
  | Pat_tuple (first, second, rest) ->
    List.concat_map pattern_names (first :: second :: rest)
  | Pat_wildcard | Pat_constant _ -> []
;;

let hide_patterns environment patterns =
  List.map (fun name -> name, unknown) (List.concat_map pattern_names patterns)
  @ environment
;;

let collect_application expression =
  let rec collect arguments = function
    | Expr_app (function_, argument) -> collect (argument :: arguments) function_
    | function_ -> function_, arguments
  in
  collect [] expression
;;

let integer_expression environment = function
  | Expr_constant (Const_int _) -> true
  | expression ->
    let function_, arguments = collect_application expression in
    (match function_ with
     | Expr_ident name when (info environment name).builtin ->
       (List.length arguments = if name = "~-" then 1 else 2)
       && List.mem
            name
            [ "+"
            ; "-"
            ; "*"
            ; "/"
            ; "mod"
            ; "land"
            ; "lor"
            ; "lxor"
            ; "lsl"
            ; "lsr"
            ; "asr"
            ; "~-"
            ]
     | _ -> false)
;;

let convert ~fresh program =
  let bind computation continuation =
    let name = fresh "t" in
    ALet (name, computation, continuation (ImmId name))
  in
  let rec bind_pattern pattern value body =
    match pattern with
    | Pat_var name -> ALet (name, CImmExpr value, body)
    | Pat_wildcard | Pat_constant Const_unit -> body
    | Pat_constant (Const_int _ | Const_bool _) ->
      raise (Invalid_input "ANF: числовые и булевы паттерны пока не поддерживаются")
    | Pat_tuple (first, second, rest) ->
      List.fold_right
        (fun (index, pattern) body ->
           match pattern with
           | Pat_wildcard | Pat_constant Const_unit -> body
           | Pat_var name -> ALet (name, CField (value, index), body)
           | _ ->
             let name = fresh "field" in
             ALet (name, CField (value, index), bind_pattern pattern (ImmId name) body))
        (List.mapi (fun index pattern -> index, pattern) (first :: second :: rest))
        body
  in
  (* Продолжение описывает место, куда надо подставить полученный атом.
     В ветках if оно не дублируется: результат связывается один раз. *)
  let rec normalize environment expression continuation =
    match expression with
    | Expr_constant (Const_int n) -> continuation (ImmNum n)
    | Expr_constant (Const_bool b) -> continuation (ImmBool b)
    | Expr_constant Const_unit -> continuation ImmUnit
    | Expr_ident name -> continuation (ImmId name)
    | Expr_fun _ ->
      raise (Invalid_input "ANF: вложенная fun, сначала нужен lambda lifting")
    | Expr_let (Recursive, _, _, _) ->
      raise (Invalid_input "ANF: локальный let rec, сначала нужен lambda lifting")
    | Expr_let (Non_recursive, first, rest, body) ->
      let bindings = first :: rest in
      let patterns = List.map (fun binding -> binding.binding_pattern) bindings in
      let expressions = List.map (fun binding -> binding.binding_expression) bindings in
      normalize_arguments environment expressions (fun values ->
        let body = normalize (hide_patterns environment patterns) body continuation in
        List.fold_right
          (fun (pattern, value) body -> bind_pattern pattern value body)
          (List.combine patterns values)
          body)
    | Expr_tuple (first, second, rest) ->
      normalize_arguments environment (first :: second :: rest) (function
        | first :: second :: rest -> bind (CTuple (first, second, rest)) continuation
        | _ -> assert false)
    | Expr_if (condition, yes, no) ->
      normalize_condition environment condition (fun condition ->
        let yes = normalize environment yes aimm in
        let no = normalize environment no aimm in
        bind (CIf (condition, yes, no)) continuation)
    | Expr_app _ ->
      let function_, arguments = collect_application expression in
      (match function_, arguments with
       | Expr_ident operator, [ left; right ]
         when (operator = "&&" || operator = "||") && (info environment operator).builtin
         ->
         normalize environment left (fun left ->
           let right = normalize environment right aimm in
           let yes, no =
             if operator = "&&"
             then right, aimm (ImmBool false)
             else aimm (ImmBool true), right
           in
           bind (CIf (CondImm left, yes, no)) continuation)
       | _ -> normalize_application environment function_ arguments continuation)
  and normalize_arguments environment expressions continuation =
    (* Сначала хвост, но атомы возвращаем в исходном порядке. У f (print 1)
       (print 2) эффекты идут 2, 1, а позиции аргументов не меняются. *)
    match expressions with
    | [] -> continuation []
    | expression :: rest ->
      normalize_arguments environment rest (fun values ->
        normalize environment expression (fun value -> continuation (value :: values)))
  and normalize_condition environment expression continuation =
    match collect_application expression with
    | Expr_ident operator, [ left; right ]
      when (info environment operator).builtin
           && List.mem operator [ "="; "<>"; "<"; "<="; ">"; ">=" ]
           && (integer_expression environment left || integer_expression environment right)
      ->
      normalize environment right (fun right ->
        normalize environment left (fun left ->
          continuation (CondCmp (operator, left, right))))
    | _ -> normalize environment expression (fun value -> continuation (CondImm value))
  and normalize_application environment function_ arguments continuation =
    let arity =
      match function_ with
      | Expr_ident name ->
        (match (info environment name).arity with
         | Some n -> n
         | None -> 1)
      | _ -> 1
    in
    let rec split count first rest =
      match count, rest with
      | 0, _ | _, [] -> List.rev first, rest
      | _, argument :: rest -> split (count - 1) (argument :: first) rest
    in
    let first, rest = split arity [] arguments in
    (* Лишние аргументы применяются к результату по одному. Внешний аргумент
       вычисляется до внутреннего вызова — это важно, если тот печатает. *)
    let rec call_outer reversed continuation =
      match reversed with
      | [] ->
        normalize_arguments environment first (fun values ->
          normalize environment function_ (fun function_ ->
            bind (CApp (function_, values)) continuation))
      | argument :: rest ->
        normalize environment argument (fun value ->
          call_outer rest (fun function_ ->
            bind (CApp (function_, [ value ])) continuation))
    in
    call_outer (List.rev rest) continuation
  in
  let signature binding =
    match binding.binding_pattern, binding.binding_expression with
    | Pat_var name, Expr_fun (_, rest, _) ->
      name, { arity = Some (1 + List.length rest); builtin = false }
    | _ ->
      raise
        (Invalid_input
           "ANF: let rec верхнего уровня должен связывать только именованные функции")
  in
  let convert_binding environment binding =
    match binding.binding_pattern, binding.binding_expression with
    | Pat_var name, Expr_fun (first, rest, body) ->
      let patterns = first :: rest in
      let parameters =
        List.map
          (function
            | Pat_var name -> name
            | _ -> fresh "arg")
          patterns
      in
      let body = normalize (hide_patterns environment patterns) body aimm in
      let body =
        List.fold_right
          (fun (pattern, name) body ->
             match pattern with
             | Pat_var _ -> body
             | _ -> bind_pattern pattern (ImmId name) body)
          (List.combine patterns parameters)
          body
      in
      (name, parameters, body), []
    | pattern, expression ->
      let name =
        match pattern with
        | Pat_var name -> name
        | _ -> fresh "value"
      in
      let body = normalize environment expression aimm in
      let projections =
        match pattern with
        | Pat_var _ -> []
        | _ ->
          let rec collect = function
            | ALet (name, computation, body) ->
              (Non_recursive, [ name, [], ACExpr computation ]) :: collect body
            | ACExpr _ -> []
          in
          collect (bind_pattern pattern (ImmId name) (aimm ImmUnit))
      in
      (name, [], body), projections
  in
  let rec convert_items environment = function
    | [] -> []
    | Structure_value (flag, first, rest) :: items ->
      let bindings = first :: rest in
      let signatures =
        List.concat_map
          (fun binding ->
             match binding.binding_expression with
             | Expr_fun _ -> [ signature binding ]
             | _ ->
               List.map
                 (fun name -> name, unknown)
                 (pattern_names binding.binding_pattern))
          bindings
      in
      let rhs_environment =
        match flag with
        | Non_recursive -> environment
        | Recursive -> List.map signature bindings @ environment
      in
      let converted = List.map (convert_binding rhs_environment) bindings in
      (flag, List.map fst converted)
      :: (List.concat_map snd converted @ convert_items (signatures @ environment) items)
  in
  try Ok (convert_items builtin_environment program) with
  | Invalid_input message -> Error message
;;
