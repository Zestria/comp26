[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml
open Ast

let unwrap = function
  | Ok value -> value
  | Error message -> failwith message
;;

let parse source = unwrap (Rd_parser.parse_structure Config.default_extensions source)

let rec pattern_names = function
  | Pat_var name -> [ name ]
  | Pat_tuple (first, second, rest) ->
    List.concat_map pattern_names (first :: second :: rest)
  | Pat_constant _ | Pat_wildcard -> []
;;

let binding_names bindings =
  List.concat_map (fun binding -> pattern_names binding.binding_pattern) bindings
;;

(* Проверка отдельно от реализации: здесь вычисляем свободные имена через
   передачу области видимости, а не копируем алгоритм CC с его окружением. *)
let rec free bound = function
  | Expr_ident name -> if List.mem name bound then [] else [ name ]
  | Expr_constant _ -> []
  | Expr_fun (first, rest, body) ->
    free (List.concat_map pattern_names (first :: rest) @ bound) body
  | Expr_let (flag, first, rest, body) ->
    let bindings = first :: rest in
    let inside = binding_names bindings @ bound in
    let rhs_bound = if flag = Recursive then inside else bound in
    List.concat_map (fun binding -> free rhs_bound binding.binding_expression) bindings
    @ free inside body
  | Expr_app (f, a) -> free bound f @ free bound a
  | Expr_if (c, t, e) -> free bound c @ free bound t @ free bound e
  | Expr_tuple (first, second, rest) ->
    List.concat_map (free bound) (first :: second :: rest)
;;

let check_unique program =
  let rec expression = function
    | Expr_fun (first, rest, body) ->
      List.concat_map pattern_names (first :: rest) @ expression body
    | Expr_let (_, first, rest, body) -> bindings (first :: rest) @ expression body
    | Expr_app (f, a) -> expression f @ expression a
    | Expr_if (c, t, e) -> expression c @ expression t @ expression e
    | Expr_tuple (first, second, rest) ->
      List.concat_map expression (first :: second :: rest)
    | Expr_constant _ | Expr_ident _ -> []
  and bindings values =
    binding_names values
    @ List.concat_map (fun binding -> expression binding.binding_expression) values
  in
  let names =
    List.concat_map
      (fun (Structure_value (_, first, rest)) -> bindings (first :: rest))
      program
  in
  assert (List.length names = List.length (List.sort_uniq String.compare names))
;;

let check_closed program =
  let rec expression globals = function
    | Expr_fun (first, rest, body) ->
      let parameters = List.concat_map pattern_names (first :: rest) in
      assert (free (parameters @ globals) body = []);
      expression globals body
    | Expr_let (flag, first, rest, body) ->
      (* Имена локальной рекурсивной группы после lambda lifting станут
         глобальными, поэтому после CC они могут быть свободны в fun. *)
      let globals =
        match flag with
        | Recursive -> binding_names (first :: rest) @ globals
        | Non_recursive -> globals
      in
      List.iter
        (fun binding -> expression globals binding.binding_expression)
        (first :: rest);
      expression globals body
    | Expr_app (f, a) ->
      expression globals f;
      expression globals a
    | Expr_if (c, t, e) ->
      expression globals c;
      expression globals t;
      expression globals e
    | Expr_tuple (first, second, rest) ->
      List.iter (expression globals) (first :: second :: rest)
    | Expr_ident _ | Expr_constant _ -> ()
  in
  ignore
    (List.fold_left
       (fun globals (Structure_value (flag, first, rest)) ->
          let bindings = first :: rest in
          let next = binding_names bindings @ globals in
          let available = if flag = Recursive then next else globals in
          List.iter
            (fun binding -> expression available binding.binding_expression)
            bindings;
          next)
       Lexicon.builtins
       program)
;;

let check_anf_unique program =
  let rec expression = function
    | Anf.ALet (name, rhs, body) -> name :: (action rhs @ expression body)
    | Anf.ACExpr rhs -> action rhs
  and action = function
    | Anf.CIf (_, yes, no) -> expression yes @ expression no
    | _ -> []
  in
  let names =
    List.concat_map
      (fun (_, bindings) ->
         List.concat_map
           (fun (name, parameters, body) -> name :: (parameters @ expression body))
           bindings)
      program
  in
  assert (List.length names = List.length (List.sort_uniq String.compare names))
;;

let () =
  let examples =
    [ Samples.fack_source ^ "\n" ^ Samples.fibk_source
    ; "let f x = x + 1 let g x = x * 2"
    ; "let f x = let y = x in fun z -> y + z"
    ; "let make (x, (y, _)) = fun z -> x + y + z"
    ; "let make x = fun y -> fun z -> x + y + z"
    ; "let x = 5 let f n = let x = n and y = x in fun z -> x + y + z"
    ; "let make x = if x > 0 then (fun y -> x + y) else (fun y -> x - y)"
    ; "let make x = let f y = x + y and g y = y * 2 in fun z -> f (g z)"
    ; "let rec f x = if x = 0 then 0 else g (x - 1) and g x = f x"
    ; "let (+) a b = a - b let (&&) x y = x || y"
    ; "let lifted__1 = 5 let make x = fun x -> x + lifted__1"
    ; "let use print_int = fun n -> print_int n"
    ; "let f x = let rec g y = x + y in g 1"
    ; "let fac n = let rec helper n k = if n <= 1 then k 1 else helper (n - 1) (fun r -> \
       k (r * n)) in helper n (fun x -> x)"
    ; "let check s n = let rec even k = if k = 0 then true else odd (k - s) and odd k = \
       if k = 0 then false else even (k - s) in even n"
    ; "let outer a = let rec f x = let rec g y = a + x + y in g x in f 1"
    ]
  in
  List.iter
    (fun source ->
       Fresh.reset ();
       let renamed = unwrap (Rename.rename (parse source)) in
       check_unique renamed;
       let cc = unwrap (Closure_conv.convert renamed) in
       check_unique cc;
       check_closed cc;
       let lifted = unwrap (Lambda_lift.lift cc) in
       check_unique lifted;
       check_closed lifted;
       let anf = unwrap (Anf_conv.convert lifted) in
       check_anf_unique anf;
       check_anf_unique (Anf_simplify.simplify anf))
    examples;
  Fresh.reset ();
  let renamed = unwrap (Rename.rename (parse "let x = 1 let f x = let x = x + 1 in x")) in
  assert (
    Pprint.string_of_structure renamed
    = "let x = 1\nlet f x__1 = let x__2 = x__1 + 1 in x__2");
  let operator =
    unwrap (Rename.rename (parse "let (+) a b = a - b let result = 9 + 2"))
  in
  assert (not (String.contains (Pprint.string_of_structure operator) '+'));
  (* В локальном let rec справа допускается только функция. *)
  let local = unwrap (Rename.rename (parse "let f x = let rec y = x in y")) in
  assert (Result.is_error (Closure_conv.convert local))
;;
