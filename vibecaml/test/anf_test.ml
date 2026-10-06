[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml
open Anf

let fail message =
  prerr_endline message;
  exit 1
;;

let require condition message = if not condition then fail message

let convert source =
  (* Счётчик принадлежит только этому тесту. В драйвере передадут Fresh.name,
     который продолжит нумерацию после предыдущих проходов. *)
  let counter = ref 0 in
  let fresh base =
    incr counter;
    Printf.sprintf "%s__%d" base !counter
  in
  match Rd_parser.parse_structure Config.default_extensions source with
  | Error message -> fail message
  | Ok program -> Anf_conv.convert ~fresh program
;;

let normalized source =
  match convert source with
  | Ok program -> program
  | Error message -> fail message
;;

let body name program =
  match
    List.find_map
      (fun (_, bindings) ->
         List.find_map
           (fun (bound, _, body) -> if bound = name then Some body else None)
           bindings)
      program
  with
  | Some body -> body
  | None -> fail ("не найдено объявление " ^ name)
;;

let rec computations = function
  | ALet (_, value, body) -> computation value @ computations body
  | ACExpr value -> computation value

and computation value =
  value
  ::
  (match value with
   | CIf (_, yes, no) -> computations yes @ computations no
   | _ -> [])
;;

let call_sizes name program =
  computations (body name program)
  |> List.filter_map (function
    | CApp (ImmId function_, arguments) -> Some (function_, List.length arguments)
    | _ -> None)
;;

let simplified source = Anf_simplify.simplify (normalized source)

let () =
  let program =
    normalized "let f a b = a + b\nlet apply k x y = k x y\nlet main = f 1 2"
  in
  require (List.mem ("f", 2) (call_sizes "main" program)) "арность f потеряна";
  require
    (List.mem ("+", 2) (call_sizes "f" program))
    "оператор разобран на частичные применения";
  require
    (List.mem ("k", 1) (call_sizes "apply" program))
    "параметр k должен принимать один аргумент";
  require
    (List.for_all (fun (_, size) -> size = 1) (call_sizes "apply" program))
    "неизвестная арность получила несколько аргументов";
  let program = normalized "let g b = b\nlet f a = g\nlet main = f 1 2" in
  require
    (List.length (call_sizes "main" program) = 2)
    "лишний аргумент не применён к результату";
  let program = normalized "let f a b = a + b\nlet partial = f 1" in
  require
    (List.mem ("f", 1) (call_sizes "partial" program))
    "частичное применение потеряно";
  let discarded = normalized "let _ = fun ignored -> print_int ignored" in
  require
    (match discarded with
     | [ (Ast.Non_recursive, [ (_, [ "ignored" ], _) ]) ] -> true
     | _ -> false)
    "let _ = fun должен сохранить функцию под свежим именем";
  let program =
    normalized
      "let rec even n = if n = 0 then true else odd (n - 1) and odd m = if m = 0 then \
       false else even (m - 1)"
  in
  require (List.mem ("odd", 1) (call_sizes "even" program)) "взаимная рекурсия потеряна";
  let program = simplified "let f n = if n <= 1 then 2 else 3" in
  require
    (match body "f" program with
     | ACExpr (CIf (CondCmp ("<=", ImmId "n", ImmNum 1), _, _)) -> true
     | _ -> false)
    "целое сравнение вынесено из if";
  let program = simplified "let f x = if x = true then 1 else 2" in
  require
    (List.exists
       (function
         | CApp (ImmId "=", _) -> true
         | _ -> false)
       (computations (body "f" program)))
    "булево сравнение стало CondCmp";
  let program = simplified "let ( <= ) a b = false\nlet f n = if n <= 1 then 2 else 3" in
  require
    (List.exists
       (function
         | CApp (ImmId "<=", _) -> true
         | _ -> false)
       (computations (body "f" program)))
    "переопределённое сравнение стало встроенным";
  let program = simplified "let f (a, (b, c), _) () = let (d, e) = (a, b) in d + e + c" in
  let fields =
    computations (body "f" program)
    |> List.filter_map (function
      | CField (_, index, size) -> Some (index, size)
      | _ -> None)
  in
  require (List.length fields = 6) "вложенные кортежные паттерны потеряли поля";
  require
    (List.for_all (fun (index, size) -> index >= 0 && index < size) fields)
    "неверные индексы кортежа";
  require
    (List.filter (fun (_, size) -> size = 3) fields = [ 0, 3; 1, 3 ])
    "потерян размер тройки с последним wildcard";
  require
    (List.length (List.filter (fun (_, size) -> size = 2) fields) = 4)
    "размер вложенной пары перепутан с внешним кортежем";
  let program = simplified "let f x = 1 + (if x then 2 else 3) + 4" in
  require
    (List.exists
       (function
         | CIf _ -> true
         | _ -> false)
       (computations (body "f" program)))
    "if внутри операнда потерян";
  let program = normalized "let f a = a\nlet main = f (let x = 3 in x + 1)" in
  let smaller = Anf_simplify.simplify program in
  require
    (Anf.count_lets smaller < Anf.count_lets program)
    "уменьшение не убрало лишние let";
  require (Anf_simplify.simplify smaller = smaller) "уменьшение не идемпотентно";
  let program = simplified "let f x = let unused = print_int x in 42" in
  require
    (List.mem ("print_int", 1) (call_sizes "f" program))
    "потерян вызов с побочным эффектом";
  let aliases =
    [ ( Ast.Non_recursive
      , [ ( "f"
          , []
          , ALet ("a", CImmExpr (ImmNum 7), ALet ("b", CImmExpr (ImmId "a"), avar "b")) )
        ] )
    ]
  in
  require
    (body "f" (Anf_simplify.simplify aliases) = aimm (ImmNum 7))
    "цепочка псевдонимов не свёрнута";
  List.iter
    (fun source ->
       require (Result.is_error (convert source)) ("ожидалась ошибка: " ^ source))
    [ "let f x = fun y -> x + y"
    ; "let f x = let rec g y = y in g x"
    ; "let f 1 = 2"
    ; "let rec x = 1"
    ; "let rec _ = fun recursive_arg -> recursive_arg"
    ];
  require (normalized "" = []) "пустая программа не поддерживается";
  print_endline "ANF: структура, арность, условия, паттерны и уменьшение — OK"
;;
