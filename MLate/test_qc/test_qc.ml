[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck
open MLate_frontend.Ast

let gen_name = Gen.map (Printf.sprintf "x%d") (Gen.int_bound 10)

let gen_binding value =
  Gen.map3
    (fun flag name value -> { flag; name; value })
    (Gen.oneof_list [ Rec; Nonrec ])
    gen_name
    value
;;

let rec gen_expr depth =
  let open Gen in
  let leaf = oneof [ map (fun n -> Int n) nat; map (fun x -> Var x) gen_name ] in
  if depth = 0
  then leaf
  else (
    let sub = gen_expr (depth - 1) in
    oneof
      [ leaf
      ; map2 (fun x body -> Fun (x, body)) gen_name sub
      ; map2 (fun f x -> App (f, x)) sub sub
      ; map3
          (fun op l r -> Binop (op, l, r))
          (oneof_list [ Plus; Minus; Times; Lt ])
          sub
          sub
      ; map3 (fun c t e -> If (c, t, e)) sub sub sub
      ; map2 (fun binding body -> Let (binding, body)) (gen_binding sub) sub
      ])
;;

let rec print_expr = function
  | Int n -> string_of_int n
  | Var x -> x
  | Fun (x, body) -> Printf.sprintf "(fun %s -> %s)" x (print_expr body)
  | App (f, x) -> Printf.sprintf "(%s %s)" (print_expr f) (print_expr x)
  | Binop (op, l, r) ->
    let op =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Lt -> "<"
    in
    Printf.sprintf "(%s %s %s)" (print_expr l) op (print_expr r)
  | If (c, t, e) ->
    Printf.sprintf "(if %s then %s else %s)" (print_expr c) (print_expr t) (print_expr e)
  | Let (binding, body) ->
    Printf.sprintf "(%s in %s)" (print_binding binding) (print_expr body)

and print_binding { flag; name; value } =
  let rec_keyword = if flag = Rec then "rec" else "" in
  Printf.sprintf "let %s %s = %s" rec_keyword name (print_expr value)
;;

let print_program program = String.concat "\n" (List.map print_binding program)

let rec shrink_expr =
  let open Iter in
  function
  | Int 0 | Var _ -> empty
  | Int _ -> return (Int 0)
  | Fun (name, body) ->
    return body <+> map (fun body -> Fun (name, body)) (shrink_expr body)
  | App (l, r) ->
    of_list [ l; r ]
    <+> map (fun l' -> App (l', r)) (shrink_expr l)
    <+> map (fun r' -> App (l, r')) (shrink_expr r)
  | Binop (op, l, r) ->
    of_list [ l; r ]
    <+> map (fun l' -> Binop (op, l', r)) (shrink_expr l)
    <+> map (fun r' -> Binop (op, l, r')) (shrink_expr r)
  | If (c, t, e) ->
    of_list [ c; t; e ]
    <+> map (fun c -> If (c, t, e)) (shrink_expr c)
    <+> map (fun t -> If (c, t, e)) (shrink_expr t)
    <+> map (fun e -> If (c, t, e)) (shrink_expr e)
  | Let (binding, body) ->
    of_list [ binding.value; body ]
    <+> map (fun value -> Let ({ binding with value }, body)) (shrink_expr binding.value)
    <+> map (fun body -> Let (binding, body)) (shrink_expr body)
;;

let shrink_binding binding =
  Iter.map (fun value -> { binding with value }) (shrink_expr binding.value)
;;

let arbitrary_program =
  make
    ~print:print_program
    ~shrink:(Shrink.list ~shrink:shrink_binding)
    Gen.(list_size (int_bound 4) (gen_binding (sized (fun n -> gen_expr (min 5 n)))))
;;

let () =
  QCheck_runner.run_tests_main
    (List.map
       (fun (name, parse) ->
          Test.make
            ~count:1000
            ~name:(name ^ ": parse(print(ast)) = ast")
            arbitrary_program
            (fun ast -> parse (print_program ast) = Ok ast))
       [ "Angstrom", MLate_frontend.Topdown.parse; "Menhir", MLate_frontend.Parse.parse ])
;;
