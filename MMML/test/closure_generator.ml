[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type typ =
  | Int
  | Bool
  | Unit
  | Arrow of typ * typ

type term =
  { typ : typ
  ; node : node
  }

and node =
  | Integer of int
  | Boolean of bool
  | Unit_value
  | Variable of string
  | Lambda of Ast.pattern * typ * term
  | Application of term * term
  | Let of Ast.pattern * term * term
  | If of term * term * term
  | Add of term * term
  | Less of term * term

let bind env pattern typ =
  match pattern with
  | Ast.PWildcard -> env
  | Ast.PVariable name -> (name, typ) :: List.remove_assoc name env
;;

let rec default typ =
  let node =
    match typ with
    | Int -> Integer 0
    | Bool -> Boolean false
    | Unit -> Unit_value
    | Arrow (argument, result) -> Lambda (Ast.PWildcard, argument, default result)
  in
  { typ; node }
;;

module Gen = QCheck.Gen

let ( let* ) = Gen.( >>= )

let pattern =
  Gen.oneof_list
    [ Ast.PVariable "x"
    ; Ast.PVariable "y"
    ; Ast.PVariable "f"
    ; Ast.PVariable "env"
    ; Ast.PVariable "self"
    ; Ast.PVariable "closure_0"
    ; Ast.PWildcard
    ]
;;

let binding_type =
  Gen.oneof_list
    [ Int
    ; Bool
    ; Unit
    ; Arrow (Int, Int)
    ; Arrow (Int, Arrow (Int, Int))
    ; Arrow (Arrow (Int, Int), Int)
    ]
;;

let rec generate depth env typ =
  let make node = { typ; node } in
  let base () =
    let literal =
      match typ with
      | Int -> Gen.map (fun value -> make (Integer value)) (Gen.int_range (-10) 10)
      | Bool -> Gen.map (fun value -> make (Boolean value)) Gen.bool
      | Unit -> Gen.return (make Unit_value)
      | Arrow (argument, result) ->
        let* parameter = pattern in
        let* body = generate (max 0 (depth - 1)) (bind env parameter argument) result in
        Gen.return (make (Lambda (parameter, argument, body)))
    in
    let variables =
      List.filter_map
        (fun (name, candidate) ->
           if candidate = typ then Some (make (Variable name)) else None)
        env
    in
    match variables with
    | [] -> literal
    | _ -> Gen.oneof_weighted [ 2, literal; 3, Gen.oneof_list variables ]
  in
  if depth = 0
  then base ()
  else
    let* choice = Gen.int_bound 5 in
    match choice with
    | 0 -> base ()
    | 1 ->
      let* parameter = pattern in
      let* rhs_type = binding_type in
      let* rhs = generate (depth - 1) env rhs_type in
      let* body = generate (depth - 1) (bind env parameter rhs_type) typ in
      Gen.return (make (Let (parameter, rhs, body)))
    | 2 ->
      let* argument_type = binding_type in
      let* callee = generate (depth - 1) env (Arrow (argument_type, typ)) in
      let* argument = generate (depth - 1) env argument_type in
      Gen.return (make (Application (callee, argument)))
    | 3 ->
      let* condition = generate (depth - 1) env Bool in
      let* yes = generate (depth - 1) env typ in
      let* no = generate (depth - 1) env typ in
      Gen.return (make (If (condition, yes, no)))
    | 4 ->
      (match typ with
       | Int | Bool ->
         let* left = generate (depth - 1) env Int in
         let* right = generate (depth - 1) env Int in
         Gen.return (make (if typ = Int then Add (left, right) else Less (left, right)))
       | Unit | Arrow _ -> base ())
    | _ ->
      (* A guaranteed shadowing opportunity, even when the random environment
         is empty: the RHS sees the outer x, while the body sees the inner x. *)
      let* rhs = generate (depth - 1) env Int in
      let x = Ast.PVariable "x" in
      let* body = generate (depth - 1) (bind env x Int) typ in
      let previous_x = { typ = Int; node = Variable "x" } in
      Gen.return (make (Let (x, rhs, make (Let (x, previous_x, body)))))
;;

let rec check env term =
  let check_children = List.for_all (check env) in
  match term.node with
  | Integer _ -> term.typ = Int
  | Boolean _ -> term.typ = Bool
  | Unit_value -> term.typ = Unit
  | Variable name -> List.assoc_opt name env = Some term.typ
  | Lambda (parameter, argument, body) ->
    term.typ = Arrow (argument, body.typ) && check (bind env parameter argument) body
  | Application (callee, argument) ->
    callee.typ = Arrow (argument.typ, term.typ) && check_children [ callee; argument ]
  | Let (pattern, rhs, body) ->
    term.typ = body.typ && check env rhs && check (bind env pattern rhs.typ) body
  | If (condition, yes, no) ->
    condition.typ = Bool
    && yes.typ = term.typ
    && no.typ = term.typ
    && check_children [ condition; yes; no ]
  | Add (left, right) ->
    term.typ = Int && left.typ = Int && right.typ = Int && check_children [ left; right ]
  | Less (left, right) ->
    term.typ = Bool && left.typ = Int && right.typ = Int && check_children [ left; right ]
;;

let well_formed = check []

let rec weight term =
  match term.node with
  | Integer value -> 1 + abs value
  | Boolean value -> if value then 2 else 1
  | Unit_value -> 1
  | Variable _ -> 2
  | Lambda (_, _, body) -> 2 + weight body
  | Application (left, right) | Add (left, right) | Less (left, right) ->
    2 + weight left + weight right
  | Let (_, rhs, body) -> 2 + weight rhs + weight body
  | If (condition, yes, no) -> 2 + weight condition + weight yes + weight no
;;

let rec candidates term =
  let rebuild node = { term with node } in
  let smaller_children =
    match term.node with
    | Integer value -> [ rebuild (Integer (value / 2)) ]
    | Boolean _ | Unit_value | Variable _ -> []
    | Lambda (parameter, argument, body) ->
      List.map
        (fun body -> rebuild (Lambda (parameter, argument, body)))
        (candidates body)
    | Application (callee, argument) ->
      [ callee; argument ]
      @ List.map
          (fun callee -> rebuild (Application (callee, argument)))
          (candidates callee)
      @ List.map
          (fun argument -> rebuild (Application (callee, argument)))
          (candidates argument)
    | Let (pattern, rhs, body) ->
      [ rhs; body ]
      @ List.map (fun rhs -> rebuild (Let (pattern, rhs, body))) (candidates rhs)
      @ List.map (fun body -> rebuild (Let (pattern, rhs, body))) (candidates body)
    | If (condition, yes, no) ->
      [ yes; no ]
      @ List.map
          (fun condition -> rebuild (If (condition, yes, no)))
          (candidates condition)
      @ List.map (fun yes -> rebuild (If (condition, yes, no))) (candidates yes)
      @ List.map (fun no -> rebuild (If (condition, yes, no))) (candidates no)
    | Add (left, right) ->
      [ left; right ]
      @ List.map (fun left -> rebuild (Add (left, right))) (candidates left)
      @ List.map (fun right -> rebuild (Add (left, right))) (candidates right)
    | Less (left, right) ->
      List.map (fun left -> rebuild (Less (left, right))) (candidates left)
      @ List.map (fun right -> rebuild (Less (left, right))) (candidates right)
  in
  List.filter
    (fun smaller -> smaller.typ = term.typ && weight smaller < weight term)
    (default term.typ :: smaller_children)
;;

let shrinks term = List.filter well_formed (candidates term)

let rec expression term =
  match term.node with
  | Integer value -> Ast.Constant (Ast.Integer value)
  | Boolean value -> Ast.Constant (Ast.Boolean value)
  | Unit_value -> Ast.Constant Ast.Unit
  | Variable name -> Ast.Variable name
  | Lambda (parameter, _, body) -> Ast.Lambda (parameter, expression body)
  | Application (callee, argument) ->
    Ast.Application (expression callee, expression argument)
  | Let (pattern, rhs, body) ->
    Ast.Let_in (Ast.Nonrecursive, pattern, expression rhs, expression body)
  | If (condition, yes, no) ->
    Ast.If_then_else (expression condition, expression yes, expression no)
  | Add (left, right) -> Ast.infix "+" (expression left) (expression right)
  | Less (left, right) -> Ast.infix "<" (expression left) (expression right)
;;

let program term =
  [ Ast.Definition (Ast.Nonrecursive, Ast.PVariable "result", expression term) ]
;;

let arbitrary =
  let generator =
    let* depth = Gen.int_range 1 4 in
    let* typ = Gen.oneof_list [ Int; Bool; Unit ] in
    generate depth [] typ
  in
  QCheck.make
    ~print:(fun term -> Ast.show_program (program term))
    ~shrink:(fun term -> QCheck.Iter.of_list (shrinks term))
    generator
;;
