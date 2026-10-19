[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Fuel and effect traces are shared by interpreter closures and must survive runtime errors, so the test oracle keeps them in local references. *)
[@@@zanuda "-mutability_check"]

type runtime_error =
  | Division_by_zero
  | Unbound_name of string
  | Expected_integer
  | Expected_boolean
  | Not_callable
  | Non_ground_result
  | Fuel_exhausted
  | Unsupported_recursion

type observation =
  { outcome : (Ast.constant, runtime_error) result
  ; trace : int list
  }

exception Runtime of runtime_error

type value =
  | Scalar of Ast.constant
  | Native of (value -> value)

let integer = function
  | Scalar (Ast.Integer value) -> value
  | Scalar (Ast.Boolean _ | Ast.Unit) | Native _ -> raise (Runtime Expected_integer)
;;

let boolean = function
  | Scalar (Ast.Boolean value) -> value
  | Scalar (Ast.Integer _ | Ast.Unit) | Native _ -> raise (Runtime Expected_boolean)
;;

let bind env pattern value =
  match pattern with
  | Ast.PWildcard -> env
  | Ast.PVariable name -> (name, value) :: env
;;

let lookup env name =
  match List.assoc_opt name env with
  | Some value -> value
  | None -> raise (Runtime (Unbound_name name))
;;

let external_bindings trace =
  let binary operation =
    Native (fun a -> Native (fun b -> Scalar (operation (integer a) (integer b))))
  in
  let arithmetic operation = binary (fun a b -> Ast.Integer (operation a b)) in
  let comparison operation = binary (fun a b -> Ast.Boolean (operation a b)) in
  [ "+", arithmetic ( + )
  ; "-", arithmetic ( - )
  ; "*", arithmetic ( * )
  ; "/", arithmetic (fun a b -> if b = 0 then raise (Runtime Division_by_zero) else a / b)
  ; "=", comparison ( = )
  ; "<>", comparison ( <> )
  ; "<", comparison ( < )
  ; "<=", comparison ( <= )
  ; ">", comparison ( > )
  ; ">=", comparison ( >= )
  ; "~-", Native (fun value -> Scalar (Ast.Integer (-integer value)))
  ; "~+", Native (fun value -> Scalar (Ast.Integer (integer value)))
  ; "service", Native (fun value -> Scalar (Ast.Integer (100 + integer value)))
  ; ( "tick"
    , Native
        (fun value ->
          trace := integer value :: !trace;
          value) )
  ]
;;

let external_names = List.map fst (external_bindings (ref []))

let observe fuel run =
  let remaining = ref fuel in
  let trace = ref [] in
  let tick () =
    if !remaining <= 0 then raise (Runtime Fuel_exhausted);
    decr remaining
  in
  let outcome =
    try
      match run tick (external_bindings trace) with
      | Scalar constant -> Ok constant
      | Native _ -> Error Non_ground_result
    with
    | Runtime error -> Error error
  in
  { outcome; trace = List.rev !trace }
;;

let source ?(fuel = 100_000) ~result_name definitions =
  observe fuel (fun tick externals ->
    let rec eval env expression =
      tick ();
      match expression with
      | Ast.Constant constant -> Scalar constant
      | Ast.Variable name -> lookup env name
      | Ast.Lambda (parameter, body) ->
        Native (fun argument -> eval (bind env parameter argument) body)
      | Ast.Application (callee, argument) ->
        let callee = eval env callee in
        let argument = eval env argument in
        (match callee with
         | Native function_ -> function_ argument
         | Scalar _ -> raise (Runtime Not_callable))
      | Ast.If_then_else (condition, yes, no) ->
        if boolean (eval env condition) then eval env yes else eval env no
      | Ast.Let_in (flag, pattern, rhs, body) ->
        let value = binding_value env flag pattern rhs in
        eval (bind env pattern value) body
    and binding_value env flag pattern rhs =
      match flag, pattern, rhs with
      | Ast.Nonrecursive, _, rhs -> eval env rhs
      | Ast.Recursive, Ast.PVariable name, Ast.Lambda (parameter, body) ->
        let rec function_ argument =
          let recursive_env = (name, Native function_) :: env in
          eval (bind recursive_env parameter argument) body
        in
        Native function_
      | Ast.Recursive, _, _ -> raise (Runtime Unsupported_recursion)
    in
    let final_env =
      List.fold_left
        (fun env (Ast.Definition (flag, pattern, rhs)) ->
           let value = binding_value env flag pattern rhs in
           bind env pattern value)
        externals
        definitions
    in
    lookup final_env result_name)
;;

let show_observation { outcome; trace } =
  let result =
    match outcome with
    | Ok value -> Ast.show_constant value
    | Error error ->
      (match error with
       | Division_by_zero -> "division by zero"
       | Unbound_name name -> "unbound " ^ name
       | Expected_integer -> "expected integer"
       | Expected_boolean -> "expected boolean"
       | Not_callable -> "not callable"
       | Non_ground_result -> "non-ground result"
       | Fuel_exhausted -> "fuel exhausted"
       | Unsupported_recursion -> "unsupported recursion")
  in
  Printf.sprintf
    "%s; trace=[%s]"
    result
    (String.concat "; " (List.map string_of_int trace))
;;
