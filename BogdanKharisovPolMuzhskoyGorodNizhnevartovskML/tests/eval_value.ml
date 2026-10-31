[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_middleend
module Address_map = Map.Make (Int)
module Address_set = Set.Make (Int)

type t = int

type kind =
  | Integer
  | Boolean
  | Unit_value
  | Tuple_value
  | Function_value

type runtime_error =
  | Division_by_zero
  | Functional_comparison

type invariant_error =
  | Uninitialized_value of t
  | Unexpected_value of kind * kind
  | Field_out_of_bounds of int * int
  | Incompatible_comparison of kind * kind
  | Unbound_variable of Id.t
  | Unknown_code of Id.t
  | Capture_arity of int * int
  | Call_arity of int * int

type error =
  | Runtime of runtime_error
  | Invalid_ir of invariant_error
  | Unsupported_integer of Bkml_frontend.Integer_literal.t
  | Heap_exhausted

type heap =
  { next : int
  ; nodes : node Address_map.t
  ; output : string list
  }

and node =
  | Int of int
  | Bool of bool
  | Unit
  | Tuple of t list
  | Function of (t -> t evaluation)

and 'a evaluation = heap -> ('a * heap, error) result

let runtime_error_to_string = function
  | Division_by_zero -> "Division_by_zero"
  | Functional_comparison -> "compare: functional value"
;;

let kind_to_string = function
  | Integer -> "integer"
  | Boolean -> "boolean"
  | Unit_value -> "unit"
  | Tuple_value -> "tuple"
  | Function_value -> "function"
;;

let invariant_error_to_string = function
  | Uninitialized_value address ->
    Printf.sprintf "Value at address %d read before initialization" address
  | Unexpected_value (expected, actual) ->
    Printf.sprintf
      "Expected %s, found %s"
      (kind_to_string expected)
      (kind_to_string actual)
  | Field_out_of_bounds (index, length) ->
    Printf.sprintf "Field index %d outside tuple of size %d" index length
  | Incompatible_comparison (a, b) ->
    Printf.sprintf "Cannot compare %s with %s" (kind_to_string a) (kind_to_string b)
  | Unbound_variable id -> "Unbound variable " ^ Id.name id
  | Unknown_code id -> "Unknown code " ^ Id.name id
  | Capture_arity (expected, actual) ->
    Printf.sprintf "Expected %d captures, found %d" expected actual
  | Call_arity (expected, actual) ->
    Printf.sprintf "Expected %d direct-call arguments, found %d" expected actual
;;

let error_to_string = function
  | Runtime error -> runtime_error_to_string error
  | Invalid_ir error -> "Invalid interpreted IR: " ^ invariant_error_to_string error
  | Unsupported_integer literal ->
    "Test interpreter cannot represent integer "
    ^ Bkml_frontend.Integer_literal.to_string literal
  | Heap_exhausted -> "Test interpreter address space exhausted"
;;

let kind = function
  | Int _ -> Integer
  | Bool _ -> Boolean
  | Unit -> Unit_value
  | Tuple _ -> Tuple_value
  | Function _ -> Function_value
;;

let return value heap = Ok (value, heap)
let fail error _heap = Error error

let ( let* ) computation continuation heap =
  Result.bind (computation heap) (fun (value, heap) -> continuation value heap)
;;

let reserve heap =
  if heap.next = max_int
  then Error Heap_exhausted
  else Ok (heap.next, { heap with next = heap.next + 1 })
;;

let store address node heap =
  Ok ((), { heap with nodes = Address_map.add address node heap.nodes })
;;

let node heap address =
  match Address_map.find_opt address heap.nodes with
  | Some value -> Ok value
  | None -> Error (Invalid_ir (Uninitialized_value address))
;;

let get address heap = Result.map (fun value -> value, heap) (node heap address)

let variable environment id =
  match Id.Map.find_opt id environment with
  | Some value -> return value
  | None -> fail (Invalid_ir (Unbound_variable id))
;;

let make value =
  let* address = reserve in
  let* () = store address value in
  return address
;;

let recursive initialize =
  let* address = reserve in
  let* value = initialize address in
  let* value = get value in
  let* () = store address value in
  return address
;;

let integer value =
  let* value = get value in
  match value with
  | Int n -> return n
  | value -> fail (Invalid_ir (Unexpected_value (Integer, kind value)))
;;

let boolean value =
  let* value = get value in
  match value with
  | Bool b -> return b
  | value -> fail (Invalid_ir (Unexpected_value (Boolean, kind value)))
;;

let field value index =
  let* value = get value in
  match value with
  | Tuple values ->
    let selected = if index < 0 then None else List.nth_opt values index in
    (match selected with
     | Some field -> return field
     | None -> fail (Invalid_ir (Field_out_of_bounds (index, List.length values))))
  | value -> fail (Invalid_ir (Unexpected_value (Tuple_value, kind value)))
;;

let apply f x =
  let* f = get f in
  match f with
  | Function f -> f x
  | value -> fail (Invalid_ir (Unexpected_value (Function_value, kind value)))
;;

let unary f = make (Function f)
let binary f = unary (fun a -> unary (fun b -> f a b))

let constant = function
  | Bkml_frontend.Constant.Int n ->
    (match int_of_string_opt (Bkml_frontend.Integer_literal.to_string n) with
     | Some value -> make (Int value)
     | None -> fail (Unsupported_integer n))
  | Bool b -> make (Bool b)
  | Unit -> make Unit
;;

(* Compare contents, inspecting tuple fields only until the first difference. *)
let rec compare_values heap a b =
  let ( let* ) = Result.bind in
  let* a = node heap a in
  let* b = node heap b in
  match a, b with
  | Int a, Int b -> Ok (Int.compare a b)
  | Bool a, Bool b -> Ok (Bool.compare a b)
  | Unit, Unit -> Ok 0
  | Tuple a, Tuple b -> compare_fields heap a b
  | Function _, _ | _, Function _ -> Error (Runtime Functional_comparison)
  | a, b -> Error (Invalid_ir (Incompatible_comparison (kind a, kind b)))

and compare_fields heap a b =
  let ( let* ) = Result.bind in
  match a, b with
  | [], [] -> Ok 0
  | [], _ -> Ok (-1)
  | _, [] -> Ok 1
  | a :: rest_a, b :: rest_b ->
    let* result = compare_values heap a b in
    if result = 0 then compare_fields heap rest_a rest_b else Ok result
;;

let comparison predicate a b heap =
  Result.bind (compare_values heap a b) (fun order -> make (Bool (predicate order)) heap)
;;

let print text heap = Ok ((), { heap with output = text :: heap.output })

let arithmetic operator a b =
  let* a = integer a in
  let* b = integer b in
  make (Int (operator a b))
;;

let division operator a b =
  let* a = integer a in
  let* b = integer b in
  if b = 0 then fail (Runtime Division_by_zero) else make (Int (operator a b))
;;

let logical operator a b =
  let* a = boolean a in
  let* b = boolean b in
  make (Bool (operator a b))
;;

let primitive = function
  | Primitive.Add -> binary (arithmetic ( + ))
  | Subtract -> binary (arithmetic ( - ))
  | Multiply -> binary (arithmetic ( * ))
  | Divide -> binary (division ( / ))
  | Modulo -> binary (division ( mod ))
  | Negate ->
    unary (fun a ->
      let* a = integer a in
      make (Int (-a)))
  | Equal -> binary (comparison (fun order -> order = 0))
  | Not_equal -> binary (comparison (fun order -> order <> 0))
  | Less -> binary (comparison (fun order -> order < 0))
  | Less_equal -> binary (comparison (fun order -> order <= 0))
  | Greater -> binary (comparison (fun order -> order > 0))
  | Greater_equal -> binary (comparison (fun order -> order >= 0))
  | And -> binary (logical ( && ))
  | Or -> binary (logical ( || ))
  | Not ->
    unary (fun a ->
      let* a = boolean a in
      make (Bool (not a)))
  | Print_int ->
    unary (fun a ->
      let* a = integer a in
      let* () = print (string_of_int a) in
      make Unit)
;;

let function_ environment parameters body =
  let rec bind environment = function
    | [] -> body environment
    | parameter :: rest ->
      unary (fun value -> bind (Id.Map.add parameter value environment) rest)
  in
  bind environment parameters
;;

let rec map evaluate = function
  | [] -> return []
  | first :: rest ->
    let* first = evaluate first in
    let* rest = map evaluate rest in
    return (first :: rest)
;;

let rec apply_arguments f = function
  | [] -> return f
  | argument :: rest ->
    let* f = apply f argument in
    apply_arguments f rest
;;

let rec show ancestors value =
  if Address_set.mem value ancestors
  then return "<cycle>"
  else
    let* contents = get value in
    match contents with
    | Int n -> return (string_of_int n)
    | Bool b -> return (string_of_bool b)
    | Unit -> return "()"
    | Function _ -> return "<function>"
    | Tuple fields ->
      let* fields = map (show (Address_set.add value ancestors)) fields in
      return ("(" ^ String.concat ", " fields ^ ")")
;;

let observe_with_output run =
  let evaluation =
    let* value = run () in
    show Address_set.empty value
  in
  match evaluation { next = 0; nodes = Address_map.empty; output = [] } with
  | Ok (value, heap) -> Ok (value, String.concat "" (List.rev heap.output))
  | Error error -> Error error
;;

let observe run = Result.map fst (observe_with_output run)
