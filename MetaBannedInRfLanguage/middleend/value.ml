[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VTuple of value * value * value list
  (** Takes the arguments and the remaining fuel, returns the result and the
      fuel left after the call. *)
  | VClosure of (value list -> int -> (value * int, string) result)

let rec equal a b =
  match a, b with
  | VInt a, VInt b -> Int.equal a b
  | VBool a, VBool b -> Bool.equal a b
  | VUnit, VUnit -> true
  | VTuple (a1, a2, arest), VTuple (b1, b2, brest) ->
    equal a1 b1
    && equal a2 b2
    &&
      (try List.for_all2 equal arest brest with
      | Invalid_argument _ -> false)
  | VClosure _, VClosure _ -> true
  | _ -> false
;;

let rec to_string = function
  | VInt n -> Int.to_string n
  | VBool b -> Bool.to_string b
  | VUnit -> "()"
  | VTuple (a, b, rest) ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map to_string (a :: b :: rest)))
  | VClosure _ -> "<closure>"
;;

let primitive_arity = function
  | "~+" | "~-" | "not" -> Some 1
  | "+" | "-" | "*" | "/" | "&&" | "||" | "=" | "<>" | "<" | "<=" | ">" | ">=" -> Some 2
  | _ -> None
;;

let is_primitive name = Option.is_some (primitive_arity name)

let rec apply_primitive name args =
  match primitive_arity name with
  | Some arity when List.length args < arity ->
    Ok
      (VClosure
         (fun rest fuel ->
           Result.map (fun value -> value, fuel) (apply_primitive name (args @ rest))))
  | _ ->
    (match name, args with
     | "~+", [ VInt a ] -> Ok (VInt a)
     | "+", [ VInt a; VInt b ] -> Ok (VInt (a + b))
     | "~-", [ VInt a ] -> Ok (VInt (-a))
     | "-", [ VInt a; VInt b ] -> Ok (VInt (a - b))
     | "*", [ VInt a; VInt b ] -> Ok (VInt (a * b))
     | "/", [ VInt _; VInt 0 ] -> Error "division by zero"
     | "/", [ VInt a; VInt b ] -> Ok (VInt (a / b))
     | "<", [ VInt a; VInt b ] -> Ok (VBool (a < b))
     | "<=", [ VInt a; VInt b ] -> Ok (VBool (a <= b))
     | ">", [ VInt a; VInt b ] -> Ok (VBool (a > b))
     | ">=", [ VInt a; VInt b ] -> Ok (VBool (a >= b))
     | "=", [ a; b ] -> Ok (VBool (equal a b))
     | "<>", [ a; b ] -> Ok (VBool (not (equal a b)))
     | "not", [ VBool a ] -> Ok (VBool (not a))
     | "&&", [ VBool a; VBool b ] -> Ok (VBool (a && b))
     | "||", [ VBool a; VBool b ] -> Ok (VBool (a || b))
     | name, args ->
       Error
         (Printf.sprintf
            "bad application of primitive %S (%d argument(s))"
            name
            (List.length args)))
;;
