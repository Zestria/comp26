[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend

(* Check source syntax: a unit/tuple pattern dereferences its input even
   when its runtime projections are later optimized away. There is one
   binding per let, without mutually recursive groups. *)
type mode =
  | Ignore
  | Delay
  | Guard
  | Return
  | Dereference

let rank = function
  | Ignore -> 0
  | Delay -> 1
  | Guard -> 2
  | Return -> 3
  | Dereference -> 4
;;

let join a b = if rank a >= rank b then a else b

let compose outer inner =
  match outer, inner with
  | Ignore, _ | _, Ignore -> Ignore
  | Delay, _ -> Delay
  | Dereference, _ -> Dereference
  | Guard, Return -> Guard
  | (Guard | Return), mode -> mode
;;

module Names = Map.Make (String)
module Name_set = Set.Make (String)

let lookup name context = Option.value (Names.find_opt name context) ~default:Ignore
let union = Names.union (fun _ a b -> Some (join a b))
let unions contexts = List.fold_left union Names.empty contexts
let scale mode = Names.map (compose mode)

let rec bound_names = function
  | Ast.PVar name -> Name_set.singleton (Name.to_string name)
  | PWildcard | PUnit -> Name_set.empty
  | PTuple (a, b, rest) ->
    List.fold_left
      (fun names p -> Name_set.union names (bound_names p))
      Name_set.empty
      (a :: b :: rest)
;;

let remove_pattern pattern context =
  Name_set.fold Names.remove (bound_names pattern) context
;;

let pattern_mode pattern context =
  let minimum =
    match pattern with
    | Ast.PVar _ | PWildcard -> Guard
    | PUnit | PTuple _ -> Dereference
  in
  Name_set.fold
    (fun name mode -> join mode (lookup name context))
    (bound_names pattern)
    minimum
;;

let rec usage mode = function
  | Ast.Constant _ -> Names.empty
  | Var name -> Names.singleton (Name.to_string name) mode
  | Tuple (a, b, rest) -> unions (List.map (usage (compose mode Guard)) (a :: b :: rest))
  | Fun { parameter; parameters; body } ->
    List.fold_left
      (fun context p -> remove_pattern p context)
      (usage (compose mode Delay) body)
      (parameter :: parameters)
  | Apply (f, a, rest) ->
    unions (List.map (usage (compose mode Dereference)) (f :: a :: rest))
  | If (condition, yes, no) ->
    unions
      [ usage (compose mode Dereference) condition
      ; usage mode yes
      ; (match no with
         | None -> Names.empty
         | Some no -> usage mode no)
      ]
  | Let ({ recursion; pattern; expression }, body) ->
    let body_context = usage mode body in
    let rhs_context =
      usage (compose mode (pattern_mode pattern body_context)) expression
    in
    let dependencies =
      match recursion with
      | Nonrecursive ->
        (* A same-spelled name in the RHS refers to the outer binding. *)
        rhs_context
      | Recursive ->
        let self_mode = pattern_mode pattern rhs_context in
        let direct = remove_pattern pattern rhs_context in
        let rec close context =
          let next = union direct (scale self_mode context) in
          if Names.equal ( = ) context next then context else close next
        in
        close direct
    in
    union dependencies (remove_pattern pattern body_context)
;;

let statically_sized expression =
  let rec classify environment = function
    | Ast.Constant _ | Tuple _ | Fun _ -> true
    | Var name ->
      Option.value (Names.find_opt (Name.to_string name) environment) ~default:false
    | Apply _ | If _ -> false
    | Let ({ pattern; expression; recursion = _ }, body) ->
      let size = classify environment expression in
      let environment = remove_pattern pattern environment in
      let environment =
        match pattern with
        | PVar name -> Names.add (Name.to_string name) size environment
        | PWildcard | PUnit | PTuple _ -> environment
      in
      classify environment body
  in
  classify Names.empty expression
;;

type error = Unsafe_recursive_definition of Name.t

let error_to_string (Unsafe_recursive_definition name) =
  "This kind of expression is not allowed on the right of let rec " ^ Name.to_string name
;;

let ( let* ) = Result.bind

let rec expression = function
  | Ast.Constant _ | Var _ -> Ok ()
  | Tuple (a, b, rest) -> expressions (a :: b :: rest)
  | Fun { body; _ } -> expression body
  | Apply (f, a, rest) -> expressions (f :: a :: rest)
  | Let (definition, body) ->
    let* () = binding definition in
    expression body
  | If (condition, yes, no) -> expressions (condition :: yes :: Option.to_list no)

and expressions = function
  | [] -> Ok ()
  | first :: rest ->
    let* () = expression first in
    expressions rest

and binding { Ast.recursion; pattern; expression = rhs } =
  let* () = expression rhs in
  match recursion with
  | Nonrecursive -> Ok ()
  | Recursive ->
    (match pattern with
     | PVar name ->
       let mode = lookup (Name.to_string name) (usage Return rhs) in
       let limit = if statically_sized rhs then Guard else Ignore in
       if rank mode <= rank limit then Ok () else Error (Unsafe_recursive_definition name)
     | PWildcard | PUnit | PTuple _ ->
       (* Lower reports invalid recursive patterns and unresolved names. *)
       Ok ())
;;

let rec program = function
  | [] -> Ok ()
  | item :: rest ->
    let* () =
      match item with
      | Ast.Definition definition -> binding definition
      | Evaluation value -> expression value
    in
    program rest
;;
