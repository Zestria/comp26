[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type atom =
  | Constant of Frontend.Constant.t
  | Var of Id.t
  | Primitive of Primitive.t

(* Stages share ordinary computations. Their function payload and callee types
   determine which function values and calls can be represented at each stage. *)
type ('function_, 'callee) computation =
  | Atom of atom
  | Tuple of atom * atom * atom list
  | Field of atom * int
  | Function of 'function_
  | Apply of 'callee * atom * atom list
  | If of atom * ('function_, 'callee) term * ('function_, 'callee) term

and ('function_, 'callee) term =
  | Return of ('function_, 'callee) computation
  | Let of Id.t * ('function_, 'callee) computation * ('function_, 'callee) term
  | Let_rec of Id.t * ('function_, 'callee) term * ('function_, 'callee) term
  | Sequence of ('function_, 'callee) computation * ('function_, 'callee) term

type function_definition =
  { parameter : Id.t
  ; parameters : Id.t list
  ; body : (function_definition, atom) term
  }

type expression = (function_definition, atom) term
type program = expression

let atom_to_lambda = function
  | Constant c -> Lambda.Constant c
  | Var id -> Lambda.Var id
  | Primitive p -> Lambda.Primitive p
;;

(** Embedding into the more general language for independent execution of
    normalized terms in tests. *)
let rec to_lambda = function
  | Return value -> computation_to_lambda value
  | Let (id, value, body) -> Lambda.Let (id, computation_to_lambda value, to_lambda body)
  | Let_rec (id, value, body) -> Lambda.Let_rec (id, to_lambda value, to_lambda body)
  | Sequence (first, second) ->
    Lambda.Sequence (computation_to_lambda first, to_lambda second)

and computation_to_lambda = function
  | Atom atom -> atom_to_lambda atom
  | Tuple (a, b, rest) ->
    Lambda.Tuple (atom_to_lambda a, atom_to_lambda b, List.map atom_to_lambda rest)
  | Field (value, index) -> Lambda.Field (atom_to_lambda value, index)
  | Function { parameter; parameters; body } ->
    Lambda.Fun { parameter; parameters; body = to_lambda body }
  | Apply (f, a, rest) ->
    Lambda.Apply (atom_to_lambda f, atom_to_lambda a, List.map atom_to_lambda rest)
  | If (condition, yes, no) ->
    Lambda.If (atom_to_lambda condition, to_lambda yes, to_lambda no)
;;

module Scope = struct
  (* Globally declared identities, kept separately from lexical scope. *)
  type state = Id.Set.t

  let ( let* ) = Result.bind
  let empty = Id.Set.empty

  let bind declared scope id =
    if Id.Set.mem id declared
    then Error (Lambda.Duplicate_binding id)
    else Ok (Id.Set.add id declared, Id.Set.add id scope)
  ;;

  let rec bind_all declared scope = function
    | [] -> Ok (declared, scope)
    | id :: rest ->
      let* declared, scope = bind declared scope id in
      bind_all declared scope rest
  ;;

  let atom scope = function
    | Var id when not (Id.Set.mem id scope) -> Error (Lambda.Unbound_variable id)
    | Var _ | Constant _ | Primitive _ -> Ok ()
  ;;

  let callee scope value ~arity:_ = atom scope value

  let rec atoms scope = function
    | [] -> Ok ()
    | value :: rest ->
      let* () = atom scope value in
      atoms scope rest
  ;;

  let rec term ~error ~callee function_ declared scope = function
    | Return value -> computation ~error ~callee function_ declared scope value
    | Let (id, value, body) ->
      let* declared, inner = Result.map_error error (bind declared scope id) in
      let* declared = computation ~error ~callee function_ declared scope value in
      term ~error ~callee function_ declared inner body
    | Let_rec (id, value, body) ->
      let* declared, inner = Result.map_error error (bind declared scope id) in
      let* declared = term ~error ~callee function_ declared inner value in
      term ~error ~callee function_ declared inner body
    | Sequence (first, second) ->
      let* declared = computation ~error ~callee function_ declared scope first in
      term ~error ~callee function_ declared scope second

  and computation ~error ~callee function_ declared scope = function
    | Atom value ->
      let* () = Result.map_error error (atom scope value) in
      Ok declared
    | Tuple (a, b, rest) ->
      let* () = Result.map_error error (atoms scope (a :: b :: rest)) in
      Ok declared
    | Field (_, index) when index < 0 -> Error (error (Lambda.Negative_field index))
    | Field (value, _) ->
      let* () = Result.map_error error (atom scope value) in
      Ok declared
    | Apply (f, a, rest) ->
      let* () = callee scope f ~arity:(1 + List.length rest) in
      let* () = Result.map_error error (atoms scope (a :: rest)) in
      Ok declared
    | If (condition, yes, no) ->
      let* () = Result.map_error error (atom scope condition) in
      let* declared = term ~error ~callee function_ declared scope yes in
      term ~error ~callee function_ declared scope no
    | Function definition -> function_ declared scope definition
  ;;

  let run check = Result.map (fun _ -> ()) (check empty)
end

let validate expression =
  let ( let* ) = Result.bind in
  let rec function_ declared scope { parameter; parameters; body } =
    let* declared, scope = Scope.bind_all declared scope (parameter :: parameters) in
    Scope.term ~error:Fun.id ~callee:Scope.callee function_ declared scope body
  in
  Scope.run (fun declared ->
    Scope.term
      ~error:Fun.id
      ~callee:Scope.callee
      function_
      declared
      Id.Set.empty
      expression)
;;

let ( let* ) = Result.bind

let rec map_accum_result ~atom ~callee ~function_ state = function
  | Return value ->
    let* state, value =
      map_computation_accum_result ~atom ~callee ~function_ state value
    in
    Ok (state, Return value)
  | Let (id, value, body) ->
    let* state, value =
      map_computation_accum_result ~atom ~callee ~function_ state value
    in
    let* state, body = map_accum_result ~atom ~callee ~function_ state body in
    Ok (state, Let (id, value, body))
  | Let_rec (id, value, body) ->
    let* state, value = map_accum_result ~atom ~callee ~function_ state value in
    let* state, body = map_accum_result ~atom ~callee ~function_ state body in
    Ok (state, Let_rec (id, value, body))
  | Sequence (first, second) ->
    let* state, first =
      map_computation_accum_result ~atom ~callee ~function_ state first
    in
    let* state, second = map_accum_result ~atom ~callee ~function_ state second in
    Ok (state, Sequence (first, second))

and map_computation_accum_result ~atom ~callee ~function_ state = function
  | Atom value ->
    let value = atom value in
    Ok (state, Atom value)
  | Tuple (a, b, rest) ->
    let a = atom a in
    let b = atom b in
    let rest = List.map atom rest in
    Ok (state, Tuple (a, b, rest))
  | Field (value, index) ->
    let value = atom value in
    Ok (state, Field (value, index))
  | Apply (f, a, rest) ->
    let f = callee f ~arity:(1 + List.length rest) in
    let a = atom a in
    let rest = List.map atom rest in
    Ok (state, Apply (f, a, rest))
  | If (condition, yes, no) ->
    let condition = atom condition in
    let* state, yes = map_accum_result ~atom ~callee ~function_ state yes in
    let* state, no = map_accum_result ~atom ~callee ~function_ state no in
    Ok (state, If (condition, yes, no))
  | Function definition ->
    let* state, definition = function_ state definition in
    Ok (state, Function definition)
;;

(* The ordinary traversal has no error-producing callback. *)
type no_error = |

let map_accum ~atom ~callee ~function_ state expression =
  let result : (_, no_error) result =
    map_accum_result
      ~atom
      ~callee
      ~function_:(fun state value -> Ok (function_ state value))
      state
      expression
  in
  match result with
  | Ok value -> value
  | Error impossible -> .
;;

let map ~atom ~callee ~function_ expression =
  let _, expression =
    map_accum
      ~atom
      ~callee
      ~function_:(fun () definition ->
        let definition = function_ definition in
        (), definition)
      ()
      expression
  in
  expression
;;

let rec fold ~atom ~callee ~binder ~function_ state = function
  | Return value -> fold_computation ~atom ~callee ~binder ~function_ state value
  | Let (id, value, body) ->
    let state = binder state id in
    let state = fold_computation ~atom ~callee ~binder ~function_ state value in
    fold ~atom ~callee ~binder ~function_ state body
  | Let_rec (id, value, body) ->
    let state = binder state id in
    let state = fold ~atom ~callee ~binder ~function_ state value in
    fold ~atom ~callee ~binder ~function_ state body
  | Sequence (first, second) ->
    let state = fold_computation ~atom ~callee ~binder ~function_ state first in
    fold ~atom ~callee ~binder ~function_ state second

and fold_computation ~atom ~callee ~binder ~function_ state = function
  | Atom value | Field (value, _) -> atom state value
  | Apply (f, a, rest) ->
    let state = callee state f ~arity:(1 + List.length rest) in
    List.fold_left atom (atom state a) rest
  | Tuple (a, b, rest) ->
    let state = atom state a in
    let state = atom state b in
    List.fold_left atom state rest
  | If (condition, yes, no) ->
    let state = atom state condition in
    let state = fold ~atom ~callee ~binder ~function_ state yes in
    fold ~atom ~callee ~binder ~function_ state no
  | Function definition -> function_ state definition
;;

let bind id value = function
  | Return (Atom (Var result)) when Id.equal id result -> Return value
  | body -> Let (id, value, body)
;;

let sequence first second =
  match first with
  | Atom _ -> second
  | Tuple _ | Field _ | Function _ | Apply _ | If _ -> Sequence (first, second)
;;

(* Remember aliases and immutable tuple fields without moving or copying
   computations. Recursive initializers never publish facts. *)
type approximation =
  | Alias of atom
  | Fields of atom list

let remember environment id = function
  | Atom value -> Id.Map.add id (Alias value) environment
  | Tuple (a, b, rest) -> Id.Map.add id (Fields (a :: b :: rest)) environment
  | Field _ | Function _ | Apply _ | If _ -> environment
;;

let resolve environment id =
  match Id.Map.find_opt id environment with
  | Some (Alias value) -> value
  | Some (Fields _) | None -> Var id
;;

let project environment value index =
  let field =
    match value with
    | Var id when index >= 0 ->
      (match Id.Map.find_opt id environment with
       | Some (Fields fields) -> List.nth_opt fields index
       | Some (Alias _) | None -> None)
    | Var _ | Constant _ | Primitive _ -> None
  in
  match field with
  | Some value -> Atom value
  | None -> Field (value, index)
;;

let application f a rest =
  let rec constants = function
    | [] -> Some []
    | Constant value :: rest -> Option.map (fun rest -> value :: rest) (constants rest)
    | Var _ :: _ | Primitive _ :: _ -> None
  in
  let folded =
    match f with
    | Primitive primitive ->
      Option.bind (constants (a :: rest)) (Primitive_simplify.apply primitive)
    | Constant _ | Var _ -> None
  in
  match folded with
  | Some value -> Atom (Constant value)
  | None -> Apply (f, a, rest)
;;

(* Visit uses before definitions and do not count
   references in a discarded RHS. A set suffices: we do not inline computations.
   Globally unique binding identities let us thread one set through branches
   and function bodies without rescanning subtrees or merging free-variable sets.
   Allocation alone is removable, but
   calls may raise/diverge and an unknown projection may inspect a recursive
   address before it is initialized. Keep both, even when their result is dead. *)
let removable = function
  | Atom _ | Tuple _ | Function _ -> true
  | Field _ | Apply _ | If _ -> false
;;

let remove_unused ~callee ~function_ expression =
  let atom used = function
    | Var id -> Id.Set.add id used
    | Constant _ | Primitive _ -> used
  in
  let rec term used = function
    | Return value ->
      let used, value = computation used value in
      used, Return value
    | Let (id, value, body) ->
      let used, body = term used body in
      let needed = Id.Set.mem id used in
      if (not needed) && removable value
      then used, body
      else (
        let used, value = computation used value in
        used, if needed then bind id value body else Sequence (value, body))
    | Let_rec (id, value, body) ->
      let used, body = term used body in
      (match value with
       | Return (Function _) when not (Id.Set.mem id used) -> used, body
       | _ ->
         let used, value = term used value in
         used, Let_rec (id, value, body))
    | Sequence (first, second) ->
      let used, second = term used second in
      if removable first
      then used, second
      else (
        let used, first = computation used first in
        used, Sequence (first, second))
  and computation used = function
    | Atom value -> atom used value, Atom value
    | Tuple (a, b, rest) as value ->
      List.fold_left atom (atom (atom used a) b) rest, value
    | Apply (f, a, rest) as value ->
      let used = callee used f ~arity:(1 + List.length rest) in
      List.fold_left atom (atom used a) rest, value
    | Field (value, index) -> atom used value, Field (value, index)
    | If (condition, yes, no) ->
      let used, yes = term (atom used condition) yes in
      let used, no = term used no in
      used, If (condition, yes, no)
    | Function definition ->
      let used, definition = function_ term used definition in
      used, Function definition
  in
  snd (term Id.Set.empty expression)
;;

let of_lambda supply expression =
  (* The continuation inherits facts for bindings hoisted around it. Facts from
     function bodies, unknown branches and recursive initializers stay local. *)
  let rec normalize environment expression continuation supply =
    match expression with
    | Lambda.Constant constant ->
      continuation environment (Atom (Constant constant)) supply
    | Primitive primitive -> continuation environment (Atom (Primitive primitive)) supply
    | Var id -> continuation environment (Atom (resolve environment id)) supply
    | Tuple (a, b, rest) ->
      atoms
        environment
        rest
        (fun environment rest supply ->
           atom
             environment
             b
             (fun environment b supply ->
                atom
                  environment
                  a
                  (fun environment a supply ->
                     continuation environment (Tuple (a, b, rest)) supply)
                  supply)
             supply)
        supply
    | Field (value, index) ->
      atom
        environment
        value
        (fun environment value supply ->
           continuation environment (project environment value index) supply)
        supply
    | Fun { parameter; parameters; body } ->
      let* body, supply = normalize environment body return supply in
      continuation environment (Function { parameter; parameters; body }) supply
    | Apply (f, a, rest) ->
      atoms
        environment
        rest
        (fun environment rest supply ->
           atom
             environment
             a
             (fun environment a supply ->
                atom
                  environment
                  f
                  (fun environment f supply ->
                     continuation environment (application f a rest) supply)
                  supply)
             supply)
        supply
    | Let (id, value, body) ->
      normalize
        environment
        value
        (fun environment value supply ->
           let environment = remember environment id value in
           match value with
           | Atom _ -> normalize environment body continuation supply
           | value ->
             let* body, supply = normalize environment body continuation supply in
             Ok (bind id value body, supply))
        supply
    | Let_rec (id, value, body) ->
      let* value, supply = normalize environment value return supply in
      let* body, supply = normalize environment body continuation supply in
      Ok (Let_rec (id, value, body), supply)
    | If (condition, yes, no) ->
      atom
        environment
        condition
        (fun environment condition supply ->
           match condition with
           | Constant (Bool choice) ->
             normalize environment (if choice then yes else no) continuation supply
           | Constant _ | Var _ | Primitive _ ->
             (* Apply the continuation once to the whole conditional. Distributing
           it into both branches would duplicate code and binding identities. *)
             let* yes, supply = normalize environment yes return supply in
             let* no, supply = normalize environment no return supply in
             continuation environment (If (condition, yes, no)) supply)
        supply
    | Sequence (first, second) ->
      normalize
        environment
        first
        (fun environment first supply ->
           let* second, supply = normalize environment second continuation supply in
           Ok (sequence first second, supply))
        supply
  and return _ value supply = Ok (Return value, supply)
  and atom environment expression continuation supply =
    normalize
      environment
      expression
      (fun environment value supply ->
         match value with
         | Atom value -> continuation environment value supply
         | value ->
           let* id, supply = Id.fresh supply "tmp" in
           let* body, supply =
             continuation (remember environment id value) (Var id) supply
           in
           Ok (bind id value body, supply))
      supply
  and atoms environment expressions continuation supply =
    match expressions with
    | [] -> continuation environment [] supply
    | expression :: rest ->
      atoms
        environment
        rest
        (fun environment rest supply ->
           atom
             environment
             expression
             (fun environment atom supply ->
                continuation environment (atom :: rest) supply)
             supply)
        supply
  in
  let* result, supply = normalize Id.Map.empty expression return supply in
  Ok
    ( remove_unused
        ~callee:(fun used value ~arity:_ ->
          match value with
          | Var id -> Id.Set.add id used
          | Constant _ | Primitive _ -> used)
        ~function_:(fun term used definition ->
          let used, body = term used definition.body in
          used, { definition with body })
        result
    , supply )
;;
