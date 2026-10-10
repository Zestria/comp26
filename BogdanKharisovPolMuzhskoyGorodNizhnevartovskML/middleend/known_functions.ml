[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type source =
  | Code of Id.t
  | Value of Anf.atom
  | Unknown

type fact =
  | Unseen
  | Known of Id.t
  | Dynamic

let join left right =
  match left with
  | Unseen -> right
  | Dynamic -> Dynamic
  | Known a ->
    (match right with
     | Unseen -> Known a
     | Known b when Id.equal a b -> Known a
     | Known _ | Dynamic -> Dynamic)
;;

let same_fact left right =
  match left, right with
  | Unseen, Unseen | Dynamic, Dynamic -> true
  | Known a, Known b -> Id.equal a b
  | _ -> false
;;

let fact facts = function
  | Anf.Var id -> Option.value (Id.Map.find_opt id facts) ~default:Dynamic
  | Constant _ | Primitive _ -> Dynamic
;;

let add_source id source constraints =
  Id.Map.update
    id
    (fun previous -> Some (source :: Option.value previous ~default:[]))
    constraints
;;

let computation_source = function
  | Anf.Function (closure : Lifted.closure) -> Code closure.code
  | Atom value -> Value value
  | Tuple _ | Field _ | Apply _ | If _ -> Unknown
;;

let add_value_dependency source target dependencies =
  Id.Map.update
    source
    (fun previous ->
       Some (Id.Set.add target (Option.value previous ~default:Id.Set.empty)))
    dependencies
;;

(* A function can be constructed at more than one site. Every corresponding
   capture contributes to the slot's fact; disagreement becomes Dynamic. *)
let known_values declarations (program : Lifted.program) =
  let rec term constraints = function
    | Anf.Return value -> computation constraints value, computation_source value
    | Let (id, value, body) ->
      let constraints = add_source id (computation_source value) constraints in
      term (computation constraints value) body
    | Let_rec (id, value, body) ->
      let constraints, source = term constraints value in
      term (add_source id source constraints) body
    | Sequence (first, second) -> term (computation constraints first) second
  and computation constraints = function
    | Anf.Function (closure : Lifted.closure) ->
      let definition : Lifted.function_definition =
        Id.Map.find closure.code declarations
      in
      List.fold_left2
        (fun constraints slot captured -> add_source slot (Value captured) constraints)
        constraints
        definition.environment
        closure.captures
    | If (_, yes, no) ->
      let constraints, _ = term constraints yes in
      let constraints, _ = term constraints no in
      constraints
    | Atom _ | Tuple _ | Field _ | Apply _ -> constraints
  in
  let constraints =
    List.fold_left
      (fun constraints (definition : Lifted.function_definition) ->
         let constraints =
           List.fold_left
             (fun constraints slot -> Id.Map.add slot [] constraints)
             constraints
             definition.environment
         in
         List.fold_left
           (fun constraints parameter -> add_source parameter Unknown constraints)
           constraints
           (definition.parameter :: definition.parameters))
      Id.Map.empty
      program.functions
  in
  let constraints, _ = term constraints program.body in
  let constraints =
    List.fold_left
      (fun constraints (definition : Lifted.function_definition) ->
         fst (term constraints definition.body))
      constraints
      program.functions
  in
  let dependencies =
    Id.Map.fold
      (fun target sources dependencies ->
         List.fold_left
           (fun dependencies -> function
              | Value (Anf.Var source) -> add_value_dependency source target dependencies
              | Code _ | Unknown | Value (Constant _ | Primitive _) -> dependencies)
           dependencies
           sources)
      constraints
      Id.Map.empty
  in
  let seed = function
    | Code code -> Known code
    | Value (Anf.Var id) when Id.Map.mem id constraints -> Unseen
    | Value _ | Unknown -> Dynamic
  in
  let facts =
    Id.Map.map
      (List.fold_left (fun value source -> join value (seed source)) Unseen)
      constraints
  in
  (* Each identity changes at most Unseen -> Known -> Dynamic. Send only these
     changes along dependency edges; no round rescans the complete fact map. *)
  let rec propagate facts pending =
    match Id.Set.min_elt_opt pending with
    | None -> facts
    | Some source ->
      let value = Id.Map.find source facts in
      let dependents =
        Option.value (Id.Map.find_opt source dependencies) ~default:Id.Set.empty
      in
      let facts, pending =
        Id.Set.fold
          (fun target (facts, pending) ->
             let previous = Id.Map.find target facts in
             let next = join previous value in
             if same_fact previous next
             then facts, pending
             else Id.Map.add target next facts, Id.Set.add target pending)
          dependents
          (facts, Id.Set.remove source pending)
      in
      propagate facts pending
  in
  let pending =
    Id.Map.fold
      (fun id value pending ->
         match value with
         | Unseen -> pending
         | Known _ | Dynamic -> Id.Set.add id pending)
      facts
      Id.Set.empty
  in
  let facts = propagate facts pending in
  (* A seedless cycle supplies no evidence of a code identity. Propagate its
     unknown result too, rather than retaining a fact inferred past Unseen. *)
  let facts, pending =
    Id.Map.fold
      (fun id value (facts, pending) ->
         match value with
         | Unseen -> Id.Map.add id Dynamic facts, Id.Set.add id pending
         | Known _ | Dynamic -> facts, pending)
      facts
      (facts, Id.Set.empty)
  in
  propagate facts pending
;;

let known_call declarations facts target ~arity =
  match target with
  | Lifted.Direct code -> Some code
  | Indirect value ->
    (match fact facts value with
     | Known code ->
       let definition : Lifted.function_definition = Id.Map.find code declarations in
       if arity = 1 + List.length definition.parameters then Some code else None
     | Unseen | Dynamic -> None)
;;

(* Code labels and value bindings have separate namespaces in Lifted. Tag
   the nodes instead of assuming their underlying identities cannot coincide. *)
module Demand = struct
  type t =
    | Slot of Id.t
    | Environment of Id.t

  let compare left right =
    match left, right with
    | Slot a, Slot b | Environment a, Environment b -> Id.compare a b
    | Slot _, Environment _ -> -1
    | Environment _, Slot _ -> 1
  ;;
end

module Demand_set = Set.Make (Demand)
module Demand_map = Map.Make (Demand)

type uses =
  { required : Demand_set.t
  ; dependencies : Demand_set.t Demand_map.t
  }

let add_use slots required = function
  | Anf.Var id when Id.Set.mem id slots -> Demand_set.add (Demand.Slot id) required
  | Var _ | Constant _ | Primitive _ -> required
;;

let add_dependency needed dependent dependencies =
  Demand_map.update
    needed
    (fun previous ->
       Some (Demand_set.add dependent (Option.value previous ~default:Demand_set.empty)))
    dependencies
;;

let collect_uses declarations facts slots (program : Lifted.program) =
  let atom uses value = { uses with required = add_use slots uses.required value } in
  let callee uses target ~arity =
    match target with
    | Lifted.Direct _ -> uses
    | Indirect value ->
      (match known_call declarations facts target ~arity, value with
       | Some code, Anf.Var slot when Id.Set.mem slot slots ->
         (* Exact known calls need the callee value only if its code needs an
            environment. Partial calls and escaping values remain ordinary uses. *)
         { uses with
           dependencies =
             add_dependency (Demand.Environment code) (Demand.Slot slot) uses.dependencies
         }
       | Some _, _ -> uses
       | None, _ -> atom uses value)
  in
  let function_ uses (closure : Lifted.closure) =
    let definition : Lifted.function_definition = Id.Map.find closure.code declarations in
    List.fold_left2
      (fun uses slot -> function
         | Anf.Var captured when Id.Set.mem captured slots ->
           { uses with
             dependencies =
               add_dependency (Demand.Slot slot) (Demand.Slot captured) uses.dependencies
           }
         | Var _ | Constant _ | Primitive _ -> uses)
      uses
      definition.environment
      closure.captures
  in
  let term = Anf.fold ~atom ~callee ~function_ ~binder:(fun uses _ -> uses) in
  List.fold_left
    (fun uses (definition : Lifted.function_definition) ->
       let dependencies =
         List.fold_left
           (fun dependencies slot ->
              add_dependency
                (Demand.Slot slot)
                (Demand.Environment definition.code)
                dependencies)
           uses.dependencies
           definition.environment
       in
       term { uses with dependencies } definition.body)
    (term { required = Demand_set.empty; dependencies = Demand_map.empty } program.body)
    program.functions
;;

let reachable dependencies roots =
  let rec visit reached = function
    | [] -> reached
    | node :: pending when Demand_set.mem node reached -> visit reached pending
    | node :: pending ->
      let next =
        Option.value (Demand_map.find_opt node dependencies) ~default:Demand_set.empty
      in
      visit (Demand_set.add node reached) (Demand_set.elements next @ pending)
  in
  visit Demand_set.empty (Demand_set.elements roots)
;;

let retain required slots values =
  List.filter_map
    (fun (slot, value) -> if Id.Set.mem slot required then Some value else None)
    (List.combine slots values)
;;

let optimize (program : Lifted.program) =
  let declarations =
    List.fold_left
      (fun declarations (definition : Lifted.function_definition) ->
         Id.Map.add definition.code definition declarations)
      Id.Map.empty
      program.functions
  in
  let facts = known_values declarations program in
  let slots =
    List.fold_left
      (fun slots (definition : Lifted.function_definition) ->
         List.fold_left
           (fun slots slot -> Id.Set.add slot slots)
           slots
           definition.environment)
      Id.Set.empty
      program.functions
  in
  let uses = collect_uses declarations facts slots program in
  let needed = reachable uses.dependencies uses.required in
  let required =
    Id.Set.filter (fun slot -> Demand_set.mem (Demand.Slot slot) needed) slots
  in
  let closed =
    Id.Map.fold
      (fun code _ closed ->
         if Demand_set.mem (Demand.Environment code) needed
         then closed
         else Id.Set.add code closed)
      declarations
      Id.Set.empty
  in
  let callee target ~arity =
    match known_call declarations facts target ~arity with
    | Some code when Id.Set.mem code closed -> Lifted.Direct code
    | Some _ | None -> target
  in
  let function_ (closure : Lifted.closure) =
    let definition : Lifted.function_definition = Id.Map.find closure.code declarations in
    { closure with captures = retain required definition.environment closure.captures }
  in
  let atom_use used = function
    | Anf.Var id -> Id.Set.add id used
    | Constant _ | Primitive _ -> used
  in
  let clean =
    Anf.remove_unused
      ~callee:(fun used target ~arity:_ ->
        match target with
        | Lifted.Indirect value -> atom_use used value
        | Direct _ -> used)
      ~function_:(fun _term used (closure : Lifted.closure) ->
        List.fold_left atom_use used closure.captures, closure)
  in
  let term expression = clean (Anf.map ~atom:Fun.id ~callee ~function_ expression) in
  let functions =
    List.map
      (fun (definition : Lifted.function_definition) ->
         { definition with
           environment =
             List.filter (fun slot -> Id.Set.mem slot required) definition.environment
         ; body = term definition.body
         })
      program.functions
  in
  { Lifted.functions; body = term program.body }
;;
