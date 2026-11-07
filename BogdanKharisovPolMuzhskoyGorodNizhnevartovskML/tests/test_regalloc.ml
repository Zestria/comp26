[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend
open Bkml_middleend
open Anf
open Bkml_backend

let check what condition = if not condition then failwith ("test_regalloc: " ^ what)

let fresh supply name =
  match Id.fresh supply name with
  | Ok value -> value
  | Error error -> failwith (Id.error_to_string error)
;;

let fresh_values supply prefix count =
  let rec go supply index values =
    if index = count
    then List.rev values, supply
    else (
      let id, supply = fresh supply (Printf.sprintf "%s%d" prefix index) in
      go supply (index + 1) (id :: values))
  in
  go supply 0 []
;;

let compile source =
  match Parser.parse_program source with
  | Error error -> failwith (source ^ ": " ^ error)
  | Ok ast ->
    (match Pipeline.compile ast with
     | Ok program -> program
     | Error error -> failwith (source ^ ": " ^ Pipeline.error_to_string error))
;;

let integer n =
  match Integer_literal.of_string (string_of_int n) with
  | Ok literal -> Constant (Int literal)
  | Error error -> failwith (Integer_literal.error_to_string error)
;;

let number n = Atom (integer n)
let variable id = Atom (Var id)

let location allocation id =
  match Id.Map.find_opt id allocation.Regalloc.locations with
  | Some location -> location
  | None -> failwith ("test_regalloc: no location for " ^ Id.name id)
;;

let location_name = function
  | Regalloc.Reg register -> Riscv.reg_to_string register
  | Slot index -> Printf.sprintf "slot(%d)" index
;;

let rec distinct_locations = function
  | [] -> true
  | location :: rest ->
    List.for_all (fun other -> location <> other) rest && distinct_locations rest
;;

let allocate_checked what entry body =
  let graph = Liveness.interference ~entry body in
  let allocation = Regalloc.allocate ~entry body in
  Id.Map.iter
    (fun id neighbours ->
       let here = location allocation id in
       check (what ^ ": irreflexive interference") (not (Id.Set.mem id neighbours));
       Id.Set.iter
         (fun neighbour ->
            let there = location allocation neighbour in
            check
              (what
               ^ Printf.sprintf
                   ": interfering %s and %s share %s"
                   (Id.name id)
                   (Id.name neighbour)
                   (location_name here))
              (here <> there);
            match Id.Map.find_opt neighbour graph with
            | Some reverse ->
              check (what ^ ": symmetric interference") (Id.Set.mem id reverse)
            | None -> failwith ("test_regalloc: missing graph key " ^ Id.name neighbour))
         neighbours)
    graph;
  Id.Map.iter
    (fun _ -> function
       | Regalloc.Reg (Riscv.S index) ->
         check (what ^ ": register range") (index >= 1 && index <= 11)
       | Reg _ -> failwith ("test_regalloc: " ^ what ^ ": allocated a non-S register")
       | Slot index -> check (what ^ ": nonnegative slot") (index >= 0))
    allocation.locations;
  allocation
;;

let program_allocations name (program : Lifted.program) =
  let program_allocation = allocate_checked (name ^ ": program") [] program.body in
  let functions =
    List.mapi
      (fun index (definition : Lifted.function_definition) ->
         let entry =
           definition.environment @ (definition.parameter :: definition.parameters)
         in
         allocate_checked
           (Printf.sprintf "%s: function %d" name index)
           entry
           definition.body)
      program.functions
  in
  program_allocation :: functions
;;

let check_program name source = program_allocations name (compile source)

let pressure_source =
  let bindings =
    String.concat
      " "
      (List.init 15 (fun index ->
         Printf.sprintf "let a%d = x + %d in" (index + 1) (index + 1)))
  in
  let sum =
    String.concat " + " (List.init 15 (fun index -> Printf.sprintf "a%d" (index + 1)))
  in
  "let f x = " ^ bindings ^ " " ^ sum ^ " in f 0"
;;

let pressure_block supply prefix input next =
  let values, supply = fresh_values supply prefix 15 in
  let finish =
    match values with
    | first :: second :: rest ->
      Sequence (Tuple (Var first, Var second, List.map (fun id -> Var id) rest), next)
    | _ -> failwith "test_regalloc: pressure block needs at least two values"
  in
  let body =
    List.fold_right
      (fun id body ->
         Let (id, Apply (Lifted.Indirect (Primitive Add), Var input, [ integer 1 ]), body))
      values
      finish
  in
  body, supply
;;

let test_pressure_and_slot_reuse () =
  let allocations = check_program "fifteen live values" pressure_source in
  check
    "fifteen live values spill and use every S register"
    (List.exists
       (fun (allocation : Regalloc.t) ->
          allocation.slots > 0 && List.length allocation.saved = 11)
       allocations);
  let input, supply = fresh Id.initial "input" in
  let one, supply = pressure_block supply "one" input (Return (variable input)) in
  let one = allocate_checked "one pressure block" [ input ] one in
  let second, supply = pressure_block supply "second" input (Return (variable input)) in
  let two, _ = pressure_block supply "first" input second in
  let two = allocate_checked "two pressure blocks" [ input ] two in
  check "the pressure block actually spills" (one.slots > 0);
  check
    (Printf.sprintf
       "sequential pressure blocks reuse spill slots (one = %d, two = %d)"
       one.slots
       two.slots)
    (one.slots = two.slots)
;;

let test_branch_locals () =
  let condition, supply = fresh Id.initial "condition" in
  let kept, supply = fresh supply "kept" in
  let result, supply = fresh supply "result" in
  let yes, supply = fresh supply "yes" in
  let no, _ = fresh supply "no" in
  let body =
    Let
      ( result
      , If
          ( Var condition
          , Let (yes, number 1, Return (variable yes))
          , Let (no, number 2, Return (variable no)) )
      , Return (Apply (Lifted.Indirect (Primitive Add), Var result, [ Var kept ])) )
  in
  let allocation = allocate_checked "branch locals" [ condition; kept ] body in
  let graph = Liveness.interference ~entry:[ condition; kept ] body in
  (match Id.Map.find_opt yes graph with
   | Some neighbours ->
     check "branch locals do not interfere" (not (Id.Set.mem no neighbours))
   | None -> failwith "test_regalloc: missing branch binder");
  check "branch locals share a register" (location allocation yes = location allocation no);
  List.iter
    (fun id ->
       check
         (Printf.sprintf "live-through value differs from %s" (Id.name id))
         (location allocation kept <> location allocation id))
    [ result; yes; no ]
;;

let test_dead_entries () =
  let a, supply = fresh Id.initial "a" in
  let b, _ = fresh supply "b" in
  let identity = allocate_checked "fun a b -> a" [ a; b ] (Return (variable a)) in
  check
    "dead entry parameter differs from the live parameter"
    (location identity a <> location identity b);
  check
    "both dead and live entries receive distinct locations"
    (distinct_locations (List.map (location identity) [ a; b ]));
  let parameters, _ = fresh_values Id.initial "parameter" 9 in
  let last = List.hd (List.rev parameters) in
  let allocation =
    allocate_checked "nine mostly dead parameters" parameters (Return (variable last))
  in
  check
    "all nine entry identifiers interfere"
    (distinct_locations (List.map (location allocation) parameters))
;;

let test_recursive_initializer () =
  let code, supply = fresh Id.initial "code" in
  let pair, supply = fresh supply "pair" in
  let first, supply = fresh supply "first" in
  let closure, _ = fresh supply "closure" in
  let value =
    Let
      ( first
      , number 1
      , Let
          ( closure
          , Function { Lifted.code; captures = [ Var pair ] }
          , Return (Tuple (Var first, Var closure, [])) ) )
  in
  let body = Let_rec (pair, value, Return (variable pair)) in
  let _ = allocate_checked "recursive static tuple" [] body in
  let graph = Liveness.interference ~entry:[] body in
  match Id.Map.find_opt pair graph with
  | None -> failwith "test_regalloc: missing recursive binder"
  | Some neighbours ->
    check "recursive binder interferes with first rhs value" (Id.Set.mem first neighbours);
    check
      "recursive binder interferes with closure rhs value"
      (Id.Set.mem closure neighbours)
;;

let () =
  let cps_factorial =
    "let rec fac n k = if n <= 1 then k 1 else fac (n - 1) (fun r -> k (n * r)) in fac 6 \
     (fun x -> x)"
  in
  let cps_fibonacci =
    "let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
     -> k (a + b))) in fib 6 (fun x -> x)"
  in
  let recursive_pair =
    "let rec p = (1, fun () -> let (x, _) = p in x) in let (_, f) = p in f ()"
  in
  List.iter
    (fun (name, source) -> ignore (check_program name source))
    [ "CPS factorial", cps_factorial
    ; "CPS fibonacci", cps_fibonacci
    ; "compiled recursive pair", recursive_pair
    ];
  test_pressure_and_slot_reuse ();
  test_branch_locals ();
  test_dead_entries ();
  test_recursive_initializer ();
  print_endline "test_regalloc: OK"
;;
