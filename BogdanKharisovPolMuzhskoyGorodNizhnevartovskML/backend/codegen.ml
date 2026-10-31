[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend

(** RISC-V 64 code for a lifted program, linked with [runtime/runtime.c].
    The value representation, calling convention and runtime symbols are
    specified in [runtime/runtime.c] and in [Abi]. *)

type error =
  | Literal_out_of_range of string
  | Internal of string

exception Codegen_failure of error

let error_to_string = function
  | Literal_out_of_range literal -> "Integer literal out of range: " ^ literal
  | Internal message -> "Internal codegen failure: " ^ message
;;

type computation = (Lifted.closure, Lifted.callee) Anf.computation

let a0 = Riscv.A 0
let a1 = Riscv.A 1
let a2 = Riscv.A 2
let t0 = Riscv.T 0
let t1 = Riscv.T 1
let t2 = Riscv.T 2
let t3 = Riscv.T 3

(* Integers are the 63-bit values in [-2^62, 2^62 - 1], tagged as [2n + 1]. *)
let literal_minimum = -4_611_686_018_427_387_904L
let literal_maximum = 4_611_686_018_427_387_903L

let integer_literal literal =
  let text = Frontend.Integer_literal.to_string literal in
  let digits = String.concat "" (String.split_on_char '_' text) in
  match Int64.of_string_opt digits with
  | None -> raise (Codegen_failure (Literal_out_of_range text))
  | Some value ->
    if value < literal_minimum || value > literal_maximum
    then raise (Codegen_failure (Literal_out_of_range text))
    else value
;;

let tagged_constant = function
  | Frontend.Constant.Int literal -> Int64.add (Int64.mul (integer_literal literal) 2L) 1L
  | Frontend.Constant.Bool boolean -> Abi.bool_value boolean
  | Frontend.Constant.Unit -> Abi.unit_value
;;

let sanitize name =
  let clean =
    String.map
      (fun character ->
         match character with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> character
         | _ -> '_')
      name
  in
  if clean = "" then "code" else clean
;;

let primitive_symbol_name = function
  | Primitive.Add -> "add"
  | Primitive.Subtract -> "sub"
  | Primitive.Multiply -> "mul"
  | Primitive.Divide -> "div"
  | Primitive.Modulo -> "mod"
  | Primitive.Negate -> "neg"
  | Primitive.Equal -> "eq"
  | Primitive.Not_equal -> "ne"
  | Primitive.Less -> "lt"
  | Primitive.Less_equal -> "le"
  | Primitive.Greater -> "gt"
  | Primitive.Greater_equal -> "ge"
  | Primitive.And -> "and"
  | Primitive.Or -> "or"
  | Primitive.Not -> "not"
  | Primitive.Print_int -> "print_int"
;;

let primitive_wrapper primitive = "ml_prim_" ^ primitive_symbol_name primitive
let primitive_closure primitive = primitive_wrapper primitive ^ "_closure"

let primitive_arity = function
  | Primitive.Add
  | Primitive.Subtract
  | Primitive.Multiply
  | Primitive.Divide
  | Primitive.Modulo
  | Primitive.Equal
  | Primitive.Not_equal
  | Primitive.Less
  | Primitive.Less_equal
  | Primitive.Greater
  | Primitive.Greater_equal
  | Primitive.And
  | Primitive.Or -> 2
  | Primitive.Negate | Primitive.Not | Primitive.Print_int -> 1
;;

let is_immediate = function
  | Anf.Constant _ -> true
  | Anf.Var _ | Anf.Primitive _ -> false
;;

(* Functional codegen state: the file-wide label counter, the primitives used
   as values (needing a wrapper and a static closure) and the static closures
   of closed code. *)
type state =
  { counter : int
  ; primitives : Primitive.t list
  ; statics : (Id.t * string) list
  }

let initial_state = { counter = 0; primitives = []; statics = [] }

let fresh_local state =
  ".L" ^ string_of_int state.counter, { state with counter = state.counter + 1 }
;;

let fresh_code state name =
  ( "ml_" ^ sanitize name ^ "_" ^ string_of_int state.counter
  , { state with counter = state.counter + 1 } )
;;

let note_primitive primitive state =
  if List.mem primitive state.primitives
  then state
  else { state with primitives = state.primitives @ [ primitive ] }
;;

let note_static code symbol state =
  if List.exists (fun (known, _) -> Id.equal known code) state.statics
  then state
  else { state with statics = state.statics @ [ code, symbol ] }
;;

(* Statically sized recursive bindings. [Block (words, header)] allocates the
   destination before the initializer and copies the result in afterwards, so
   inner closures may capture the destination address; [Dynamic] computes the
   initializer into the variable's own location. *)
type shape =
  | Dynamic
  | Block of int * int64

type binding =
  | Let_bound of computation
  | Rec_bound of Lifted.expression

let rec shape_term bound (term : Lifted.expression) : shape =
  match term with
  | Return (Anf.Tuple (_, _, rest)) ->
    let fields = 2 + List.length rest in
    Block (fields + 1, Abi.tuple_header fields)
  | Return (Anf.Function { captures; _ }) ->
    Block (3 + List.length captures, Abi.closure_header)
  | Return (Anf.Atom (Anf.Var id)) -> shape_binding bound id
  | Return _ -> Dynamic
  | Anf.Let (id, source, body) -> shape_term ((id, Let_bound source) :: bound) body
  | Anf.Sequence (_, body) -> shape_term bound body
  | Anf.Let_rec (id, initializer_, body) ->
    shape_term ((id, Rec_bound initializer_) :: bound) body

and shape_binding bound id =
  match List.assoc_opt id bound with
  | Some (Let_bound (Anf.Tuple (_, _, rest))) ->
    let fields = 2 + List.length rest in
    Block (fields + 1, Abi.tuple_header fields)
  | Some (Let_bound (Anf.Function { captures; _ })) ->
    Block (3 + List.length captures, Abi.closure_header)
  | Some (Let_bound (Anf.Atom (Anf.Var id'))) -> shape_binding bound id'
  | Some (Let_bound _) | None -> Dynamic
  | Some (Rec_bound term) -> shape_term bound term
;;

let shape (term : Lifted.expression) = shape_term [] term

(* Calls of a body: the outgoing area size and whether any [call] is emitted.
   This pre-pass mirrors the emission below exactly, so the frame covers every
   call, and no needless return address is saved. *)
let rec scan_term (term : Lifted.expression) : int * bool =
  match term with
  | Return computation -> scan_computation computation
  | Anf.Let (_, computation, body) | Anf.Sequence (computation, body) ->
    merge_scan (scan_computation computation) (scan_term body)
  | Anf.Let_rec (_, initializer_, body) ->
    let own =
      match shape initializer_ with
      | Block _ -> 0, true
      | Dynamic -> 0, false
    in
    merge_scan (merge_scan own (scan_term initializer_)) (scan_term body)

and scan_computation (computation : computation) : int * bool =
  match computation with
  | Anf.Atom _ | Anf.Field _ -> 0, false
  | Anf.Tuple _ -> 0, true
  | Anf.Function { captures = []; _ } -> 0, false
  | Anf.Function _ -> 0, true
  | Anf.If (_, yes, no) -> merge_scan (scan_term yes) (scan_term no)
  | Anf.Apply (Lifted.Direct _, _, rest) -> 8 * max (1 + List.length rest - 7) 0, true
  | Anf.Apply (Lifted.Indirect (Anf.Primitive primitive), first, rest)
    when 1 + List.length rest = primitive_arity primitive ->
    (match primitive with
     | Primitive.Add
     | Primitive.Subtract
     | Primitive.Multiply
     | Primitive.Negate
     | Primitive.Not
     | Primitive.And
     | Primitive.Or -> 0, false
     | Primitive.Divide | Primitive.Modulo | Primitive.Print_int -> 0, true
     | Primitive.Equal
     | Primitive.Not_equal
     | Primitive.Less
     | Primitive.Less_equal
     | Primitive.Greater
     | Primitive.Greater_equal ->
       (match rest with
        | [ second ] ->
          if is_immediate first || is_immediate second then 0, false else 0, true
        | _ -> 0, true))
  | Anf.Apply (Lifted.Indirect _, _, rest) -> 8 * (1 + List.length rest), true

and merge_scan (outgoing_one, calls_one) (outgoing_two, calls_two) =
  max outgoing_one outgoing_two, calls_one || calls_two
;;

(* Frame layout, from [sp] upward: the outgoing area, the allocation slots,
   the saved [s] registers, and [ra] when the body contains a call. *)
type frame =
  { size : int
  ; saved : Riscv.reg list
  ; saves_ra : bool
  ; slot_offset : int -> int
  ; saved_offset : int -> int
  ; ra_offset : int
  }

let make_frame (allocation : Regalloc.t) outgoing calls =
  let slots = allocation.slots in
  let saved = allocation.saved in
  let payload =
    outgoing + (8 * slots) + (8 * List.length saved) + if calls then 8 else 0
  in
  let size = if payload = 0 then 0 else (payload + 15) / 16 * 16 in
  { size
  ; saved
  ; saves_ra = calls
  ; slot_offset = (fun index -> outgoing + (8 * index))
  ; saved_offset = (fun index -> outgoing + (8 * slots) + (8 * index))
  ; ra_offset = outgoing + (8 * slots) + (8 * List.length saved)
  }
;;

type context =
  { frame : frame
  ; locations : Regalloc.location Id.Map.t
  ; labels : string Id.Map.t
  ; arities : int Id.Map.t
  }

let internal message = raise (Codegen_failure (Internal message))

let location_of context id =
  match Id.Map.find_opt id context.locations with
  | Some place -> place
  | None -> internal ("no location for " ^ Id.name id)
;;

let code_label context code =
  match Id.Map.find_opt code context.labels with
  | Some label -> label
  | None -> internal ("no code label for " ^ Id.name code)
;;

let code_arity context code =
  match Id.Map.find_opt code context.arities with
  | Some arity -> arity
  | None -> internal ("no code arity for " ^ Id.name code)
;;

let slot_load context register index =
  Riscv.load register (context.frame.slot_offset index) Riscv.Sp
;;

let slot_store context register index =
  Riscv.store register (context.frame.slot_offset index) Riscv.Sp
;;

let move_to_location context source = function
  | Regalloc.Reg register ->
    if register = source then [] else [ Riscv.Mv (register, source) ]
  | Regalloc.Slot index -> slot_store context source index
;;

(* Where a computation writes its result, and how the result reaches its
   destination. Slot destinations compute in [t0] and store. *)
type destination =
  | Into of Regalloc.location
  | Drop

type target =
  | Tail
  | Nested of destination

let working_reg = function
  | Drop -> t0
  | Into (Regalloc.Reg register) -> register
  | Into (Regalloc.Slot _) -> t0
;;

let commit context destination register =
  match destination with
  | Drop | Into (Regalloc.Reg _) -> []
  | Into (Regalloc.Slot index) -> slot_store context register index
;;

let move_result context destination source =
  match destination with
  | Drop -> []
  | Into (Regalloc.Reg register) ->
    if register = source then [] else [ Riscv.Mv (register, source) ]
  | Into (Regalloc.Slot index) -> slot_store context source index
;;

(* The value of an atom in a register: a register variable is used directly, a
   slot is loaded and a constant or primitive is materialized. *)
let atom_to_reg context state register (atom : Anf.atom) : Riscv.instr list * state =
  match atom with
  | Anf.Constant constant -> [ Riscv.Li (register, tagged_constant constant) ], state
  | Anf.Var id ->
    (match location_of context id with
     | Regalloc.Reg source ->
       (if source = register then [] else [ Riscv.Mv (register, source) ]), state
     | Regalloc.Slot index -> slot_load context register index, state)
  | Anf.Primitive primitive ->
    [ Riscv.La (register, primitive_closure primitive) ], note_primitive primitive state
;;

(* An atom as an operand: no instructions when it already lives in a register. *)
let atom_operand context state temp (atom : Anf.atom)
  : Riscv.reg * Riscv.instr list * state
  =
  match atom with
  | Anf.Var id ->
    (match location_of context id with
     | Regalloc.Reg register -> register, [], state
     | Regalloc.Slot index -> temp, slot_load context temp index, state)
  | _ ->
    let code, state' = atom_to_reg context state temp atom in
    temp, code, state'
;;

(* Inline sequences for saturated primitives, shared by applications and the
   first-class wrappers below; operands are tagged words in registers. *)
let add_core destination a b =
  [ Riscv.Binop (Riscv.Add, destination, a, b)
  ; Riscv.Imm (Riscv.Addi, destination, destination, -1)
  ]
;;

let subtract_core destination a b =
  [ Riscv.Binop (Riscv.Sub, destination, a, b)
  ; Riscv.Imm (Riscv.Addi, destination, destination, 1)
  ]
;;

let multiply_core destination a b =
  [ Riscv.Imm (Riscv.Srai, t2, a, 1)
  ; Riscv.Imm (Riscv.Addi, t3, b, -1)
  ; Riscv.Binop (Riscv.Mul, t2, t2, t3)
  ; Riscv.Imm (Riscv.Addi, destination, t2, 1)
  ]
;;

(* The zero divisor check, always emitted even when the result is unused. *)
let divrem_check state ~divisor =
  let ok, state' = fresh_local state in
  ( [ Riscv.Li (t2, 1L)
    ; Riscv.Branch (Riscv.Bne, divisor, t2, ok)
    ; Riscv.Call Abi.div_by_zero
    ; Riscv.Label ok
    ]
  , state' )
;;

let divrem_core state ~remainder ~dividend ~divisor ~destination =
  let ok, state' = fresh_local state in
  ( [ Riscv.Li (t2, 1L)
    ; Riscv.Branch (Riscv.Bne, divisor, t2, ok)
    ; Riscv.Call Abi.div_by_zero
    ; Riscv.Label ok
    ; Riscv.Imm (Riscv.Srai, t2, dividend, 1)
    ; Riscv.Imm (Riscv.Srai, t3, divisor, 1)
    ; Riscv.Binop ((if remainder then Riscv.Rem else Riscv.Div), t2, t2, t3)
    ; Riscv.Imm (Riscv.Slli, t2, t2, 1)
    ; Riscv.Imm (Riscv.Addi, destination, t2, 1)
    ]
  , state' )
;;

(* The comparison bit of two tagged words. *)
let fast_bit primitive a b =
  match primitive with
  | Primitive.Less -> [ Riscv.Binop (Riscv.Slt, t2, a, b) ]
  | Primitive.Less_equal ->
    [ Riscv.Binop (Riscv.Slt, t2, b, a); Riscv.Imm (Riscv.Xori, t2, t2, 1) ]
  | Primitive.Greater -> [ Riscv.Binop (Riscv.Slt, t2, b, a) ]
  | Primitive.Greater_equal ->
    [ Riscv.Binop (Riscv.Slt, t2, a, b); Riscv.Imm (Riscv.Xori, t2, t2, 1) ]
  | Primitive.Equal -> [ Riscv.Binop (Riscv.Xor, t2, a, b); Riscv.Seqz (t2, t2) ]
  | Primitive.Not_equal -> [ Riscv.Binop (Riscv.Xor, t2, a, b); Riscv.Snez (t2, t2) ]
  | _ -> internal "comparison"
;;

(* The comparison bit from the raw [-1, 0, 1] result of [rt_compare]. *)
let derived_bit primitive =
  match primitive with
  | Primitive.Less -> [ Riscv.Imm (Riscv.Slti, t2, a0, 0) ]
  | Primitive.Less_equal -> [ Riscv.Imm (Riscv.Slti, t2, a0, 1) ]
  | Primitive.Greater -> [ Riscv.Binop (Riscv.Slt, t2, Riscv.Zero, a0) ]
  | Primitive.Greater_equal ->
    [ Riscv.Imm (Riscv.Slti, t2, a0, 0); Riscv.Imm (Riscv.Xori, t2, t2, 1) ]
  | Primitive.Equal -> [ Riscv.Seqz (t2, a0) ]
  | Primitive.Not_equal -> [ Riscv.Snez (t2, a0) ]
  | _ -> internal "comparison"
;;

(* Full comparison: the inline immediate test selects between a fast tagged
   comparison and the structural [rt_compare]. [keep_ra] preserves the return
   address around the call, for frameless wrappers. *)
let compare_core state primitive ~a ~b ~destination ~keep_ra =
  let slow, state' = fresh_local state in
  let join, state'' = fresh_local state' in
  let saved =
    if keep_ra
    then Riscv.add_imm Riscv.Sp Riscv.Sp (-16) @ Riscv.store Riscv.Ra 0 Riscv.Sp
    else []
  in
  let restored =
    if keep_ra
    then Riscv.load Riscv.Ra 0 Riscv.Sp @ Riscv.add_imm Riscv.Sp Riscv.Sp 16
    else []
  in
  ( [ Riscv.Binop (Riscv.And, t2, a, b)
    ; Riscv.Imm (Riscv.Andi, t2, t2, 1)
    ; Riscv.Beqz (t2, slow)
    ]
    @ fast_bit primitive a b
    @ [ Riscv.J join; Riscv.Label slow ]
    @ saved
    @ [ Riscv.Mv (a0, a); Riscv.Mv (a1, b); Riscv.Call Abi.compare ]
    @ restored
    @ derived_bit primitive
    @ [ Riscv.Label join
      ; Riscv.Imm (Riscv.Slli, t2, t2, 1)
      ; Riscv.Imm (Riscv.Addi, destination, t2, 1)
      ]
  , state'' )
;;

(* A discarded structural comparison keeps only its error effect. *)
let compare_check state ~a ~b =
  let done_label, state' = fresh_local state in
  ( [ Riscv.Binop (Riscv.And, t2, a, b)
    ; Riscv.Imm (Riscv.Andi, t2, t2, 1)
    ; Riscv.Bnez (t2, done_label)
    ; Riscv.Mv (a0, a)
    ; Riscv.Mv (a1, b)
    ; Riscv.Call Abi.compare
    ; Riscv.Label done_label
    ]
  , state' )
;;

let negate_core destination a =
  [ Riscv.Li (t2, 2L); Riscv.Binop (Riscv.Sub, destination, t2, a) ]
;;

let not_core destination a = [ Riscv.Imm (Riscv.Xori, destination, a, 2) ]

let boolean_core ~conjunctive destination a b =
  [ Riscv.Binop ((if conjunctive then Riscv.And else Riscv.Or), destination, a, b) ]
;;

let print_int_core ~keep_ra a =
  let saved =
    if keep_ra
    then Riscv.add_imm Riscv.Sp Riscv.Sp (-16) @ Riscv.store Riscv.Ra 0 Riscv.Sp
    else []
  in
  let restored =
    if keep_ra
    then Riscv.load Riscv.Ra 0 Riscv.Sp @ Riscv.add_imm Riscv.Sp Riscv.Sp 16
    else []
  in
  saved @ [ Riscv.Mv (a0, a); Riscv.Call Abi.print_int ] @ restored
;;

(* Call arguments: [a1] .. [a7] directly, the rest through [t0] in the
   outgoing stack area. *)
let setup_arguments context state (arguments : Anf.atom list) : Riscv.instr list * state =
  let rec loop index state code = function
    | [] -> code, state
    | atom :: rest ->
      let number = index + 1 in
      let argument_code, state' =
        if number <= 7
        then atom_to_reg context state (Riscv.A number) atom
        else (
          let code', state' = atom_to_reg context state t0 atom in
          code' @ Riscv.store t0 (8 * (number - 8)) Riscv.Sp, state')
      in
      loop (index + 1) state' (code @ argument_code) rest
  in
  loop 0 state [] arguments
;;

(* The argument vector of [rt_apply], re-read from the locations. *)
let restow_arguments context state (arguments : Anf.atom list) : Riscv.instr list * state =
  let rec loop index state code = function
    | [] -> code, state
    | atom :: rest ->
      let atom_code, state' = atom_to_reg context state t0 atom in
      loop
        (index + 1)
        state'
        (code @ atom_code @ Riscv.store t0 (8 * index) Riscv.Sp)
        rest
  in
  loop 0 state [] arguments
;;

(* A saturated primitive application, inlined. *)
let emit_primitive
      context
      state
      (destination : destination)
      primitive
      (arguments : Anf.atom list)
  : Riscv.instr list * state
  =
  let only () =
    match arguments with
    | [ atom ] -> atom
    | _ -> internal "primitive arity"
  in
  let binary () =
    match arguments with
    | [ first; second ] -> first, second
    | _ -> internal "primitive arity"
  in
  let operands () =
    let first, second = binary () in
    let a, code_a, state' = atom_operand context state t0 first in
    let b, code_b, state'' = atom_operand context state' t1 second in
    a, b, code_a @ code_b, state''
  in
  let comparison () =
    let register = working_reg destination in
    let first, second = binary () in
    if is_immediate first || is_immediate second
    then (
      let a, b, code, state' = operands () in
      ( code
        @ fast_bit primitive a b
        @ [ Riscv.Imm (Riscv.Slli, t2, t2, 1); Riscv.Imm (Riscv.Addi, register, t2, 1) ]
        @ commit context destination register
      , state' ))
    else (
      let a, b, code, state' = operands () in
      let core, state'' =
        compare_core state' primitive ~a ~b ~destination:register ~keep_ra:false
      in
      code @ core @ commit context destination register, state'')
  in
  match destination, primitive with
  | ( Drop
    , ( Primitive.Add
      | Primitive.Subtract
      | Primitive.Multiply
      | Primitive.Negate
      | Primitive.Not
      | Primitive.And
      | Primitive.Or ) ) -> [], state
  | Drop, (Primitive.Divide | Primitive.Modulo) ->
    let _, second = binary () in
    let divisor, code, state' = atom_operand context state t1 second in
    let check, state'' = divrem_check state' ~divisor in
    code @ check, state''
  | ( Drop
    , ( Primitive.Equal
      | Primitive.Not_equal
      | Primitive.Less
      | Primitive.Less_equal
      | Primitive.Greater
      | Primitive.Greater_equal ) ) ->
    let first, second = binary () in
    if is_immediate first || is_immediate second
    then [], state
    else (
      let a, b, code, state' = operands () in
      let check, state'' = compare_check state' ~a ~b in
      code @ check, state'')
  | Drop, Primitive.Print_int ->
    let a, code, state' = atom_operand context state t0 (only ()) in
    code @ print_int_core ~keep_ra:false a, state'
  | ( _
    , ( Primitive.Equal
      | Primitive.Not_equal
      | Primitive.Less
      | Primitive.Less_equal
      | Primitive.Greater
      | Primitive.Greater_equal ) ) -> comparison ()
  | _, (Primitive.Divide | Primitive.Modulo) ->
    let register = working_reg destination in
    let a, b, code, state' = operands () in
    let core, state'' =
      divrem_core
        state'
        ~remainder:(primitive = Primitive.Modulo)
        ~dividend:a
        ~divisor:b
        ~destination:register
    in
    code @ core @ commit context destination register, state''
  | _, Primitive.Print_int ->
    let a, code, state' = atom_operand context state t0 (only ()) in
    code @ print_int_core ~keep_ra:false a @ move_result context destination a0, state'
  | _, (Primitive.Add | Primitive.Subtract) ->
    let register = working_reg destination in
    let immediate =
      match primitive, arguments with
      | Primitive.Add, [ Anf.Constant (Frontend.Constant.Int literal); other ]
      | Primitive.Add, [ other; Anf.Constant (Frontend.Constant.Int literal) ] ->
        let value = integer_literal literal in
        if value >= -1024L && value <= 1023L
        then Some (other, 2 * Int64.to_int value)
        else None
      | Primitive.Subtract, [ first; Anf.Constant (Frontend.Constant.Int literal) ] ->
        let value = integer_literal literal in
        if value >= -1023L && value <= 1024L
        then Some (first, -2 * Int64.to_int value)
        else None
      | _ -> None
    in
    (match immediate with
     | Some (other, delta) ->
       let operand, code, state' = atom_operand context state t0 other in
       ( code
         @ [ Riscv.Imm (Riscv.Addi, register, operand, delta) ]
         @ commit context destination register
       , state' )
     | None ->
       let a, b, code, state' = operands () in
       let core =
         if primitive = Primitive.Add
         then add_core register a b
         else subtract_core register a b
       in
       code @ core @ commit context destination register, state')
  | _, (Primitive.Multiply | Primitive.And | Primitive.Or) ->
    let register = working_reg destination in
    let a, b, code, state' = operands () in
    let core =
      match primitive with
      | Primitive.Multiply -> multiply_core register a b
      | Primitive.And -> boolean_core ~conjunctive:true register a b
      | _ -> boolean_core ~conjunctive:false register a b
    in
    code @ core @ commit context destination register, state'
  | _, (Primitive.Negate | Primitive.Not) ->
    let register = working_reg destination in
    let a, code, state' = atom_operand context state t0 (only ()) in
    let core =
      if primitive = Primitive.Negate then negate_core register a else not_core register a
    in
    code @ core @ commit context destination register, state'
;;

(* Branch to [label] when [atom] is [false]; [false] is the immediate 1. *)
let condition_branch context state label (atom : Anf.atom) : Riscv.instr list * state =
  let tested, code, state' =
    match atom with
    | Anf.Var id ->
      (match location_of context id with
       | Regalloc.Reg register -> register, [], state
       | Regalloc.Slot index -> t1, slot_load context t1 index, state)
    | Anf.Constant _ | Anf.Primitive _ ->
      let code, state' = atom_to_reg context state t1 atom in
      t1, code, state'
  in
  Riscv.Li (t0, 1L) :: (code @ [ Riscv.Branch (Riscv.Beq, tested, t0, label) ]), state'
;;

let rec emit_term context state (target : target) (term : Lifted.expression)
  : Riscv.instr list * state
  =
  match term with
  | Return (Anf.If (condition, yes, no)) when target = Tail ->
    let else_label, state' = fresh_local state in
    let test, state'' = condition_branch context state' else_label condition in
    let yes_code, state_yes = emit_term context state'' Tail yes in
    let no_code, state_no = emit_term context state_yes Tail no in
    test @ yes_code @ [ Riscv.Label else_label ] @ no_code, state_no
  | Return computation ->
    (match target with
     | Tail ->
       let code, state' =
         emit_computation context state (Into (Regalloc.Reg a0)) computation
       in
       code @ epilogue context @ [ Riscv.Ret ], state'
     | Nested destination -> emit_computation context state destination computation)
  | Anf.Let (bound, computation, body) ->
    let code, state' =
      emit_computation context state (Into (location_of context bound)) computation
    in
    let rest, state'' = emit_term context state' target body in
    code @ rest, state''
  | Anf.Sequence (computation, body) ->
    let code, state' = emit_computation context state Drop computation in
    let rest, state'' = emit_term context state' target body in
    code @ rest, state''
  | Anf.Let_rec (bound, initializer_, body) ->
    let place = location_of context bound in
    let initializer_code, state' =
      match shape initializer_ with
      | Block (words, header) ->
        let setup =
          [ Riscv.Li (a0, Int64.of_int words)
          ; Riscv.Call Abi.alloc
          ; Riscv.Li (t0, header)
          ]
          @ Riscv.store t0 0 a0
          @ move_to_location context a0 place
        in
        let code, state' =
          emit_term context state (Nested (Into (Regalloc.Reg a0))) initializer_
        in
        let base, hold =
          match place with
          | Regalloc.Reg register -> register, []
          | Regalloc.Slot index -> t1, slot_load context t1 index
        in
        let copy =
          List.concat
            (List.init words (fun index ->
               Riscv.load t0 (8 * index) a0 @ Riscv.store t0 (8 * index) base))
        in
        setup @ code @ hold @ copy, state'
      | Dynamic ->
        let seed =
          match place with
          | Regalloc.Reg register -> [ Riscv.Li (register, 1L) ]
          | Regalloc.Slot index -> Riscv.Li (t0, 1L) :: slot_store context t0 index
        in
        let code, state' = emit_term context state (Nested (Into place)) initializer_ in
        seed @ code, state'
    in
    let rest, state'' = emit_term context state' target body in
    initializer_code @ rest, state''

and emit_computation context state (destination : destination) (computation : computation)
  : Riscv.instr list * state
  =
  match computation with
  | Anf.Atom _ when destination = Drop -> [], state
  | Anf.Atom atom ->
    let register = working_reg destination in
    let code, state' = atom_to_reg context state register atom in
    code @ commit context destination register, state'
  | Anf.Field (source, index) ->
    let base, code, state' = atom_operand context state t1 source in
    let register = working_reg destination in
    ( code
      @ Riscv.load register (Abi.tuple_field index) base
      @ commit context destination register
    , state' )
  | Anf.Tuple (first, second, rest) ->
    let fields = first :: second :: rest in
    let count = List.length fields in
    let rec stores index state code = function
      | [] -> code, state
      | field :: other ->
        let register, field_code, state' = atom_operand context state t0 field in
        stores
          (index + 1)
          state'
          (code @ field_code @ Riscv.store register (Abi.tuple_field index) a0)
          other
    in
    let field_stores, state' = stores 0 state [] fields in
    ( [ Riscv.Li (a0, Int64.of_int (count + 1))
      ; Riscv.Call Abi.alloc
      ; Riscv.Li (t0, Abi.tuple_header count)
      ]
      @ Riscv.store t0 0 a0
      @ field_stores
      @ move_result context destination a0
    , state' )
  | Anf.Function { code; captures = [] } when destination = Drop -> [], state
  | Anf.Function { code; captures = [] } ->
    let symbol = code_label context code ^ "_closure" in
    let state' = note_static code symbol state in
    let register = working_reg destination in
    Riscv.La (register, symbol) :: commit context destination register, state'
  | Anf.Function { code; captures } ->
    let arity = code_arity context code in
    let rec captures_store index state code = function
      | [] -> code, state
      | capture :: other ->
        let register, capture_code, state' = atom_operand context state t0 capture in
        captures_store
          (index + 1)
          state'
          (code @ capture_code @ Riscv.store register (Abi.closure_env index) a0)
          other
    in
    let env_stores, state' = captures_store 0 state [] captures in
    ( [ Riscv.Li (a0, Int64.of_int (3 + List.length captures))
      ; Riscv.Call Abi.alloc
      ; Riscv.Li (t0, Abi.closure_header)
      ]
      @ Riscv.store t0 0 a0
      @ (Riscv.La (t0, code_label context code) :: Riscv.store t0 Abi.closure_code a0)
      @ (Riscv.Li (t0, Int64.of_int arity) :: Riscv.store t0 Abi.closure_arity a0)
      @ env_stores
      @ move_result context destination a0
    , state' )
  | Anf.Apply (Lifted.Direct code, first, rest) ->
    let setup, state' = setup_arguments context state (first :: rest) in
    ( setup @ [ Riscv.Call (code_label context code) ] @ move_result context destination a0
    , state' )
  | Anf.Apply (Lifted.Indirect (Anf.Primitive primitive), first, rest)
    when 1 + List.length rest = primitive_arity primitive ->
    emit_primitive context state destination primitive (first :: rest)
  | Anf.Apply (Lifted.Indirect callee, first, rest) ->
    let arguments = first :: rest in
    let count = List.length arguments in
    let setup, state_setup = setup_arguments context state arguments in
    let callee_code, state_callee = atom_to_reg context state_setup a0 callee in
    let slow, state_slow = fresh_local state_callee in
    let done_label, state_done = fresh_local state_slow in
    let argv, state_argv = restow_arguments context state_done arguments in
    let callee_again, state_again = atom_to_reg context state_argv a0 callee in
    ( setup
      @ callee_code
      @ Riscv.load t0 Abi.closure_arity a0
      @ [ Riscv.Li (t1, Int64.of_int count); Riscv.Branch (Riscv.Bne, t0, t1, slow) ]
      @ Riscv.load t0 Abi.closure_code a0
      @ [ Riscv.Jalr t0; Riscv.J done_label; Riscv.Label slow ]
      @ argv
      @ callee_again
      @ [ Riscv.Li (a1, Int64.of_int count)
        ; Riscv.Mv (a2, Riscv.Sp)
        ; Riscv.Call Abi.apply
        ; Riscv.Label done_label
        ]
      @ move_result context destination a0
    , state_again )
  | Anf.If (condition, yes, no) ->
    let else_label, state' = fresh_local state in
    let test, state'' = condition_branch context state' else_label condition in
    let join, state_join = fresh_local state'' in
    let yes_code, state_yes = emit_term context state_join (Nested destination) yes in
    let no_code, state_no = emit_term context state_yes (Nested destination) no in
    ( test
      @ yes_code
      @ [ Riscv.J join; Riscv.Label else_label ]
      @ no_code
      @ [ Riscv.Label join ]
    , state_no )

and epilogue context =
  let frame = context.frame in
  let restore_ra =
    if frame.saves_ra then Riscv.load Riscv.Ra frame.ra_offset Riscv.Sp else []
  in
  let restores =
    List.concat
      (List.mapi
         (fun index register -> Riscv.load register (frame.saved_offset index) Riscv.Sp)
         frame.saved)
  in
  let adjust =
    if frame.size > 0 then Riscv.add_imm Riscv.Sp Riscv.Sp frame.size else []
  in
  restore_ra @ restores @ adjust
;;

let prologue context environment arguments =
  let frame = context.frame in
  let adjust =
    if frame.size > 0 then Riscv.add_imm Riscv.Sp Riscv.Sp (-frame.size) else []
  in
  let save_ra =
    if frame.saves_ra then Riscv.store Riscv.Ra frame.ra_offset Riscv.Sp else []
  in
  let saves =
    List.concat
      (List.mapi
         (fun index register -> Riscv.store register (frame.saved_offset index) Riscv.Sp)
         frame.saved)
  in
  let environment_moves =
    List.concat
      (List.mapi
         (fun index id ->
            Riscv.load t0 (Abi.closure_env index) a0
            @ move_to_location context t0 (location_of context id))
         environment)
  in
  let argument_moves =
    List.concat
      (List.mapi
         (fun index id ->
            let number = index + 1 in
            if number <= 7
            then move_to_location context (Riscv.A number) (location_of context id)
            else
              Riscv.load t0 (frame.size + (8 * (number - 8))) Riscv.Sp
              @ move_to_location context t0 (location_of context id))
         arguments)
  in
  adjust @ save_ra @ saves @ environment_moves @ argument_moves
;;

let make_context ~entry ~labels ~arities body =
  let allocation = Regalloc.allocate ~entry body in
  let outgoing, calls = scan_term body in
  let frame = make_frame allocation outgoing calls in
  { frame; locations = allocation.locations; labels; arities }
;;

let emit_body context state ~label ~environment ~arguments (body : Lifted.expression)
  : Riscv.instr list * state
  =
  let code, state' = emit_term context state Tail body in
  Riscv.Label label :: (prologue context environment arguments @ code), state'
;;

(* First-class wrappers for primitives used as values: the inline sequences of
   saturated applications, with the arguments in [a1] and [a2]. *)
let emit_wrapper state primitive =
  let name = primitive_wrapper primitive in
  let enter = [ Riscv.Label name ] in
  match primitive with
  | Primitive.Print_int -> enter @ print_int_core ~keep_ra:true a1 @ [ Riscv.Ret ], state
  | Primitive.Divide | Primitive.Modulo ->
    let core, state' =
      divrem_core
        state
        ~remainder:(primitive = Primitive.Modulo)
        ~dividend:a1
        ~divisor:a2
        ~destination:a0
    in
    enter @ core @ [ Riscv.Ret ], state'
  | ( Primitive.Equal
    | Primitive.Not_equal
    | Primitive.Less
    | Primitive.Less_equal
    | Primitive.Greater
    | Primitive.Greater_equal ) as operation ->
    let core, state' =
      compare_core state operation ~a:a1 ~b:a2 ~destination:a0 ~keep_ra:true
    in
    enter @ core @ [ Riscv.Ret ], state'
  | Primitive.Negate -> enter @ negate_core a0 a1 @ [ Riscv.Ret ], state
  | Primitive.Not -> enter @ not_core a0 a1 @ [ Riscv.Ret ], state
  | Primitive.Add -> enter @ add_core a0 a1 a2 @ [ Riscv.Ret ], state
  | Primitive.Subtract -> enter @ subtract_core a0 a1 a2 @ [ Riscv.Ret ], state
  | Primitive.Multiply -> enter @ multiply_core a0 a1 a2 @ [ Riscv.Ret ], state
  | Primitive.And ->
    enter @ boolean_core ~conjunctive:true a0 a1 a2 @ [ Riscv.Ret ], state
  | Primitive.Or ->
    enter @ boolean_core ~conjunctive:false a0 a1 a2 @ [ Riscv.Ret ], state
;;

let quad value = Riscv.Directive (".quad\t" ^ value)

(* A static closure: header, code address, raw arity. *)
let closure_data label code arity =
  [ Riscv.Directive ".p2align\t3"
  ; Riscv.Label label
  ; quad (Int64.to_string Abi.closure_header)
  ; quad code
  ; quad (string_of_int arity)
  ]
;;

let compile (program : Lifted.program) : (string, error) result =
  try
    let labels, state_labels =
      List.fold_left
        (fun (labels, state) (definition : Lifted.function_definition) ->
           let label, state' = fresh_code state (Id.name definition.code) in
           Id.Map.add definition.code label labels, state')
        (Id.Map.empty, initial_state)
        program.functions
    in
    let arities =
      List.fold_left
        (fun map (definition : Lifted.function_definition) ->
           Id.Map.add definition.code (1 + List.length definition.parameters) map)
        Id.Map.empty
        program.functions
    in
    let bodies, state_bodies =
      List.fold_left
        (fun (code, state) (definition : Lifted.function_definition) ->
           let environment = definition.environment in
           let arguments = definition.parameter :: definition.parameters in
           let context =
             make_context
               ~entry:(environment @ arguments)
               ~labels
               ~arities
               definition.body
           in
           let body, state' =
             emit_body
               context
               state
               ~label:(code_label context definition.code)
               ~environment
               ~arguments
               definition.body
           in
           code @ body, state')
        ([], state_labels)
        program.functions
    in
    let main_context = make_context ~entry:[] ~labels ~arities program.body in
    let main, state_main =
      emit_body
        main_context
        state_bodies
        ~label:Abi.entry
        ~environment:[]
        ~arguments:[]
        program.body
    in
    let wrappers, state_wrappers =
      List.fold_left
        (fun (code, state) primitive ->
           let wrapper, state' = emit_wrapper state primitive in
           code @ wrapper, state')
        ([], state_main)
        state_main.primitives
    in
    let static_closures =
      List.concat_map
        (fun (code, symbol) ->
           closure_data
             symbol
             (code_label main_context code)
             (code_arity main_context code))
        state_wrappers.statics
    in
    let primitive_closures =
      List.concat_map
        (fun primitive ->
           closure_data
             (primitive_closure primitive)
             (primitive_wrapper primitive)
             (primitive_arity primitive))
        state_wrappers.primitives
    in
    let text =
      [ Riscv.Directive ".text"; Riscv.Directive ".globl\tml_main" ]
      @ bodies
      @ main
      @ wrappers
    in
    let data = [ Riscv.Directive ".data" ] @ static_closures @ primitive_closures in
    Ok (Riscv.to_string (text @ data))
  with
  | Codegen_failure error -> Error error
;;
