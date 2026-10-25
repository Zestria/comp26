[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Riscv
open Anf
module SMap = Map.Make (String)

(* ---------- State monad ---------- *)

type location =
  | In_reg of reg
  | On_stack of int (* offset from fp *)
  | Global of string

type state =
  { env : location SMap.t
  ; arities : int SMap.t
  ; stack_offset : int (* lowest allocated offset for locals *)
  ; next_label : int
  ; code_rev : asm (* accumulated assembly, reversed *)
  }

module StateResult = struct
  type 'a t = state -> ('a * state, string) result

  let return value state = Ok (value, state)
  let fail message _state = Error message

  let ( let* ) computation f state =
    match computation state with
    | Ok (value, state) -> f value state
    | Error message -> Error message
  ;;

  let ( let+ ) computation f =
    let* value = computation in
    return (f value)
  ;;

  let get state = Ok (state, state)
  let put state _old_state = Ok ((), state)
  let modify f state = Ok ((), f state)
  let run initial_state computation = computation initial_state
end

open StateResult

let initial_state =
  { env = SMap.empty
  ; arities = SMap.empty
  ; stack_offset = -16 (* fp - 8 and fp - 16 hold saved ra and fp. *)
  ; next_label = 0
  ; code_rev = []
  }
;;

(* ---------- Code emission ---------- *)

let emit ?(comm = "") instruction =
  instruction (fun i ->
    match i with
    | (Addi (_, _, n) | Ld (_, { offset = n; _ }) | Sd (_, { offset = n; _ }))
      when n < -2048 || n > 2047 ->
      fail "instruction immediate or offset is outside [-2048, 2047]"
    | (Slli (_, _, n) | Srai (_, _, n)) when n < 0 || n > 63 ->
      fail "shift amount is outside [0, 63]"
    | _ ->
      modify (fun state ->
        let code_rev =
          if String.equal comm "" then state.code_rev else Comment comm :: state.code_rev
        in
        { state with code_rev = Instruction i :: code_rev }))
;;

let emit_directive directive =
  match directive with
  | P2align n when n < 0 -> fail "negative alignment exponent"
  | Zero bytes when bytes < 0 -> fail "negative zero-fill size"
  | _ ->
    modify (fun state -> { state with code_rev = Directive directive :: state.code_rev })
;;

(* ---------- Labels ---------- *)

let fresh_label =
  let* state = get in
  let name = Printf.sprintf ".L%d" state.next_label in
  let* () = put { state with next_label = state.next_label + 1 } in
  return name
;;

(* ---------- Variables ---------- *)

let bind_var name location =
  modify (fun state -> { state with env = SMap.add name location state.env })
;;

let lookup_var name =
  let* state = get in
  match SMap.find_opt name state.env with
  | Some location -> return location
  | None -> fail (Printf.sprintf "unbound variable %S" name)
;;

let alloc_local name =
  let* state = get in
  let offset = state.stack_offset - 8 in
  let* () =
    put
      { state with
        stack_offset = offset
      ; env = SMap.add name (On_stack offset) state.env
      }
  in
  return offset
;;

(* ---------- Functions ---------- *)

let get_function_arity name =
  let* state = get in
  return (SMap.find_opt name state.arities)
;;

let register_function name ~arity =
  if arity < 0
  then fail (Printf.sprintf "negative arity for function %S" name)
  else modify (fun state -> { state with arities = SMap.add name arity state.arities })
;;

(* ---------- Frame layout ---------- *)

let get_frame_size =
  let* state = get in
  return ((-state.stack_offset + 15) / 16 * 16)
;;

(* The frame size includes saved ra and fp and preserves 16-byte alignment. *)
let check_frame_size frame_size =
  if frame_size < 16 || frame_size mod 16 <> 0
  then fail "frame size must be a multiple of 16 and at least 16"
  else return ()
;;

(* fp is the caller's sp. Saved ra is at fp - 8, saved fp at fp - 16,
   and local values start at fp - 24. *)
let prologue ~frame_size =
  let* () = check_frame_size frame_size in
  if frame_size <= 2032
  then
    let* () = emit addi sp sp (-frame_size) in
    let* () = emit sd ra { offset = frame_size - 8; base = sp } in
    let* () = emit sd fp { offset = frame_size - 16; base = sp } in
    emit addi fp sp frame_size
  else
    (* A large frame does not fit in addi's signed 12-bit immediate. *)
    let* () = emit li t0 frame_size in
    let* () = emit sub sp sp t0 in
    let* () = emit add t0 sp t0 in
    let* () = emit sd ra { offset = -8; base = t0 } in
    let* () = emit sd fp { offset = -16; base = t0 } in
    emit mv fp t0
;;

(* Any temporary stack space used by the body must be released before this
   epilogue. The result is already in a0 and is left unchanged. *)
let epilogue ~frame_size =
  let* () = check_frame_size frame_size in
  let* () =
    if frame_size <= 2032
    then
      let* () = emit ld ra { offset = frame_size - 8; base = sp } in
      let* () = emit ld fp { offset = frame_size - 16; base = sp } in
      emit addi sp sp frame_size
    else
      let* () = emit ld ra { offset = -8; base = fp } in
      let* () = emit ld t0 { offset = -16; base = fp } in
      let* () = emit mv sp fp in
      emit mv fp t0
  in
  emit ret
;;

(* ---------- Operands ---------- *)

(* ML values are tagged words: integers use 2*n+1, false/unit use 1, true
   uses 3. Heap values are aligned pointers with the low bit clear.
   Raw machine quantities such as frame sizes and offsets are not tagged. *)

let gen_imm ~dst = function
  | ImmInt n ->
    if n >= min_int / 2 && n <= max_int / 2
    then emit li dst ((n lsl 1) lor 1)
    else
      (* Form a full-width tagged constant in the target register. *)
      let* () = emit li dst n in
      let* () = emit slli dst dst 1 in
      emit addi dst dst 1
  | ImmBool b -> emit li dst (if b then 3 else 1)
  | ImmUnit -> emit li dst 1
  | ImmVar name ->
    let* location = lookup_var name in
    (match location with
     | In_reg src -> if src = dst then return () else emit mv dst src
     | On_stack offset ->
       if offset >= -2048 && offset <= 2047
       then emit ld dst { offset; base = fp }
       else
         let* () = emit li dst offset in
         let* () = emit add dst fp dst in
         emit ld dst { offset = 0; base = dst }
     | Global name ->
       let* () = emit la dst name in
       emit ld dst { offset = 0; base = dst })
;;

let get_imm_register = function
  | ImmVar name ->
    let* location = lookup_var name in
    (match location with
     | In_reg reg -> return (Some reg)
     | On_stack _ | Global _ -> return None)
  | ImmInt _ | ImmBool _ | ImmUnit -> return None
;;

(* Reuse a value's register when possible; otherwise load into scratch. *)
let gen_operand ~scratch imm =
  let* reg = get_imm_register imm in
  match reg with
  | Some reg -> return reg
  | None ->
    let* () = gen_imm ~dst:scratch imm in
    return scratch
;;

(* Choose scratch registers without overwriting the other operand. Values
   already in registers need no moves, even when they occupy t0 and t1. *)
let gen_binary_operands left right =
  let* right_reg = get_imm_register right in
  let left_scratch = if right_reg = Some T0 then t1 else t0 in
  let* left_reg = gen_operand ~scratch:left_scratch left in
  let right_scratch = if left_reg = T1 then t0 else t1 in
  let+ right_reg = gen_operand ~scratch:right_scratch right in
  left_reg, right_reg
;;

(* ---------- Arithmetic ---------- *)

let tag_result dst =
  let* () = emit slli dst dst 1 in
  emit addi dst dst 1
;;

(* Decode both inputs into t0/t1. *)
let untag_operands left_reg right_reg =
  match left_reg, right_reg with
  | T1, T0 ->
    let* () = emit mv t2 t0 in
    let* () = emit srai t0 t1 1 in
    emit srai t1 t2 1
  | _, T0 ->
    let* () = emit srai t1 t0 1 in
    emit srai t0 left_reg 1
  | _ ->
    let* () = emit srai t0 left_reg 1 in
    emit srai t1 right_reg 1
;;

let gen_negation ~dst arg =
  let* src = gen_operand ~scratch:dst arg in
  let* () = emit sub dst x0 src in
  emit addi dst dst 2 (* tag(-n) = 2 - tag(n) *)
;;

(* Load operands, choose the operation, return a tagged result.
  Addition and subtraction can work on tagged values directly. *)
let gen_binop ~dst op left right =
  let* left_reg, right_reg = gen_binary_operands left right in
  match op with
  | "+" ->
    let* () = emit add dst left_reg right_reg in
    emit addi dst dst (-1)
  | "-" ->
    let* () = emit sub dst left_reg right_reg in
    emit addi dst dst 1
  | "*" ->
    let* () = untag_operands left_reg right_reg in
    let* () = emit mul dst t0 t1 in
    tag_result dst
  | "/" ->
    let* () = untag_operands left_reg right_reg in
    let* () =
      match right with
      | ImmInt n when n <> 0 -> return ()
      | _ ->
        let* nonzero = fresh_label in
        let* () = emit bne t1 x0 nonzero in
        let* () = emit call "division_by_zero" in
        emit label nonzero
    in
    let* () = emit div dst t0 t1 in
    tag_result dst
  | _ -> fail (Printf.sprintf "unsupported arithmetic operator %S" op)
;;

let gen_add_immediate ~dst value n =
  if n = 0
  then gen_imm ~dst value
  else
    let* src = gen_operand ~scratch:dst value in
    emit addi dst src (2 * n)
;;

let gen_arithmetic ~dst op args =
  match op, args with
  | "+", [ arg ] -> gen_imm ~dst arg
  | "-", [ arg ] -> gen_negation ~dst arg
  | "+", [ value; ImmInt n ] when n >= -1024 && n <= 1023 ->
    gen_add_immediate ~dst value n
  | "+", [ ImmInt n; value ] when n >= -1024 && n <= 1023 ->
    gen_add_immediate ~dst value n
  | "-", [ value; ImmInt n ] when n >= -1023 && n <= 1024 ->
    gen_add_immediate ~dst value (-n)
  | "/", [ value; ImmInt 1 ] -> gen_imm ~dst value
  | "/", [ value; ImmInt -1 ] -> gen_negation ~dst value
  | ("+" | "-" | "*" | "/"), [ left; right ] -> gen_binop ~dst op left right
  | ("+" | "-" | "*" | "/"), _ ->
    fail (Printf.sprintf "wrong number of arguments to %S: %d" op (List.length args))
  | _ -> fail (Printf.sprintf "unsupported arithmetic operator %S" op)
;;

(* ---------- Comparisons and booleans ---------- *)

let gen_comparison ~dst op left right =
  let* left_reg, right_reg = gen_binary_operands left right in
  let* () =
    match op with
    | "<" -> emit slt dst left_reg right_reg
    | ">" -> emit slt dst right_reg left_reg
    | "<=" ->
      let* () = emit slt dst right_reg left_reg in
      emit seqz dst dst
    | ">=" ->
      let* () = emit slt dst left_reg right_reg in
      emit seqz dst dst
    | "=" ->
      let* () = emit xor dst left_reg right_reg in
      emit seqz dst dst
    | "<>" ->
      let* () = emit xor dst left_reg right_reg in
      emit snez dst dst
    | _ -> fail (Printf.sprintf "unsupported comparison operator %S" op)
  in
  tag_result dst
;;

let gen_not ~dst arg =
  let* src = gen_operand ~scratch:dst arg in
  let* () = emit sub dst x0 src in
  emit addi dst dst 4 (* not false = 4 - 1; not true = 4 - 3 *)
;;

let gen_logic ~dst op args =
  match op, args with
  | "not", [ arg ] -> gen_not ~dst arg
  | ("&&" | "||"), [ left; right ] ->
    (* For false = 1 and true = 3, bitwise and/or preserve the boolean tag. *)
    let* left_reg, right_reg = gen_binary_operands left right in
    if String.equal op "&&"
    then emit and_ dst left_reg right_reg
    else emit or_ dst left_reg right_reg
  | ("<" | "<=" | ">" | ">=" | "=" | "<>"), [ left; right ] ->
    gen_comparison ~dst op left right
  | ("not" | "&&" | "||" | "<" | "<=" | ">" | ">=" | "=" | "<>"), _ ->
    fail (Printf.sprintf "wrong number of arguments to %S: %d" op (List.length args))
  | _ -> fail (Printf.sprintf "unsupported logical operator %S" op)
;;

(* ---------- Entry point ---------- *)

let run computation =
  Result.map
    (fun (value, state) -> value, List.rev state.code_rev)
    (StateResult.run initial_state computation)
;;

let gen_program (_ : Anf.program) =
  Result.map snd (run (fail "RISC-V program generation is not implemented yet"))
;;
