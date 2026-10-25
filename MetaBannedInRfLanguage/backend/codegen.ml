[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Riscv
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

(* ---------- Entry point ---------- *)

let run computation =
  Result.map
    (fun (value, state) -> value, List.rev state.code_rev)
    (StateResult.run initial_state computation)
;;

let gen_program (_ : Anf.program) =
  Result.map snd (run (fail "RISC-V program generation is not implemented yet"))
;;
