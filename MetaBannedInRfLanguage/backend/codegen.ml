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
  ; functions : (string * int) SMap.t (* assembly symbol and arity *)
  ; wrappers : (string * int) SMap.t (* builtin wrappers requested by the program *)
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

  let rec map_m f = function
    | [] -> return []
    | value :: rest ->
      let* value = f value in
      let+ rest = map_m f rest in
      value :: rest
  ;;
end

open StateResult

let initial_state =
  { env = SMap.empty
  ; functions = SMap.empty
  ; wrappers = SMap.empty
  ; stack_offset = -16 (* fp - 8 and fp - 16 hold saved ra and fp. *)
  ; next_label = 0
  ; code_rev = []
  }
;;

let run computation =
  Result.map
    (fun (value, state) -> value, List.rev state.code_rev)
    (StateResult.run initial_state computation)
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

let rec gen_globals = function
  | [] -> return ()
  | symbol :: rest ->
    let* () = emit label symbol in
    let* () = emit_directive (Quad 1) in
    gen_globals rest
;;

(* ---------- Labels ---------- *)

let fresh_label =
  let* state = get in
  let name = Printf.sprintf ".L%d" state.next_label in
  let* () = put { state with next_label = state.next_label + 1 } in
  return name
;;

(* ---------- Variables ---------- *)

(* Shadow any function with the same name. *)
let bind_var name location =
  modify (fun state ->
    { state with
      env = SMap.add name location state.env
    ; functions = SMap.remove name state.functions
    })
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
      ; functions = SMap.remove name state.functions
      }
  in
  return offset
;;

(* ---------- Function bindings ---------- *)

let lookup_function name =
  let* state = get in
  return (SMap.find_opt name state.functions)
;;

let register_function name ~symbol ~arity =
  if arity < 0
  then fail (Printf.sprintf "negative arity for function %S" name)
  else
    modify (fun state ->
      { state with
        functions = SMap.add name (symbol, arity) state.functions
      ; env = SMap.remove name state.env
      })
;;

let declare_function name params =
  let* symbol = fresh_label in
  let* () = register_function name ~symbol ~arity:(List.length params) in
  return symbol
;;

let get_builtin_wrapper name ~arity =
  let* state = get in
  match SMap.find_opt name state.wrappers with
  | Some (symbol, _) -> return symbol
  | None ->
    let* symbol = fresh_label in
    let* () =
      modify (fun state ->
        { state with wrappers = SMap.add name (symbol, arity) state.wrappers })
    in
    return symbol
;;

let lookup_builtin name =
  match Value.primitive_arity name with
  | Some arity ->
    let+ symbol = get_builtin_wrapper name ~arity in
    Some (symbol, arity)
  | None when String.equal name "print_int" -> return (Some ("print_int_closure", 1))
  | None -> return None
;;

(* ---------- Memory accesses ---------- *)

let load_word ~dst ~base offset =
  if offset >= -2048 && offset <= 2047
  then emit ld dst { offset; base }
  else (
    let address = if dst <> base then dst else if dst = t0 then t1 else t0 in
    let* () = emit li address offset in
    let* () = emit add address base address in
    emit ld dst { offset = 0; base = address })
;;

let store_word ~src ~base offset =
  if offset >= -2048 && offset <= 2047
  then emit sd src { offset; base }
  else (
    let address =
      if src <> t0 && base <> t0 then t0 else if src <> t1 && base <> t1 then t1 else t2
    in
    let* () = emit li address offset in
    let* () = emit add address base address in
    emit sd src { offset = 0; base = address })
;;

let store_local ~src offset = store_word ~src ~base:fp offset

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

let adjust_sp bytes =
  if bytes = 0
  then return ()
  else if bytes >= -2048 && bytes <= 2047
  then emit addi sp sp bytes
  else
    let* () = emit li t0 bytes in
    emit add sp sp t0
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

(* Both main and ML functions get a frame sized from the generated body. *)
let emit_function ~symbol ~frame_size body_rev =
  let* () = emit label symbol in
  let* () = prologue ~frame_size in
  let* () = modify (fun state -> { state with code_rev = body_rev @ state.code_rev }) in
  epilogue ~frame_size
;;

(* ---------- Closure allocation ---------- *)

let alloc_closure ~symbol ~arity ~received =
  let* () = emit la a0 symbol in
  let* () = emit li a1 arity in
  let* () = emit li a2 received in
  let* () = if received = 0 then emit li A3 0 else emit mv A3 sp in
  emit call "closure_alloc"
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
    let* fn = lookup_function name in
    let* state = get in
    let* fn =
      match fn with
      | None when not (SMap.mem name state.env) -> lookup_builtin name
      | _ -> return fn
    in
    (match fn with
     | Some (symbol, arity) ->
       let* () = alloc_closure ~symbol ~arity ~received:0 in
       if dst = a0 then return () else emit mv dst a0
     | None ->
       let* location = lookup_var name in
       (match location with
        | In_reg src -> if src = dst then return () else emit mv dst src
        | On_stack offset -> load_word ~dst ~base:fp offset
        | Global name ->
          let* () = emit la dst name in
          emit ld dst { offset = 0; base = dst }))
;;

let get_imm_register = function
  | ImmVar name ->
    let+ state = get in
    (match SMap.find_opt name state.env with
     | Some (In_reg reg) -> Some reg
     | Some (On_stack _ | Global _) | None -> None)
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

let gen_add_immediate ~dst value n =
  if n = 0
  then gen_imm ~dst value
  else
    let* src = gen_operand ~scratch:dst value in
    emit addi dst src (2 * n)
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

let gen_arithmetic ~dst op args =
  match op, args with
  | "~+", [ arg ] -> gen_imm ~dst arg
  | "~-", [ arg ] -> gen_negation ~dst arg
  | "+", [ value; ImmInt n ] when n >= -1024 && n <= 1023 ->
    gen_add_immediate ~dst value n
  | "+", [ ImmInt n; value ] when n >= -1024 && n <= 1023 ->
    gen_add_immediate ~dst value n
  | "-", [ value; ImmInt n ] when n >= -1023 && n <= 1024 ->
    gen_add_immediate ~dst value (-n)
  | "/", [ value; ImmInt 1 ] -> gen_imm ~dst value
  | "/", [ value; ImmInt -1 ] -> gen_negation ~dst value
  | ("+" | "-" | "*" | "/"), [ left; right ] -> gen_binop ~dst op left right
  | ("+" | "-" | "*" | "/" | "~+" | "~-"), _ ->
    fail (Printf.sprintf "wrong number of arguments to %S: %d" op (List.length args))
  | _ -> fail (Printf.sprintf "unsupported arithmetic operator %S" op)
;;

(* ---------- Comparisons and booleans ---------- *)

let gen_not ~dst arg =
  let* src = gen_operand ~scratch:dst arg in
  let* () = emit sub dst x0 src in
  emit addi dst dst 4 (* not false = 4 - 1; not true = 4 - 3 *)
;;

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

(* ---------- Function calls ---------- *)

(* Calling convention:
   Caller:
   - passes a0 = pointer to a contiguous array of tagged arguments on its stack,
     aligned to 16 bytes, and releases it after the call.
   Callee:
   - saves ra/fp, copies parameters into its own frame before evaluating the
     body; returns the result in a0.
   - no live ML values depend on caller-saved registers.
   C runtime calls (e.g. print_int) use the usual RISC-V calling convention. *)

let with_call_frame args body =
  let arg_size = ((8 * List.length args) + 15) / 16 * 16 in
  let rec store_args offset = function
    | [] -> return ()
    | arg :: rest ->
      let* () = gen_imm ~dst:t0 arg in
      let* () = store_word ~src:t0 ~base:sp offset in
      store_args (offset + 8) rest
  in
  let* () = adjust_sp (-arg_size) in
  let* () = store_args 0 args in
  let* () = body in
  adjust_sp arg_size
;;

let gen_direct_call ~symbol args =
  with_call_frame
    args
    (let* () = emit mv a0 sp in
     emit call symbol)
;;

let gen_partial_application ~symbol ~arity args =
  let received = List.length args in
  with_call_frame args (alloc_closure ~symbol ~arity:(arity - received) ~received)
;;

let gen_apply fn args =
  with_call_frame
    args
    (* Building arguments may allocate closures and clobber a0, so load the
       callee only after the argument array is complete. *)
    (let* () = gen_imm ~dst:a0 fn in
     let* () = emit li a1 (List.length args) in
     let* () = emit mv a2 sp in
     emit call "apply")
;;

(* ---------- ANF expressions ---------- *)

(* Each expression leaves its result in a0. Values that must survive a call
   are stored in the frame or in globals. *)
let rec gen_cexpr = function
  | CImm imm -> gen_imm ~dst:a0 imm
  | CApp (ImmVar name, args) ->
    let* state = get in
    (match SMap.find_opt name state.functions with
     | Some (symbol, arity) when arity = List.length args -> gen_direct_call ~symbol args
     | Some (symbol, arity) when List.length args < arity ->
       gen_partial_application ~symbol ~arity args
     | Some _ -> gen_apply (ImmVar name) args
     | None ->
       if SMap.mem name state.env
       then gen_apply (ImmVar name) args
       else (
         match Value.primitive_arity name with
         | Some arity when List.length args < arity ->
           let* symbol = get_builtin_wrapper name ~arity in
           gen_partial_application ~symbol ~arity args
         | _ ->
           (match name, args with
            | ("+" | "-" | "*" | "/" | "~+" | "~-"), _ -> gen_arithmetic ~dst:a0 name args
            | ("not" | "&&" | "||" | "<" | "<=" | ">" | ">=" | "=" | "<>"), _ ->
              gen_logic ~dst:a0 name args
            | "print_int", [ arg ] ->
              let* () = gen_imm ~dst:a0 arg in
              let* () = emit call "print_int" in
              emit li a0 1 (* The C function returns void; the ML result is unit. *)
            | "print_int", _ -> fail "print_int expects one argument"
            | _ -> fail (Printf.sprintf "unsupported function %S" name))))
  | CApp (fn, args) -> gen_apply fn args
  | CIte (ImmBool true, then_branch, _) -> gen_aexpr then_branch
  | CIte (ImmBool false, _, else_branch) -> gen_aexpr else_branch
  | CIte (cond, then_branch, else_branch) ->
    let* else_label = fresh_label in
    let* end_label = fresh_label in
    let* () = gen_imm ~dst:a0 cond in
    (* Tagged false is 1, not zero. *)
    let* () = emit li t0 1 in
    let* () = emit beq a0 t0 else_label in
    let* () = gen_aexpr then_branch in
    let* () = emit j end_label in
    let* () = emit label else_label in
    let* () = gen_aexpr else_branch in
    emit label end_label
  | CFun _ -> fail "unexpected function expression after lambda lifting"
  | CTuple _ -> fail "tuple generation is not implemented yet"
  | CMatchFailure -> fail "match failure generation is not implemented yet"

and gen_aexpr = function
  | ACExpr expr -> gen_cexpr expr
  | ALet (name, value, body) ->
    let* outer = get in
    let* () = gen_cexpr value in
    let* offset = alloc_local name in
    let* () = store_local ~src:a0 offset in
    let* () = gen_aexpr body in
    modify (fun state -> { state with env = outer.env; functions = outer.functions })
  | ALetTuple _ -> fail "tuple destructuring is not implemented yet"
  | ALetRec _ -> fail "unexpected local recursive binding after lambda lifting"
;;

(* ---------- Functions ---------- *)

let gen_function ~symbol params body =
  let* outer = get in
  let* () = put { outer with code_rev = []; stack_offset = initial_state.stack_offset } in
  let rec bind_params offset = function
    | [] -> return ()
    | name :: rest ->
      let* () = load_word ~dst:t0 ~base:a0 offset in
      let* local = alloc_local name in
      let* () = store_local ~src:t0 local in
      bind_params (offset + 8) rest
  in
  let* () = bind_params 0 params in
  let* () = gen_aexpr body in
  let* frame_size = get_frame_size in
  let* body_state = get in
  let* () = put { body_state with code_rev = [] } in
  let* () = emit_function ~symbol ~frame_size body_state.code_rev in
  let* generated = get in
  (* Keep the caller's environment and frame, but never reuse generated labels. *)
  let* () =
    put { outer with next_label = generated.next_label; wrappers = generated.wrappers }
  in
  return generated.code_rev
;;

(* A wrapper has the usual ML argument-buffer convention. Its body uses the
   primitive directly, even if a user binding shadows that name later. *)
let gen_builtin_wrappers =
  let* outer = get in
  let* () = put { outer with env = SMap.empty; functions = SMap.empty } in
  let* code =
    map_m
      (fun (name, (symbol, arity)) ->
         let params = List.init arity (fun index -> Printf.sprintf "arg%d" index) in
         let args = List.map (fun name -> ImmVar name) params in
         gen_function ~symbol params (ACExpr (CApp (ImmVar name, args))))
      (SMap.bindings outer.wrappers)
  in
  let* generated = get in
  let* () =
    put { outer with next_label = generated.next_label; wrappers = generated.wrappers }
  in
  return code
;;

(* ---------- Top-level items ---------- *)

let rec gen_items = function
  | [] -> return ([], [])
  | AValue (name, ACExpr (CFun (params, body))) :: rest ->
    let* symbol = fresh_label in
    (* A non-recursive body sees the previous binding of its own name. *)
    let* code = gen_function ~symbol params body in
    let* () = register_function name ~symbol ~arity:(List.length params) in
    let+ globals, functions = gen_items rest in
    globals, code :: functions
  | AValue (name, body) :: rest ->
    let* () = gen_aexpr body in
    let* symbol = fresh_label in
    let* () = emit la t0 symbol in
    let* () = emit sd a0 { offset = 0; base = t0 } in
    let* () = bind_var name (Global symbol) in
    let+ globals, functions = gen_items rest in
    symbol :: globals, functions
  | AEval body :: rest ->
    let* () = gen_aexpr body in
    gen_items rest
  | ARec bindings :: rest ->
    (* Every member is visible while compiling any body of a recursive group. *)
    let* declared =
      map_m
        (function
          | name, CFun (params, body) ->
            let+ symbol = declare_function name params in
            symbol, params, body
          | _ -> fail "a recursive binding must define a function")
        bindings
    in
    let* code =
      map_m (fun (symbol, params, body) -> gen_function ~symbol params body) declared
    in
    let+ globals, functions = gen_items rest in
    globals, code @ functions
;;

(* ---------- Entry point ---------- *)

let gen_program (program : Anf.program) =
  let generate =
    (* Generate the body first to learn how much frame space it needs. *)
    let* globals, functions = gen_items program in
    let* wrappers = gen_builtin_wrappers in
    (* C main returns an untagged exit status. *)
    let* () = emit li a0 0 in
    let* frame_size = get_frame_size in
    let* body = get in
    let* () = put { body with code_rev = [] } in
    let* () =
      match globals with
      | [] -> return ()
      | _ ->
        let* () = emit_directive (Section Data) in
        let* () = emit_directive (P2align 3) in
        gen_globals globals
    in
    let* () = emit_directive (Section Text) in
    let* () = emit_directive (Globl "main") in
    let* () = emit_function ~symbol:"main" ~frame_size body.code_rev in
    modify (fun state ->
      { state with
        code_rev = List.concat (List.rev (functions @ wrappers)) @ state.code_rev
      })
  in
  Result.map snd (run generate)
;;
