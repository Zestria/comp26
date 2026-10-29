[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Common
open Allocator
module Anf = SchonfinkelML_middleend.Anf
module SMap = Map.Make (String)

type symbol_entry =
  | Reg of reg
  | Function of int

type item =
  | Instruction of instr
  | Label of string

module Monad = struct
  type state =
    { current_rev : item list
    ; functions_rev : item list list
    }

  type 'a t = state -> ('a * state, error) result

  let return (x : 'a) : 'a t = fun state -> Ok (x, state)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun state ->
    match m state with
    | Error e -> Error e
    | Ok (x, state) -> f x state
  ;;

  let ( let* ) = bind
  let ( >>= ) = bind

  let emit (instruction : instr checked) : unit t =
    fun state ->
    match instruction with
    | Error e -> Error e
    | Ok instruction ->
      Ok ((), { state with current_rev = Instruction instruction :: state.current_rev })
  ;;

  let error e = fun _state -> Error e

  let label (name : string) : unit t =
    fun state ->
    if String.trim name = ""
    then Error (Invalid_label name)
    else Ok ((), { state with current_rev = Label name :: state.current_rev })
  ;;

  let function_ (name : string) (body : unit t) : unit t =
    fun state ->
    if String.trim name = ""
    then Error (Invalid_label name)
    else (
      let isolated_state = { state with current_rev = [] } in
      let function_body =
        let* () = label name in
        body
      in
      match function_body isolated_state with
      | Error e -> Error e
      | Ok ((), compiled_state) ->
        let function_code = List.rev compiled_state.current_rev in
        Ok
          ( ()
          , { state with functions_rev = function_code :: compiled_state.functions_rev }
          ))
  ;;

  let run (program : unit t) : (item list, error) result =
    let initial_state = { current_rev = []; functions_rev = [] } in
    match program initial_state with
    | Error e -> Error e
    | Ok ((), final_state) ->
      let main_code = Label "_start" :: List.rev final_state.current_rev in
      let functions = List.concat (List.rev final_state.functions_rev) in
      Ok (main_code @ functions)
  ;;

  let rec map_m f = function
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = map_m f xs in
      return (y :: ys)
  ;;

  let iteri_m f l =
    snd
      (List.fold_left (fun (i, m) x -> i + 1, bind m (fun () -> f i x)) (0, return ()) l)
  ;;
end

let align16 n = (n + 15) land lnot 15

let rec get_all_variables acc len_acc = function
  | Anf.ACExpr (CAbstr (v, body)) -> get_all_variables (v :: acc) (len_acc + 1) body
  | t -> acc, t, len_acc
;;

let rec combine l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | a1 :: l1, a2 :: l2 -> (a1, a2) :: combine l1 l2
  | _, _ -> []
;;

let get_rtl anf =
  let open Monad in
  let open Allocator in
  let label_counter = ref 0 in
  let fresh base =
    incr label_counter;
    Printf.sprintf "%s__%d" base !label_counter
  in
  let materialize env = function
    | Anf.ImmId name ->
      (match SMap.find_opt name env with
       | Some (Reg r) -> return r
       | Some (Function _) ->
         let reg = alloc () in
         let* () = emit (la reg name) in
         return reg
       | None -> error Symbol_table_error)
    | Anf.ImmNum n ->
      let reg = alloc () in
      let* () = emit (li reg n) in
      return reg
    | Anf.ImmBool b ->
      let reg = alloc () in
      let* () = emit (li reg (Bool.to_int b)) in
      return reg
    | Anf.ImmUnit ->
      let reg = alloc () in
      let* () = emit (li reg 0) in
      return reg
  in
  let emit_arg reg env = function
    | Anf.ImmNum n -> emit (li reg n)
    | Anf.ImmBool b -> emit (li reg (Bool.to_int b))
    | Anf.ImmUnit -> emit (li reg 0)
    | Anf.ImmId id ->
      (match SMap.find_opt id env with
       | Some (Reg r) -> emit (mv reg r)
       | Some (Function _) -> emit (la reg id)
       | None -> error Symbol_table_error)
  in
  let live_caller_saved env dst =
    SMap.fold
      (fun _ entry acc ->
         match entry with
         | Reg r
           when r <> dst && r <> RA && List.mem r caller_saved && not (List.mem r acc) ->
           r :: acc
         | _ -> acc)
      env
      []
  in
  let compile_call env dst f args =
    let* () =
      List.fold_left
        (fun m (arg, arg_reg) -> m >>= fun () -> emit_arg arg_reg env arg)
        (return ())
        (combine args arg_registers)
    in
    let* () = emit (call f) in
    emit (mv dst A0)
  in
  let compile_mk_closure env dst f arity args =
    let* caps = map_m (materialize env) args in
    let saved = live_caller_saved env dst in
    let caps_bytes = 16 * List.length caps in
    let frame = align16 (caps_bytes + (8 * List.length saved)) in
    let* () = emit (addi SP SP (-frame)) in
    let* () =
      iteri_m
        (fun i r ->
           let* () = emit (sd Zero SP (16 * i)) in
           emit (sd r SP ((16 * i) + 8)))
        caps
    in
    let* () = iteri_m (fun j r -> emit (sd r SP (caps_bytes + (8 * j)))) saved in
    let* () = emit (la A0 f) in
    let* () = emit (li A1 arity) in
    let* () = emit (li A2 (List.length caps)) in
    let* () = emit (mv A3 SP) in
    let* () = emit (call "schoml_closure_make") in
    let* () = if dst = A0 then return () else emit (mv dst A0) in
    let* () = iteri_m (fun j r -> emit (ld r SP (caps_bytes + (8 * j)))) saved in
    emit (addi SP SP frame)
  in
  let compile_apply_closure env dst f args =
    let n = List.length args in
    if n > 2
    then error (Unsupported "apply with more than 2 arguments")
    else
      let* f_reg = materialize env f in
      let* () = emit (addi SP SP (-16)) in
      let* () = emit (sd f_reg SP 0) in
      let tmp_regs = List.filteri (fun i _ -> i < n) [ T3; T4 ] in
      let* () =
        iteri_m (fun i _ -> emit_arg (List.nth tmp_regs i) env (List.nth args i)) args
      in
      let* () = emit (ld T0 SP 0) in
      let* () = emit (addi SP SP 16) in
      let raw = fresh "apply_raw" in
      let a0 = fresh "apply_a0" in
      let a1 = fresh "apply_a1" in
      let a2 = fresh "apply_a2" in
      let fail = fresh "apply_fail" in
      let done_ = fresh "apply_done" in
      let* () = emit (andi T1 T0 1) in
      let* () = emit (beq T1 Zero raw) in
      let* () = emit (addi T0 T0 (-1)) in
      let* () = emit (ld T1 T0 16) in
      let* () = emit (ld T2 T0 8) in
      let* () = emit (beq T2 Zero a0) in
      let* () = emit (li T5 1) in
      let* () = emit (beq T2 T5 a1) in
      let* () = emit (li T5 2) in
      let* () = emit (beq T2 T5 a2) in
      let* () = emit (jal Zero fail) in
      let* () = label a0 in
      let* () =
        iteri_m
          (fun i _ -> emit (mv (List.nth arg_registers i) (List.nth tmp_regs i)))
          args
      in
      let* () = emit (jalr RA T1 0) in
      let* () = emit (jal Zero done_) in
      let* () = label a1 in
      let* () = emit (ld A0 T0 32) in
      let* () =
        iteri_m
          (fun i _ -> emit (mv (List.nth arg_registers (i + 1)) (List.nth tmp_regs i)))
          args
      in
      let* () = emit (jalr RA T1 0) in
      let* () = emit (jal Zero done_) in
      let* () = label a2 in
      let* () = emit (ld A0 T0 32) in
      let* () = emit (ld A1 T0 48) in
      let* () =
        iteri_m
          (fun i _ -> emit (mv (List.nth arg_registers (i + 2)) (List.nth tmp_regs i)))
          args
      in
      let* () = emit (jalr RA T1 0) in
      let* () = emit (jal Zero done_) in
      let* () = label raw in
      let* () =
        iteri_m
          (fun i _ -> emit (mv (List.nth arg_registers i) (List.nth tmp_regs i)))
          args
      in
      let* () = emit (jalr RA T0 0) in
      let* () = emit (jal Zero done_) in
      let* () = label fail in
      let* () = emit (li A0 0) in
      let* () = label done_ in
      emit (mv dst A0)
  in
  let compile_binop env dst op lhs rhs =
    let* rs1 = materialize env lhs in
    let* rs2 = materialize env rhs in
    match op with
    | "+" -> emit (add dst rs1 rs2)
    | "-" -> emit (sub dst rs1 rs2)
    | "*" -> emit (mul dst rs1 rs2)
    | "/" -> emit (div dst rs1 rs2)
    | "mod" -> emit (rem dst rs1 rs2)
    | "land" | "&&" -> emit (and_ dst rs1 rs2)
    | "lor" | "||" -> emit (or_ dst rs1 rs2)
    | "lxor" -> emit (xor dst rs1 rs2)
    | "lsl" -> emit (sll dst rs1 rs2)
    | "lsr" -> emit (srl dst rs1 rs2)
    | "asr" -> emit (sra dst rs1 rs2)
    | _ -> error (Unsupported "binop")
  in
  let rec compile_aexpr env dst =
    let open Anf in
    function
    | ALet (_, name, ACExpr (CAbstr (v, rhs)), body)
      when VarSet.is_empty
             (VarSet.filter
                (fun x ->
                   x <> name
                   &&
                   match SMap.find_opt x env with
                   | Some (Function _) -> false
                   | _ -> true)
                (fv_aexpr (ACExpr (CAbstr (v, rhs))))) ->
      let vars, func_body, count = get_all_variables [ v ] 1 rhs in
      let env = SMap.add name (Function count) env in
      let new_env =
        List.fold_left
          (fun env (var, reg) -> SMap.add var (Reg reg) env)
          env
          (combine (List.rev vars) arg_registers)
      in
      let* () =
        function_
          name
          (let* () = emit (addi SP SP (-16)) in
           let* () = emit (sd RA SP 8) in
           let* () = emit (sd S0 SP 0) in
           let* () = compile_aexpr new_env A0 func_body in
           let* () = emit (ld S0 SP 0) in
           let* () = emit (ld RA SP 8) in
           let* () = emit (addi SP SP 16) in
           emit ret)
      in
      compile_aexpr env dst body
    | ALet (Recursive, _, _, _) -> error (Unsupported "recursive bindings")
    | ALet (NotRecursive, name, rhs, body) ->
      let reg = alloc () in
      let env = SMap.add name (Reg reg) env in
      let* () = compile_aexpr env reg rhs in
      compile_aexpr env dst body
    | ACExpr (CBinop (op, lhs, rhs)) -> compile_binop env dst op lhs rhs
    | ACExpr (CImmExpr imm) ->
      let* src = materialize env imm in
      emit (mv dst src)
    | ACExpr (CIfExpr _) -> error (Unsupported "if")
    | ACExpr (CAbstr (v, body)) -> error (Unsupported "closures")
    | ACExpr (CApp (head, y, z)) ->
      let args = y :: z in
      let arity_of f =
        match SMap.find_opt f env with
        | Some (Function n) -> Some n
        | _ -> None
      in
      (match Closures.classify ~arity_of head (List.length args) with
       | Closures.Direct f -> compile_call env dst f args
       | Closures.Partial (f, arity) -> compile_mk_closure env dst f arity args
       | Closures.Over _ -> error (Unsupported "over-application")
       | Closures.Unknown -> compile_apply_closure env dst head args)
  in
  let dst = alloc () in
  run
    (let* () = compile_aexpr SMap.empty dst anf in
     let* () = emit (li A7 94) in
     let* () = emit (mv A0 dst) in
     let* () = emit ecall in
     return ())
;;
