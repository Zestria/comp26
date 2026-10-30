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
    ; fresh_counter : int
    ; reg_counter : int
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
    let initial_state =
      { current_rev = []; functions_rev = []; fresh_counter = 0; reg_counter = 0 }
    in
    match program initial_state with
    | Error e -> Error e
    | Ok ((), final_state) ->
      let main_code = Label "_start" :: List.rev final_state.current_rev in
      let functions = List.concat (List.rev final_state.functions_rev) in
      Ok (main_code @ functions)
  ;;

  let fresh_label (prefix : string) : string t =
    fun state ->
    let n = state.fresh_counter in
    Ok (Printf.sprintf "%s_%d" prefix n, { state with fresh_counter = n + 1 })
  ;;

  let alloc () =
    fun state ->
    let n = state.reg_counter in
    Ok (List.nth Allocator.registers n, { state with reg_counter = n + 1 })
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
  let materialize env = function
    | Anf.ImmNum n ->
      let* reg = alloc () in
      let* () = emit (li reg n) in
      return reg
    | Anf.ImmBool b ->
      let* reg = alloc () in
      let* () = emit (li reg (Bool.to_int b)) in
      return reg
    | Anf.ImmUnit ->
      let* reg = alloc () in
      let* () = emit (li reg 0) in
      return reg
    | Anf.ImmId name ->
      (match SMap.find_opt name env with
       | Some (Reg r) -> return r
       | Some (Function _) ->
         let* reg = alloc () in
         let* () = emit (la reg name) in
         return reg
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
    let* arg_regs = map_m (materialize env) args in
    let saved = live_caller_saved env dst in
    (* Frame layout: staged arguments at 0.., then saved registers. Staging
       avoids the swap hazard when two arguments sit in registers that the
       A-register loading order would exchange. *)
    let args_bytes = 8 * List.length args in
    let frame = align16 (args_bytes + (8 * List.length saved)) in
    let* () = emit (addi SP SP (-frame)) in
    let* () = iteri_m (fun i r -> emit (sd r SP (8 * i))) arg_regs in
    let* () = iteri_m (fun j r -> emit (sd r SP (args_bytes + (8 * j)))) saved in
    let* () = iteri_m (fun i _ -> emit (ld (List.nth arg_registers i) SP (8 * i))) args in
    let* () = emit (call f) in
    (* Move the result out of A0 before restoring saved registers: restore
       may overwrite A0 if it held a named value (e.g. a function parameter
       used as a call argument). dst is excluded from [saved], so it survives
       restore. *)
    let* () = if dst = A0 then return () else emit (mv dst A0) in
    let* () = iteri_m (fun j r -> emit (ld r SP (args_bytes + (8 * j)))) saved in
    let* () = emit (addi SP SP frame) in
    return ()
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
           (* is_ref = 0: no type info at this stage, so the GC does not trace
              captures. Safe while the heap stays under the collection threshold. *)
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
      (* Stage f and the arguments on the stack (no register swaps), save
         live caller-saved registers, then call the runtime entry point. *)
      let* f_reg = materialize env f in
      let* arg_regs = map_m (materialize env) args in
      let saved = live_caller_saved env dst in
      let args_bytes = 8 * (1 + n) in
      let frame = align16 (args_bytes + (8 * List.length saved)) in
      let* () = emit (addi SP SP (-frame)) in
      let* () = emit (sd f_reg SP 0) in
      let* () = iteri_m (fun i r -> emit (sd r SP (8 * (i + 1)))) arg_regs in
      let* () = iteri_m (fun j r -> emit (sd r SP (args_bytes + (8 * j)))) saved in
      let* () = emit (ld A0 SP 0) in
      let* () =
        iteri_m
          (fun i _ -> emit (ld (List.nth arg_registers (i + 1)) SP (8 * (i + 1))))
          args
      in
      let* () =
        emit (call (if n = 1 then "schoml_closure_apply1" else "schoml_closure_apply2"))
      in
      (* Move the result out of A0 before restoring, same reasoning as
         compile_call. *)
      let* () = if dst = A0 then return () else emit (mv dst A0) in
      let* () = iteri_m (fun j r -> emit (ld r SP (args_bytes + (8 * j)))) saved in
      let* () = emit (addi SP SP frame) in
      return ()
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
    | "=" ->
      let* () = emit (xor dst rs1 rs2) in
      emit (sltiu dst dst 1)
    | "<>" ->
      let* () = emit (xor dst rs1 rs2) in
      emit (sltu dst Zero dst)
    | "<" -> emit (slt dst rs1 rs2)
    | ">" -> emit (slt dst rs2 rs1)
    | "<=" ->
      let* () = emit (slt dst rs2 rs1) in
      emit (xori dst dst 1)
    | ">=" ->
      let* () = emit (slt dst rs1 rs2) in
      emit (xori dst dst 1)
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
    (* | ALet (Recursive, _, _, _) -> error (Unsupported "recursive bindings") *)
    | ALet (_, name, rhs, body) ->
      let* reg = alloc () in
      let env = SMap.add name (Reg reg) env in
      let* () = compile_aexpr env reg rhs in
      compile_aexpr env dst body
    | ACExpr (CBinop (op, lhs, rhs)) -> compile_binop env dst op lhs rhs
    | ACExpr (CImmExpr imm) ->
      let* src = materialize env imm in
      emit (mv dst src)
    | ACExpr (CIfExpr (cond, then_branch, else_branch)) ->
      let* cond_reg = materialize env cond in
      let* else_lbl = fresh_label "if_else" in
      let* end_lbl = fresh_label "if_end" in
      let* () = emit (beq cond_reg Zero else_lbl) in
      let* () = compile_aexpr env dst then_branch in
      let* () = emit (jal Zero end_lbl) in
      let* () = label else_lbl in
      let* () = compile_aexpr env dst else_branch in
      let* () = label end_lbl in
      return ()
    | ACExpr (CAbstr (v, body)) -> error (Unsupported "closures")
    | ACExpr (CApp (ImmId f, y, [])) when f = "print_int" ->
      compile_call env dst "schoml_print_int" [ y ]
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
  let env = SMap.empty in
  (let* dst = alloc () in
   let* () = compile_aexpr env dst anf in
   let* () = emit (li A7 94) in
   let* () = emit (mv A0 Zero) in
   emit ecall)
  |> run
;;
