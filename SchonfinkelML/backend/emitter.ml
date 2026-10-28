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
end

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
    | Anf.ImmId name ->
      (match SMap.find_opt name env with
       | Some (Reg r) -> return r
       | _ -> error Symbol_table_error)
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
    (* We don't care about rec flag for now since we don't have tail rec optimization and even type checker *)
    | ALet (_, name, ACExpr (CAbstr (v, rhs)), body)
      when VarSet.is_empty (fv_aexpr (ACExpr (CAbstr (v, rhs)))) ->
      let vars, func_body, count = get_all_variables [ v ] 1 rhs in
      let env = SMap.add name (Function count) env in
      let new_env =
        List.fold_left
          (fun env (var, reg) -> SMap.add var (Reg reg) env)
          env
          (combine vars arg_registers)
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
    | ACExpr (CApp (ImmId x, y, z)) ->
      let emit_arg reg env = function
        | ImmNum n -> emit (li reg n)
        | ImmId id ->
          (match SMap.find_opt id env with
           | Some (Reg r) -> emit (mv reg r)
           | _ -> error Symbol_table_error)
        | ImmBool b -> emit (li reg (Bool.to_int b))
        | ImmUnit -> emit (li reg 0)
      in
      (match SMap.find_opt x env with
       | Some (Function arity) ->
         let* () =
           List.fold_left
             (fun m (name, arg_reg) -> m >>= fun () -> emit_arg arg_reg env name)
             (return ())
             (combine (y :: z) arg_registers)
         in
         let* () = emit (call x) in
         emit (mv dst A0)
       | _ -> error Invalid_application)
    | ACExpr (CApp (_, _, _)) -> error Invalid_application
  in
  let dst = alloc () in
  let env = SMap.empty in
  (let* () = compile_aexpr env dst anf in
   let* () = emit (li A7 94) in
   let* () = emit (mv A0 dst) in
   emit ecall)
  |> run
;;
