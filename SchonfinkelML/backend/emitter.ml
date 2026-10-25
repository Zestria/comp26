[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Common
open Allocator
module Anf = SchonfinkelML_middleend.Anf

type item =
  | Instruction of instr
  | Label of string

module Monad = struct
  type 'a t = item list -> ('a * item list, error) result

  let return (x : 'a) : 'a t = fun code -> Ok (x, code)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun code ->
    match m code with
    | Error e -> Error e
    | Ok (x, code) -> f x code
  ;;

  let ( let* ) = bind

  let emit (instruction : instr checked) : unit t =
    fun code ->
    match instruction with
    | Error e -> Error e
    | Ok instruction -> Ok ((), Instruction instruction :: code)
  ;;

  let error (e : error) : unit t = fun code -> Error e

  let label (name : string) : unit t =
    fun code ->
    if String.trim name = ""
    then Error (Invalid_label name)
    else Ok ((), Label name :: code)
  ;;

  let start : unit t = label "_start"
  let function_label (name : string) : unit t = label name

  let run (program : unit t) : (item list, error) result =
    match program [] with
    | Error e -> Error e
    | Ok ((), reversed_code) -> Ok (List.rev reversed_code)
  ;;
end

let get_rtl anf =
  let open Monad in
  let open Allocator in
  let materialize env = function
    | Anf.ImmId name -> return (find_var env name)
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
  let rec compile_aexpr env dst = function
    | Anf.ALet (flag, name, rhs, body) ->
      (match flag with
       | Recursive -> error (Unsupported "recursive bindings")
       | NotRecursive ->
         let reg = alloc () in
         let* () = compile_aexpr env reg rhs in
         compile_aexpr ((name, reg) :: env) dst body)
    | Anf.ACExpr cexpr ->
      (match cexpr with
       | Anf.CBinop (op, lhs, rhs) -> compile_binop env dst op lhs rhs
       | Anf.CImmExpr imm ->
         let* src = materialize env imm in
         emit (mv dst src)
       | Anf.CIfExpr _ -> error (Unsupported "if")
       | Anf.CApp _ -> error (Unsupported "applications")
       | Anf.CAbstr _ -> error (Unsupported "applications"))
  in
  let dst = alloc () in
  run
    (let* () = start in
     let* () = compile_aexpr [] dst anf in
     let* () = emit (li A7 94) in
     let* () = emit (mv A0 dst) in
     let* () = emit ecall in
     return ())
;;
