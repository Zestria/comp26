[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_middleend
open Riscv
module Ast = Shmel_lib.Ast
module Env = Map.Make (String)

type hooks =
  { load_global : reg -> string -> (instr list, string) result
  ; call :
      load:(reg -> Anf.imm -> (instr list, string) result)
      -> Anf.imm
      -> Anf.imm list
      -> (instr list, string) result
  }

let no_calls =
  { load_global = (fun _ x -> Error (Printf.sprintf "unbound variable %s" x))
  ; call = (fun ~load:_ _ _ -> Error "function calls are not supported here")
  }
;;

type body =
  { code : instr list
  ; locals : int
  }

(* Compilation threads the number of labels used and the largest number of
   local slots, and can fail. *)
type state =
  { labels : int
  ; slots : int
  }

type 'a t = state -> ('a * state, string) result

let return x st = Ok (x, st)

let ( let* ) m f st =
  match m st with
  | Ok (x, st) -> f x st
  | Error e -> Error e
;;

let lift r st = Result.map (fun x -> x, st) r
let fail msg _ = Error msg
let fresh_label st = Ok (st.labels, { st with labels = st.labels + 1 })
let use_slots n st = Ok ((), { st with slots = max st.slots n })

(* Where the variables are: offsets from [s0], and how many locals are live.
   [in_ret] is the variable whose value is still in [a0] right after its
   [let], so the next instruction can take it from there instead of the
   stack. *)
type frame =
  { vars : int Env.t
  ; depth : int
  ; prefix : string
  ; in_ret : string option
  }

let ret = A 0
let tmp1 = T 0
let tmp2 = T 1

(* Address scratch register for offsets that do not fit into 12 bits. *)
let addr = T 6
let slot_offset depth = -24 - (8 * depth)
let param_offset i = 8 * i

let load_slot rd off =
  if fits_imm12 off
  then [ Ld (rd, off, Fp) ]
  else [ Li (addr, off); R (Add, addr, addr, Fp); Ld (rd, 0, addr) ]
;;

let store_slot rs off =
  if fits_imm12 off
  then [ Sd (rs, off, Fp) ]
  else [ Li (addr, off); R (Add, addr, addr, Fp); Sd (rs, 0, addr) ]
;;

let in_ret frame x = Option.equal String.equal frame.in_ret (Some x)

(* The code that puts the immediate into [rd]. *)
let load hooks frame rd = function
  | Anf.ImmInt n -> Ok [ Li (rd, n) ]
  | Anf.ImmVar x when in_ret frame x -> Ok (if rd = ret then [] else [ Mv (rd, ret) ])
  | Anf.ImmVar x ->
    (match Env.find_opt x frame.vars with
     | Some off -> Ok (load_slot rd off)
     | None -> hooks.load_global rd x)
;;

(* Like [load], but zero and a value already in [a0] need no extra register. *)
let operand hooks frame rd = function
  | Anf.ImmInt 0 -> Ok ([], Zero)
  | Anf.ImmVar x when in_ret frame x -> Ok ([], ret)
  | imm -> Result.map (fun code -> code, rd) (load hooks frame rd imm)
;;

let operands hooks frame a b =
  let* ca, ra = lift (operand hooks frame tmp1 a) in
  let* cb, rb = lift (operand hooks frame tmp2 b) in
  return (ca @ cb, ra, rb)
;;

let comparison_value op ra rb =
  match op with
  | Ast.Lt -> Ok [ R (Slt, ret, ra, rb) ]
  | Ast.Gt -> Ok [ R (Slt, ret, rb, ra) ]
  | Ast.Le -> Ok [ R (Slt, ret, rb, ra); I (Xori, ret, ret, 1) ]
  | Ast.Ge -> Ok [ R (Slt, ret, ra, rb); I (Xori, ret, ret, 1) ]
  | Ast.Eq when rb = Zero -> Ok [ Seqz (ret, ra) ]
  | Ast.Eq -> Ok [ R (Xor, ret, ra, rb); Seqz (ret, ret) ]
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> Error "not a comparison"
;;

let binop hooks frame op a b =
  match op, a, b with
  (* Small constants go into the instruction itself. *)
  | Ast.Add, x, Anf.ImmInt n when fits_imm12 n ->
    let* code, r = lift (operand hooks frame tmp1 x) in
    return (code @ [ I (Addi, ret, r, n) ])
  | Ast.Add, Anf.ImmInt n, x when fits_imm12 n ->
    let* code, r = lift (operand hooks frame tmp1 x) in
    return (code @ [ I (Addi, ret, r, n) ])
  | Ast.Sub, x, Anf.ImmInt n when fits_imm12 (-n) ->
    let* code, r = lift (operand hooks frame tmp1 x) in
    return (code @ [ I (Addi, ret, r, -n) ])
  | Ast.Lt, x, Anf.ImmInt n when fits_imm12 n ->
    let* code, r = lift (operand hooks frame tmp1 x) in
    return (code @ [ I (Slti, ret, r, n) ])
  | (Ast.Add | Ast.Sub | Ast.Mul | Ast.Div), _, _ ->
    let rop =
      match op with
      | Ast.Add -> Add
      | Ast.Sub -> Sub
      | Ast.Mul -> Mul
      | _ -> Div
    in
    let* code, ra, rb = operands hooks frame a b in
    return (code @ [ R (rop, ret, ra, rb) ])
  | (Ast.Eq | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge), _, _ ->
    let* code, ra, rb = operands hooks frame a b in
    let* cmp = lift (comparison_value op ra rb) in
    return (code @ cmp)
;;

(* The branch taken when the comparison is false. *)
let branch_unless op ra rb target =
  match op with
  | Ast.Eq -> Ok (Branch (Bne, ra, rb, target))
  | Ast.Lt -> Ok (Branch (Bge, ra, rb, target))
  | Ast.Le -> Ok (Branch (Bgt, ra, rb, target))
  | Ast.Gt -> Ok (Branch (Ble, ra, rb, target))
  | Ast.Ge -> Ok (Branch (Blt, ra, rb, target))
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> Error "not a comparison"
;;

let jump_unless hooks frame cond target =
  match cond with
  | Anf.CondCmp (op, a, b) ->
    let* code, ra, rb = operands hooks frame a b in
    let* br = lift (branch_unless op ra rb target) in
    return (code @ [ br ])
  | Anf.CondImm c ->
    let* code, r = lift (operand hooks frame tmp1 c) in
    return (code @ [ Branch (Beq, r, Zero, target) ])
;;

(* How many times [x] is read in an expression. *)
let imm_uses x = function
  | Anf.ImmVar y when String.equal x y -> 1
  | Anf.ImmVar _ | Anf.ImmInt _ -> 0
;;

let cond_uses x = function
  | Anf.CondImm i -> imm_uses x i
  | Anf.CondCmp (_, a, b) -> imm_uses x a + imm_uses x b
;;

let rec uses x = function
  | Anf.AExpr c -> cexpr_uses x c
  | Anf.ALet (y, c, rest) -> cexpr_uses x c + if String.equal x y then 0 else uses x rest
  | Anf.ALetRec (f, params, body, rest) ->
    let shadowed = String.equal x f in
    (if shadowed || List.mem x params then 0 else uses x body)
    + if shadowed then 0 else uses x rest

and cexpr_uses x = function
  | Anf.CImm i | Anf.CNeg i -> imm_uses x i
  | Anf.CBinOp (_, a, b) -> imm_uses x a + imm_uses x b
  | Anf.CApp (f, args) -> List.fold_left (fun n a -> n + imm_uses x a) (imm_uses x f) args
  | Anf.CIf (c, t, e) -> cond_uses x c + uses x t + uses x e
  | Anf.CFun (params, _) when List.mem x params -> 0
  | Anf.CFun (_, body) -> uses x body
;;

(* Reads of [x] by the very next computation, which takes it from [a0]. *)
let next_uses x rest =
  let head =
    match rest with
    | Anf.AExpr c | Anf.ALet (_, c, _) -> c
    | Anf.ALetRec _ -> Anf.CImm (Anf.ImmInt 0)
  in
  match head with
  | Anf.CImm i | Anf.CNeg i -> imm_uses x i
  | Anf.CBinOp (_, a, b) -> imm_uses x a + imm_uses x b
  | Anf.CIf (c, _, _) -> cond_uses x c
  | Anf.CApp _ | Anf.CFun _ -> 0
;;

let rec expr hooks frame = function
  | Anf.AExpr c -> cexpr hooks frame c
  | Anf.ALet (x, c, rest) ->
    let* code = cexpr hooks frame c in
    if String.equal x "_"
    then
      let* rest = expr hooks { frame with in_ret = None } rest in
      return (code @ rest)
    else if uses x rest = next_uses x rest
    then
      (* Only the next computation reads [x]: it stays in [a0], no slot. *)
      let* rest = expr hooks { frame with in_ret = Some x } rest in
      return (code @ rest)
    else (
      let off = slot_offset frame.depth in
      let* () = use_slots (frame.depth + 1) in
      let frame =
        { frame with
          vars = Env.add x off frame.vars
        ; depth = frame.depth + 1
        ; in_ret = Some x
        }
      in
      let* rest = expr hooks frame rest in
      return (code @ store_slot ret off @ rest))
  | Anf.ALetRec (f, _, _, _) ->
    fail (Printf.sprintf "local let rec %s must be lifted before code generation" f)

and cexpr hooks frame = function
  | Anf.CImm i -> lift (load hooks frame ret i)
  | Anf.CNeg i ->
    let* code, r = lift (operand hooks frame tmp1 i) in
    return (code @ [ Neg (ret, r) ])
  | Anf.CBinOp (op, a, b) -> binop hooks frame op a b
  | Anf.CApp (f, args) ->
    (* The call may use [a0] while it puts the arguments in place. *)
    lift (hooks.call ~load:(load hooks { frame with in_ret = None }) f args)
  | Anf.CIf (c, t, e) ->
    let* n = fresh_label in
    let else_ = Printf.sprintf ".L%s_else%d" frame.prefix n in
    let end_ = Printf.sprintf ".L%s_end%d" frame.prefix n in
    let* test = jump_unless hooks frame c else_ in
    let* t = expr hooks frame t in
    let* e = expr hooks frame e in
    return (test @ t @ [ J end_; Label else_ ] @ e @ [ Label end_ ])
  | Anf.CFun _ -> fail "fun must be lifted before code generation"
;;

(* Labels may only contain letters, digits and underscores. *)
let label_prefix name =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
      | _ -> '_')
    name
;;

let round16 n = (n + 15) / 16 * 16

let compile_body hooks ~name ~params body =
  let vars =
    List.fold_left
      (fun (vars, i) p -> Env.add p (param_offset i) vars, i + 1)
      (Env.empty, 0)
      params
    |> fst
  in
  let frame = { vars; depth = 0; prefix = label_prefix name; in_ret = None } in
  Result.map
    (fun (code, st) -> { code; locals = round16 (8 * st.slots) })
    (expr hooks frame body { labels = 0; slots = 0 })
;;

let adjust_sp delta =
  if delta = 0
  then []
  else if fits_imm12 delta
  then [ I (Addi, Sp, Sp, delta) ]
  else [ Li (addr, delta); R (Add, Sp, Sp, addr) ]
;;

let prologue locals =
  [ I (Addi, Sp, Sp, -16); Sd (Ra, 8, Sp); Sd (Fp, 0, Sp); I (Addi, Fp, Sp, 16) ]
  @ adjust_sp (-locals)
;;

let epilogue =
  [ I (Addi, Sp, Fp, -16); Ld (Ra, 8, Sp); Ld (Fp, 0, Sp); I (Addi, Sp, Sp, 16); Ret ]
;;

let compile_function hooks ~name ~params body =
  Result.map
    (fun { code; locals } -> (Label name :: prologue locals) @ code @ epilogue)
    (compile_body hooks ~name ~params body)
;;
