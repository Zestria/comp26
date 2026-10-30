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
   stack slots, and can fail. *)
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

(* Where the variables are: [in_regs] are the variables whose value is still
   in a register (the parameters, the variable bound by the last [let]),
   [vars] are the stack slots, as offsets from [s0]. [depth] is the number of
   slots in use. *)
type frame =
  { vars : int Env.t
  ; in_regs : reg Env.t
  ; depth : int
  ; prefix : string
  }

let ret = A 0

(* The runtime function called on division by zero; it does not return. *)
let division_by_zero = "shmel_div_by_zero"
let tmp1 = T 0
let tmp2 = T 1

(* Address scratch register for offsets that do not fit into 12 bits. *)
let addr = T 6
let max_reg_args = 8
let slot_offset depth = -24 - (8 * depth)

(* Arguments after the eighth are on the stack, where [s0] points. *)
let stack_arg_offset i = 8 * (i - max_reg_args)

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

(* The code that puts the immediate into [rd]. *)
let load hooks frame rd = function
  | Anf.ImmInt n -> Ok [ Li (rd, n) ]
  | Anf.ImmVar x ->
    (match Env.find_opt x frame.in_regs, Env.find_opt x frame.vars with
     | Some r, _ -> Ok (if r = rd then [] else [ Mv (rd, r) ])
     | None, Some off -> Ok (load_slot rd off)
     | None, None -> hooks.load_global rd x)
;;

(* Like [load], but zero and a value already in a register need no
   instruction. *)
let operand hooks frame rd = function
  | Anf.ImmInt 0 -> Ok ([], Zero)
  | Anf.ImmVar x when Env.mem x frame.in_regs -> Ok ([], Env.find x frame.in_regs)
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
  | Ast.Div, _, Anf.ImmInt n when n <> 0 ->
    let* code, ra, rb = operands hooks frame a b in
    return (code @ [ R (Div, ret, ra, rb) ])
  | Ast.Div, _, _ ->
    (* [div] gives -1 for a zero divisor; OCaml raises Division_by_zero. *)
    let* code, ra, rb = operands hooks frame a b in
    let* n = fresh_label in
    let nonzero = Printf.sprintf ".L%s_div%d" frame.prefix n in
    return
      (code
       @ [ Branch (Bne, rb, Zero, nonzero)
         ; Call division_by_zero
         ; Label nonzero
         ; R (Div, ret, ra, rb)
         ])
  | (Ast.Add | Ast.Sub | Ast.Mul), _, _ ->
    let rop =
      match op with
      | Ast.Add -> Add
      | Ast.Sub -> Sub
      | _ -> Mul
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

let head = function
  | Anf.AExpr c | Anf.ALet (_, c, _) -> Some c
  | Anf.ALetRec _ -> None
;;

(* Reads of [x] by the very next computation, which can take it from a
   register. A call reads its arguments from the stack slots. *)
let next_uses x rest =
  match head rest with
  | Some (Anf.CImm i | Anf.CNeg i) -> imm_uses x i
  | Some (Anf.CBinOp (_, a, b)) -> imm_uses x a + imm_uses x b
  | Some (Anf.CIf (c, _, _)) -> cond_uses x c
  | Some (Anf.CApp _ | Anf.CFun _) | None -> 0
;;

(* Whether the code may call something, which destroys [a*] and [t*]. *)
let rec has_call = function
  | Anf.AExpr c -> cexpr_has_call c
  | Anf.ALet (_, c, rest) -> cexpr_has_call c || has_call rest
  | Anf.ALetRec _ -> true

and cexpr_has_call = function
  | Anf.CImm _ | Anf.CNeg _ | Anf.CBinOp _ -> false
  | Anf.CApp _ | Anf.CFun _ -> true
  | Anf.CIf (_, t, e) -> has_call t || has_call e
;;

(* What is left in the registers after computing [c] into [a0]. *)
let after frame c =
  let in_regs =
    if cexpr_has_call c then Env.empty else Env.filter (fun _ r -> r <> ret) frame.in_regs
  in
  { frame with in_regs }
;;

let new_slot frame x =
  let off = slot_offset frame.depth in
  let* () = use_slots (frame.depth + 1) in
  return (off, { frame with vars = Env.add x off frame.vars; depth = frame.depth + 1 })
;;

let rec expr hooks frame = function
  | Anf.AExpr c -> cexpr hooks frame c
  | Anf.ALet (x, c, rest) ->
    let* code = cexpr hooks frame c in
    let frame = after frame c in
    if String.equal x "_"
    then
      let* rest = expr hooks frame rest in
      return (code @ rest)
    else (
      let frame =
        { frame with
          vars = Env.remove x frame.vars
        ; in_regs = Env.add x ret frame.in_regs
        }
      in
      if uses x rest = next_uses x rest
      then
        (* Only the next computation reads [x]: it stays in [a0], no slot. *)
        let* rest = expr hooks frame rest in
        return (code @ rest)
      else
        let* off, frame = new_slot frame x in
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
    (* The call fills the argument registers, so it reads the stack only. *)
    lift (hooks.call ~load:(load hooks { frame with in_regs = Env.empty }) f args)
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

(* Reads of [x] before the first call, while [a1] .. [a7] still hold the
   parameters. A call reads its own arguments from the stack slots; in an [if]
   with a call only the condition is counted. *)
let rec uses_before_call x = function
  | Anf.AExpr c -> cexpr_uses_before_call x c
  | Anf.ALet (y, c, rest) when not (cexpr_has_call c) ->
    cexpr_uses x c + if String.equal x y then 0 else uses_before_call x rest
  | Anf.ALet (_, c, _) -> cexpr_uses_before_call x c
  | Anf.ALetRec _ -> 0

and cexpr_uses_before_call x c =
  match c with
  | Anf.CIf (cond, _, _) when cexpr_has_call c -> cond_uses x cond
  | c when cexpr_has_call c -> 0
  | c -> cexpr_uses x c
;;

(* A parameter in [a0] lives until the first computation, the ones in [a1]
   .. [a7] until the first call; a parameter read later is saved to a slot
   when the function starts. *)
let needs_slot body i p =
  let read_from_register = if i = 0 then next_uses p body else uses_before_call p body in
  uses p body > read_from_register
;;

let bind_param body (frame, spills) (i, p) =
  if String.equal p "_"
  then frame, spills
  else if i >= max_reg_args
  then (
    let vars = Env.add p (stack_arg_offset i) frame.vars in
    { frame with vars; in_regs = Env.remove p frame.in_regs }, spills)
  else (
    let frame =
      { frame with
        in_regs = Env.add p (A i) frame.in_regs
      ; vars = Env.remove p frame.vars
      }
    in
    if needs_slot body i p
    then (
      let off = slot_offset frame.depth in
      let frame =
        { frame with vars = Env.add p off frame.vars; depth = frame.depth + 1 }
      in
      frame, spills @ store_slot (A i) off)
    else frame, spills)
;;

let compile_body hooks ~name ~params body =
  let empty =
    { vars = Env.empty; in_regs = Env.empty; depth = 0; prefix = label_prefix name }
  in
  let frame, spills =
    List.fold_left (bind_param body) (empty, []) (List.mapi (fun i p -> i, p) params)
  in
  Result.map
    (fun (code, st) -> { code = spills @ code; locals = round16 (8 * st.slots) })
    (expr hooks frame body { labels = 0; slots = frame.depth })
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
