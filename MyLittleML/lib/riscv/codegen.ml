[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V 64 backend. Lexical ANF is lowered to explicit heap closures,
    then to integer instructions. See README-riscv.md for the ABI. *)

open Mylittleml.Middleend.Anf
module Reg = Program.Reg
module A = Asm
module M = Mylittleml.State_error
open M

let unsupported fmt = Printf.ksprintf M.fail fmt

(* ---------------------------------------------------------------------- *)
(* Stage 1: lambda-lifted ANF -> Program.program                           *)
(* ---------------------------------------------------------------------- *)

module S = Set.Make (String)

let conv_bop = function
  | Mylittleml.Ast.Add -> Program.Add
  | Mylittleml.Ast.Sub -> Program.Sub
  | Mylittleml.Ast.Mul -> Program.Mul
  | Mylittleml.Ast.Div -> Program.Div
  | Mylittleml.Ast.Mod -> Program.Mod
  | Mylittleml.Ast.Lt -> Program.Lt
  | Mylittleml.Ast.Le -> Program.Le
  | Mylittleml.Ast.Gt -> Program.Gt
  | Mylittleml.Ast.Ge -> Program.Ge
  | Mylittleml.Ast.Eq -> Program.Eq
  | Mylittleml.Ast.Neq -> Program.Ne
;;

let free_atom = function
  | AVar x -> S.singleton x
  | AInt _ | ABool _ -> S.empty
;;

let rec free_exp = function
  | ACexp c -> free_cexp c
  | ALet (flag, x, rhs, rest) ->
    let fv = free_exp rhs in
    let fv = if flag = Rec then S.remove x fv else fv in
    S.union fv (S.remove x (free_exp rest))

and free_cexp = function
  | CAtom a | CNeg a -> free_atom a
  | CBinop (_, a, b) | CApp (a, b) -> S.union (free_atom a) (free_atom b)
  | CIf (a, t, f) -> S.union (free_atom a) (S.union (free_exp t) (free_exp f))
  | CFun (params, body) -> List.fold_right S.remove params (free_exp body)
;;

(** Lexical closure conversion and lifting are performed together for the
    native backend. Every application consumes exactly one argument. Thus
    partial application, returned functions and unknown callees use the same
    ABI, without guessing arities or delaying effectful ANF bindings. *)
type lower_state =
  { next : int
  ; functions : Program.function_ list
  }

let fresh_function =
  let* state = get in
  let* () = put { state with next = state.next + 1 } in
  return (".Lml_fn" ^ string_of_int state.next)
;;

let add_function fn =
  modify (fun state -> { state with functions = fn :: state.functions })
;;

let from_aprogram prog =
  let lookup env x =
    match List.assoc_opt x env with
    | Some v -> return v
    | None when x = "print_int" -> return (Program.Var ".Lml_print")
    | None -> unsupported "unbound variable '%s'" x
  in
  let atom env = function
    | AInt n -> return (Program.Const n)
    | ABool b -> return (Program.Const (if b then 1 else 0))
    | AVar x -> lookup env x
  in
  let bound_value name = function
    | Program.Var label when String.starts_with ~prefix:".Lml_" label -> Program.Var label
    | Program.Const n -> Program.Const n
    | _ -> Program.Var name
  in
  let rec exp env = function
    | ACexp c -> cexp env c
    | ALet (flag, x, rhs, rest) ->
      let* value = binding env flag x rhs in
      let* rest = exp ((x, bound_value x value) :: env) rest in
      return (Program.Let (x, value, rest))
  and binding env flag x rhs =
    match flag, rhs with
    | Rec, ACexp (CFun (params, body)) -> lambda env (Some x) params body
    | Rec, _ -> unsupported "let rec requires a function on its right-hand side"
    | Nonrec, _ -> exp env rhs
  and cexp env = function
    | CAtom a -> atom env a
    | CNeg a ->
      let* a = atom env a in
      return (Program.BinOp (Program.Sub, Program.Const 0, a))
    | CBinop (op, a, b) ->
      let* a = atom env a in
      let* b = atom env b in
      return (Program.BinOp (conv_bop op, a, b))
    | CIf (a, t, f) ->
      let* a = atom env a in
      let* t = exp env t in
      let* f = exp env f in
      return (Program.If (a, t, f))
    | CApp (AVar "print_int", a) when not (List.mem_assoc "print_int" env) ->
      let* a = atom env a in
      return (Program.PrintInt a)
    | CApp (f, a) ->
      let* f = atom env f in
      let* a = atom env a in
      return (Program.App (Program.Value f, [ a ]))
    | CFun (params, body) -> lambda env None params body
  and lambda env self params body =
    match params with
    | [] -> unsupported "function without parameters"
    | x :: xs ->
      let body =
        match xs with
        | [] -> body
        | _ -> ACexp (CFun (xs, body))
      in
      let free = S.remove x (free_exp body) in
      let free =
        match self with
        | None -> free
        | Some name -> S.remove name free
      in
      let captures = S.elements free in
      let* values = M.map_list (lookup env) captures in
      let inner = List.mapi (fun i name -> name, Program.Env i) captures in
      let inner =
        match self with
        | None -> inner
        | Some name -> (name, Program.Self) :: inner
      in
      let* name = fresh_function in
      let* body = exp ((x, Program.Var x) :: inner) body in
      let* () = add_function { Program.name; params = [ x ]; body } in
      return
        (match captures with
         | [] -> Program.Var name
         | _ -> Program.MakeClosure (name, values))
  in
  let rec items env = function
    | [] -> return (Program.Const 0)
    | ADecl (flag, name, rhs) :: rest ->
      let* value = binding env flag name rhs in
      let* rest = items ((name, bound_value name value) :: env) rest in
      return (Program.Let (name, value, rest))
    | AExp e :: rest ->
      let* value = exp env e in
      let* rest = items env rest in
      return (Program.Let ("$discard", value, rest))
  in
  let computation =
    let* body = items [] prog in
    let* state = get in
    return
      (({ Program.name = "main"; params = []; body } :: List.rev state.functions)
       @ [ { Program.name = ".Lml_print"
           ; params = [ "x" ]
           ; body = Program.PrintInt (Program.Var "x")
           }
         ])
  in
  M.eval computation { next = 0; functions = [] }
;;

(* ---------------------------------------------------------------------- *)
(* Stage 2: Program.program -> Asm.t                                       *)
(* ---------------------------------------------------------------------- *)

type frame =
  { nlocals : int
  ; fnames : string list
  }

let rec uses_var x = function
  | Program.Var y -> x = y
  | Const _ | Env _ | Self -> false
  | BinOp (_, a, b) -> uses_var x a || uses_var x b
  | If (c, t, f) -> uses_var x c || uses_var x t || uses_var x f
  | Let (y, v, rest) -> uses_var x v || (x <> y && uses_var x rest)
  | App (callee, args) ->
    (match callee with
     | Direct _ -> false
     | Value c -> uses_var x c)
    || List.exists (uses_var x) args
  | MakeClosure (_, args) -> List.exists (uses_var x) args
  | PrintInt e -> uses_var x e
;;

(** Peak frame usage. The traversal is pure; mutually exclusive branches
    share slots and nested operands cannot overwrite earlier spills. *)
let rec frame_usage ~locals ~depth = function
  | Program.Const _ | Var _ | Env _ | Self -> locals, depth
  | BinOp (_, l, r) ->
    merge_usage (frame_usage ~locals ~depth l) (frame_usage ~locals ~depth:(depth + 1) r)
  | If (c, t, f) ->
    merge_usage
      (frame_usage ~locals ~depth c)
      (merge_usage (frame_usage ~locals ~depth t) (frame_usage ~locals ~depth f))
  | Let (x, v, rest) ->
    let extra = if uses_var x rest then 1 else 0 in
    merge_usage
      (frame_usage ~locals:(locals + extra) ~depth v)
      (frame_usage ~locals:(locals + extra) ~depth rest)
  | App (callee, args) ->
    let base =
      match callee with
      | Direct _ -> locals, depth + List.length args
      | Value c -> frame_usage ~locals ~depth:(depth + List.length args + 1) c
    in
    List.fold_left
      merge_usage
      base
      (List.mapi (fun i a -> frame_usage ~locals ~depth:(depth + i) a) args)
  | MakeClosure (_, args) ->
    List.fold_left
      merge_usage
      (locals, depth + List.length args)
      (List.mapi (fun i a -> frame_usage ~locals ~depth:(depth + i) a) args)
  | PrintInt e -> frame_usage ~locals ~depth e

and merge_usage (a, b) (c, d) = max a c, max b d

(** Fold over all subexpressions to collect static records and helpers. *)
let rec exists_body pred body =
  pred body
  ||
  match body with
  | Program.Const _ | Var _ | Env _ | Self -> false
  | BinOp (_, a, b) | Let (_, a, b) -> exists_body pred a || exists_body pred b
  | If (c, t, f) -> exists_body pred c || exists_body pred t || exists_body pred f
  | App (callee, args) ->
    (match callee with
     | Direct _ -> false
     | Value c -> exists_body pred c)
    || List.exists (exists_body pred) args
  | MakeClosure (_, args) -> List.exists (exists_body pred) args
  | PrintInt e -> exists_body pred e
;;

let atomic = function
  | Program.Const _ | Var _ | Env _ | Self -> true
  | _ -> false
;;

let binop_instructions = function
  | Program.Add -> [ A.Add (Reg.A0, Reg.T0, Reg.A0) ]
  | Sub -> [ A.Sub (Reg.A0, Reg.T0, Reg.A0) ]
  | Mul -> [ A.Mul (Reg.A0, Reg.T0, Reg.A0) ]
  | Div -> [ A.Beq (Reg.A0, Reg.Zero, ".Lml_div_zero"); A.Div (Reg.A0, Reg.T0, Reg.A0) ]
  | Mod -> [ A.Beq (Reg.A0, Reg.Zero, ".Lml_div_zero"); A.Rem (Reg.A0, Reg.T0, Reg.A0) ]
  | Lt -> [ A.Slt (Reg.A0, Reg.T0, Reg.A0) ]
  | Gt -> [ A.Slt (Reg.A0, Reg.A0, Reg.T0) ]
  | Le -> [ A.Slt (Reg.A0, Reg.A0, Reg.T0); A.Xori (Reg.A0, Reg.A0, 1) ]
  | Ge -> [ A.Slt (Reg.A0, Reg.T0, Reg.A0); A.Xori (Reg.A0, Reg.A0, 1) ]
  | Eq -> [ A.Sub (Reg.T1, Reg.T0, Reg.A0); A.Seqz (Reg.A0, Reg.T1) ]
  | Ne ->
    [ A.Sub (Reg.T1, Reg.T0, Reg.A0)
    ; A.Seqz (Reg.A0, Reg.T1)
    ; A.Xori (Reg.A0, Reg.A0, 1)
    ]
;;

(** Emission is a State_error computation (unit state). Invalid IL propagates
    Error through the same bind operator as lowering, without exceptions. *)
let rec compile_body frame ~path ~depth lenv body =
  let spill d = Callconv.spill_offset ~nlocals:frame.nlocals d in
  let slot = Callconv.local_offset in
  let sub suffix = compile_body frame ~path:(path ^ suffix) ~depth lenv in
  match body with
  | Program.Const n -> return [ A.Li (Reg.A0, n) ]
  | Self -> return [ A.Ld (Reg.A0, 8, Reg.Sp) ]
  | Env i when i < 0 -> unsupported "negative closure capture index"
  | Env i -> return [ A.Ld (Reg.T0, 8, Reg.Sp); A.Ld (Reg.A0, 8 * (i + 1), Reg.T0) ]
  | Var v ->
    (match List.assoc_opt v lenv with
     | Some i -> return [ A.Ld (Reg.A0, slot i, Reg.Sp) ]
     | None when List.mem v frame.fnames ->
       return [ A.La (Reg.A0, Callconv.static_closure_label v) ]
     | None -> unsupported "unbound variable '%s' at code generation" v)
  | MakeClosure (f, args) ->
    let* compiled = compile_arguments frame ~path ~depth lenv args in
    let stores =
      List.concat
        (List.mapi
           (fun i _ ->
              [ A.Ld (Reg.T1, spill (depth + i), Reg.Sp)
              ; A.Sd (Reg.T1, 8 * (i + 1), Reg.A0)
              ])
           args)
    in
    return
      (compiled
       @ [ A.Li (Reg.A0, Callconv.closure_size (List.length args))
         ; A.Call "malloc"
         ; A.La (Reg.T0, f)
         ; A.Sd (Reg.T0, 0, Reg.A0)
         ]
       @ stores)
  | BinOp (Sub, Const 0, r) ->
    let* code = sub ".neg" r in
    return (code @ [ A.Neg (Reg.A0, Reg.A0) ])
  | BinOp (Add, Const 0, r) -> sub ".zero" r
  | BinOp (Add, Const n, r) when n >= -2048 && n <= 2047 ->
    let* code = sub ".add" r in
    return (code @ [ A.Addi (Reg.A0, Reg.A0, n) ])
  | BinOp (((Add | Sub) as op), l, Const n)
    when (if op = Sub then -n else n) >= -2048 && (if op = Sub then -n else n) <= 2047 ->
    let* code = sub ".imm" l in
    return (code @ [ A.Addi (Reg.A0, Reg.A0, if op = Sub then -n else n) ])
  | BinOp (op, l, r) ->
    let* left = sub ".left" l in
    let* right = compile_body frame ~path:(path ^ ".right") ~depth:(depth + 1) lenv r in
    let operands =
      if atomic r
      then left @ [ A.Mov (Reg.T1, Reg.A0) ] @ right @ [ A.Mov (Reg.T0, Reg.T1) ]
      else
        left
        @ [ A.Sd (Reg.A0, spill depth, Reg.Sp) ]
        @ right
        @ [ A.Ld (Reg.T0, spill depth, Reg.Sp) ]
    in
    return (operands @ binop_instructions op)
  | If (c, t, f) ->
    let* cond = sub ".cond" c in
    let* yes = sub ".yes" t in
    let* no = sub ".no" f in
    return
      (cond
       @ [ A.Beq (Reg.A0, Reg.Zero, path ^ ".else") ]
       @ yes
       @ [ A.J (path ^ ".end"); A.Label (path ^ ".else") ]
       @ no
       @ [ A.Label (path ^ ".end") ])
  | Let (x, v, Var y) when x = y -> sub ".identity" v
  | Let (x, v, rest) when not (uses_var x rest) ->
    let* code = if atomic v then return [] else sub ".discard" v in
    let* rest = sub ".next" rest in
    return (code @ rest)
  | Let (x, v, rest) ->
    let i = List.length lenv in
    let* code = sub ".value" v in
    let* rest = compile_body frame ~path:(path ^ ".body") ~depth ((x, i) :: lenv) rest in
    return (code @ [ A.Sd (Reg.A0, slot i, Reg.Sp) ] @ rest)
  | App (Value (Var f), [ arg ])
    when List.mem f frame.fnames && not (List.mem_assoc f lenv) ->
    let* arg = sub ".argument" arg in
    return (arg @ [ A.La (Reg.S0, Callconv.static_closure_label f); A.Call f ])
  | App (Value c, [ arg ]) when atomic c && atomic arg ->
    let* callee = sub ".callee" c in
    let* arg = sub ".argument" arg in
    return
      (callee
       @ [ A.Mov (Reg.S0, Reg.A0) ]
       @ arg
       @ [ A.Ld (Reg.T0, 0, Reg.S0); A.Jalr Reg.T0 ])
  | App (_, args) when List.length args > Callconv.max_args ->
    unsupported "too many IL call arguments"
  | App (callee, args) ->
    let* compiled = compile_arguments frame ~path ~depth lenv args in
    let* loads =
      M.map_list
        (fun (i, _) ->
           let* reg = M.of_result (Callconv.arg_reg i) in
           return (A.Ld (reg, spill (depth + i), Reg.Sp)))
        (List.mapi (fun i arg -> i, arg) args)
    in
    (match callee with
     | Direct f -> return (compiled @ loads @ [ A.Call f ])
     | Value c ->
       let n = depth + List.length args in
       let* callee = compile_body frame ~path:(path ^ ".callee") ~depth:n lenv c in
       return
         (compiled
          @ callee
          @ [ A.Sd (Reg.A0, spill n, Reg.Sp) ]
          @ loads
          @ [ A.Ld (Reg.S0, spill n, Reg.Sp); A.Ld (Reg.T0, 0, Reg.S0); A.Jalr Reg.T0 ]))
  | PrintInt e ->
    let* code = sub ".print" e in
    return (code @ [ A.Call "print_int" ])

and compile_arguments frame ~path ~depth lenv args =
  let* parts =
    M.map_list
      (fun (i, arg) ->
         let* code =
           compile_body
             frame
             ~path:(path ^ ".arg" ^ string_of_int i)
             ~depth:(depth + i)
             lenv
             arg
         in
         return
           (code
            @ [ A.Sd
                  ( Reg.A0
                  , Callconv.spill_offset ~nlocals:frame.nlocals (depth + i)
                  , Reg.Sp )
              ]))
      (List.mapi (fun i arg -> i, arg) args)
  in
  return (List.concat parts)
;;

let print_int_helper : A.instr list =
  (* Keep the working value nonpositive: this also handles INT64_MIN. *)
  [ A.Label "print_int"
  ; A.Addi (Reg.Sp, Reg.Sp, -32)
  ; A.Mov (Reg.T0, Reg.A0)
  ; A.Slt (Reg.T5, Reg.T0, Reg.Zero)
  ; A.Addi (Reg.T2, Reg.Sp, 32)
  ; A.Blt (Reg.T0, Reg.Zero, ".Lprint_digits")
  ; A.Neg (Reg.T0, Reg.T0)
  ; A.Label ".Lprint_digits"
  ; A.Li (Reg.T3, 10)
  ; A.Rem (Reg.T4, Reg.T0, Reg.T3)
  ; A.Div (Reg.T0, Reg.T0, Reg.T3)
  ; A.Neg (Reg.T4, Reg.T4)
  ; A.Addi (Reg.T4, Reg.T4, 48)
  ; A.Addi (Reg.T2, Reg.T2, -1)
  ; A.Sb (Reg.T4, 0, Reg.T2)
  ; A.Bne (Reg.T0, Reg.Zero, ".Lprint_digits")
  ; A.Beq (Reg.T5, Reg.Zero, ".Lprint_write")
  ; A.Li (Reg.T4, 45)
  ; A.Addi (Reg.T2, Reg.T2, -1)
  ; A.Sb (Reg.T4, 0, Reg.T2)
  ; A.Label ".Lprint_write"
  ; A.Li (Reg.A0, 1)
  ; A.Mov (Reg.A1, Reg.T2)
  ; A.Addi (Reg.A2, Reg.Sp, 32)
  ; A.Sub (Reg.A2, Reg.A2, Reg.T2)
  ; A.Li (Reg.A7, 64)
  ; A.Ecall
  ; A.Li (Reg.A0, 0)
  ; A.Addi (Reg.Sp, Reg.Sp, 32)
  ; A.Ret
  ]
;;

let compile_function fnames (f : Program.function_) =
  if List.length f.params > Callconv.max_args
  then unsupported "function '%s' has too many IL parameters" f.name
  else (
    let nlocals, nspill = frame_usage ~locals:(List.length f.params) ~depth:0 f.body in
    let frame = { nlocals; fnames } in
    let size = Callconv.frame_size ~nlocals ~max_spill:nspill in
    let lenv = List.mapi (fun i p -> p, i) f.params in
    let* code = compile_body frame ~path:(".Lbody" ^ f.name) ~depth:0 lenv f.body in
    let* parameters =
      M.map_list
        (fun (i, _) ->
           let* reg = M.of_result (Callconv.arg_reg i) in
           return (A.Sd (reg, Callconv.local_offset i, Reg.Sp)))
        (List.mapi (fun i p -> i, p) f.params)
    in
    return
      ([ A.Label f.name
       ; A.Addi (Reg.Sp, Reg.Sp, -size)
       ; A.Sd (Reg.Ra, 0, Reg.Sp)
       ; A.Sd (Reg.S0, 8, Reg.Sp)
       ]
       @ parameters
       @ code
       @ (if f.name = "main" then [ A.Li (Reg.A0, 0) ] else [])
       @ [ A.Ld (Reg.Ra, 0, Reg.Sp)
         ; A.Ld (Reg.S0, 8, Reg.Sp)
         ; A.Addi (Reg.Sp, Reg.Sp, size)
         ; A.Ret
         ]))
;;

let compile_program (prog : Program.program) =
  let referenced name =
    List.exists
      (fun f ->
         exists_body
           (function
             | Program.Var v -> v = name
             | _ -> false)
           f.Program.body)
      prog
  in
  let prog =
    List.filter (fun f -> f.Program.name <> ".Lml_print" || referenced ".Lml_print") prog
  in
  let fnames = List.map (fun f -> f.Program.name) prog in
  let needs pred = List.exists (fun f -> exists_body pred f.Program.body) prog in
  let computation =
    let* parts = M.map_list (compile_function fnames) prog in
    let functions = List.concat parts in
    let printing =
      if
        needs (function
          | Program.PrintInt _ -> true
          | _ -> false)
      then print_int_helper
      else []
    in
    let errors =
      if
        needs (function
          | Program.BinOp ((Div | Mod), _, _) -> true
          | _ -> false)
      then [ A.Label ".Lml_div_zero"; A.Li (Reg.A0, 2); A.Li (Reg.A7, 93); A.Ecall ]
      else []
    in
    let data =
      List.filter_map
        (fun name ->
           if referenced name
           then Some (Callconv.static_closure_label name, name)
           else None)
        fnames
    in
    return { A.text = functions @ printing @ errors; data }
  in
  M.eval computation ()
;;
