[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V 64 code generator (Task 3).

    Two stages, both in this module:

    - {!from_aprogram} lowers a lambda-lifted ANF program (the Task 2
      output) into the small {!Program} IL.  It resolves curried
      application chains ([t1 = f a; t2 = t1 b; … t2]) into multi-argument
      calls, unfolds the self-application fixpoints that closure
      conversion introduces for recursive functions
      ([let rec f = (fun f x -> …) f]), hoists the nested recursive
      lambdas, inlines constant globals and maps [print_int] applications
      onto {!Program.PrintInt}.  Application chains that stop before the
      callee's full arity become {!Program.Closure} records.

    - {!compile_program} emits {!Asm} for a {!Program.program}.  Functions
      take their parameters in [a0]... and return in [a0]; arithmetic
      never leaves the registers — there are no runtime helpers for
      [+ - * / mod < <= …].  First-class function values are closure
      records [code; v1 … vk]: an indirect call sets [s0] to the record,
      loads the remaining arguments into [a0]... and jumps through
      [t0]; a per-[(code, k)] trampoline reloads the [k] captured
      arguments.  Zero-capture references ([Var f] for a known function
      [f]) use static [.rodata] records, so the common cases allocate
      nothing.

    The only library function ever called is [malloc] (for closure
    records); the only emitted helper is [print_int], a dozen instructions
    that format the argument in decimal and issue the [write] syscall. *)

open Mylittleml.Middleend.Anf
module Reg = Program.Reg
module A = Asm

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

(* ---------------------------------------------------------------------- *)
(* Stage 1: lambda-lifted ANF -> Program.program                           *)
(* ---------------------------------------------------------------------- *)

(** How a known-arity callee is reached: a static code label, or a variable
    holding a closure value (e.g. the self-parameter of an unfolded
    recursive function, or a [let rec] binding inside [main]). *)
type callee_desc =
  | DLabel of string
  | DValue of string

(** Lowering environment threaded through the conversion.  It merges
    application-chain state ([BPartial]: a temporary holding a partially
    applied known function) and alias bindings ([BAlias]: [let cc = f in …]
    produced by closure conversion). *)
type binding =
  | BPartial of callee_desc * Program.body list
  | BAlias of callee_desc

type lctx =
  { arity : (string, int) Hashtbl.t
    (** callable name -> total arity (labels, [let rec] values, hoisted
            nested lambdas) *)
  ; labels : (string, unit) Hashtbl.t
    (** names that are static code labels (zero captured arguments) *)
  ; params : (string, string list) Hashtbl.t
    (** label -> parameter list (used to unfold [let rec] fixpoints) *)
  ; value_of : (string, string) Hashtbl.t
    (** closure-valued name -> underlying code label *)
  ; eta : (string, string) Hashtbl.t
    (** [let rec] name -> the code label of its fixpoint unfolding *)
  ; consts : (string, int) Hashtbl.t
    (** compile-time int/bool globals, inlined at every use *)
  ; mutable funcs_out : Program.function_ list
  ; mutable fresh : int
  }

let is_callable lctx name = Hashtbl.mem lctx.arity name
let is_label lctx name = Hashtbl.mem lctx.labels name
let desc_of lctx name = if is_label lctx name then DLabel name else DValue name

let fresh_label_name lctx base =
  lctx.fresh <- lctx.fresh + 1;
  base ^ string_of_int lctx.fresh
;;

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

let atom_name = function
  | AVar v -> v
  | _ -> "<constant>"
;;

(** Complete or reify an application of a known-arity callee.  A full
    application becomes a call; a partial one becomes a closure record
    value (the argument list is already applied). *)
let rec finish_call lctx (desc : callee_desc) (args : Program.body list) : Program.body =
  let arity =
    match desc with
    | DLabel f | DValue f -> Hashtbl.find lctx.arity f
  in
  if List.length args = arity
  then (
    match desc with
    | DLabel f -> Program.App (Program.Direct f, args)
    | DValue v -> Program.App (Program.Value (Program.Var v), args))
  else (
    match desc with
    | DLabel f -> Program.Closure (f, arity, args)
    | DValue v ->
      (match Hashtbl.find_opt lctx.value_of v with
       | Some lab -> Program.Closure (lab, arity, args)
       | None ->
         unsupported
           "partial application of the unknown function value '%s' is not supported"
           v))
;;

(** Register a nested lambda as a plain zero-capture top-level function. *)
let rec register_function lctx name (params : string list) (body : aexp) : unit =
  Hashtbl.replace lctx.arity name (List.length params);
  Hashtbl.replace lctx.labels name ();
  Hashtbl.replace lctx.params name params;
  let body' = conv_aexp lctx [] body in
  lctx.funcs_out <- { Program.name; params; body = body' } :: lctx.funcs_out

and conv_atom lctx env (a : atom) : Program.body =
  match a with
  | AInt n -> Program.Const n
  | ABool b -> Program.Const (if b then 1 else 0)
  | AVar v ->
    (match Hashtbl.find_opt lctx.consts v with
     | Some n -> Program.Const n
     | None ->
       (match List.assoc_opt v env with
        | Some (BPartial (desc, args)) -> finish_call lctx desc args
        | Some (BAlias desc) ->
          (match desc with
           | DLabel f | DValue f -> Program.Var f)
        | None -> Program.Var v))

and conv_cexp lctx env (c : cexp) : Program.body =
  match c with
  | CAtom a -> conv_atom lctx env a
  | CBinop (op, a, b) ->
    Program.BinOp (conv_bop op, conv_atom lctx env a, conv_atom lctx env b)
  | CNeg a -> Program.BinOp (Program.Sub, Program.Const 0, conv_atom lctx env a)
  | CApp (AVar "print_int", x) when not (is_callable lctx "print_int") ->
    Program.PrintInt (conv_atom lctx env x)
  | CApp (AVar f, x) when is_callable lctx f ->
    finish_call lctx (desc_of lctx f) [ conv_atom lctx env x ]
  | CApp (AVar g, x) ->
    (match List.assoc_opt g env with
     | Some (BPartial (desc, args)) ->
       finish_call lctx desc (args @ [ conv_atom lctx env x ])
     | Some (BAlias desc) -> finish_call lctx desc [ conv_atom lctx env x ]
     | None ->
       (* unknown arity (e.g. a continuation parameter): assume a full call *)
       Program.App (Program.Value (Program.Var g), [ conv_atom lctx env x ]))
  | CApp (f, _) -> unsupported "call of a non-function value '%s'" (atom_name f)
  | CIf (c, t, f) ->
    Program.If (conv_atom lctx env c, conv_aexp lctx env t, conv_aexp lctx env f)
  | CFun _ -> unsupported "internal error: lambda survived lambda lifting"

and conv_aexp lctx env (e : aexp) : Program.body =
  match e with
  | ACexp c -> conv_cexp lctx env c
  | ALet (_, name, ACexp (CFun (params, body)), rest) ->
    (* Lambda lifting keeps nested recursive lambdas in place; hoist them
       as plain functions (closure conversion guarantees they are closed). *)
    register_function lctx name params body;
    conv_aexp lctx env rest
  | ALet (Rec, name, rhs, rest) ->
    (match resolve_eta lctx name rhs with
     | Some target ->
       let params = Hashtbl.find lctx.params target in
       Hashtbl.replace lctx.eta name target;
       Hashtbl.replace lctx.arity name (List.length params - 1);
       Hashtbl.replace lctx.value_of name target;
       Program.Let
         (name, Program.ClosRec (target, List.length params), conv_aexp lctx env rest)
     | None ->
       unsupported
         "recursive binding of a non-function value is not supported by the backend")
  | ALet (_, name, ACexp (CApp (AVar "print_int", x)), rest)
    when not (is_callable lctx "print_int") ->
    Program.Let (name, Program.PrintInt (conv_atom lctx env x), conv_aexp lctx env rest)
  | ALet (_, name, ACexp (CApp (AVar f, arg)), rest) ->
    let arg' = conv_atom lctx env arg in
    (match List.assoc_opt f env with
     | Some (BPartial (desc, args)) ->
       conv_aexp lctx ((name, BPartial (desc, args @ [ arg' ])) :: env) rest
     | Some (BAlias desc) ->
       conv_aexp lctx ((name, BPartial (desc, [ arg' ])) :: env) rest
     | None ->
       if is_callable lctx f
       then conv_aexp lctx ((name, BPartial (desc_of lctx f, [ arg' ])) :: env) rest
       else
         (* unknown arity (e.g. a continuation parameter): full call *)
         Program.Let
           ( name
           , Program.App (Program.Value (Program.Var f), [ arg' ])
           , conv_aexp lctx env rest ))
  | ALet (_, name, ACexp (CAtom (AVar v)), rest) when is_callable lctx v ->
    (* local alias of a known function: substitute instead of copying *)
    conv_aexp lctx ((name, BAlias (desc_of lctx v)) :: env) rest
  | ALet (_, name, rhs, rest) ->
    let rhs' = conv_aexp lctx env rhs in
    (match rhs' with
     | Program.Closure (lab, _, _) | Program.ClosRec (lab, _) ->
       Hashtbl.replace lctx.value_of name lab
     | _ -> ());
    Program.Let (name, rhs', conv_aexp lctx env rest)

(** Recognize the [let rec] fixpoint shape produced by closure conversion:

    {[let rec f = let cc = (fun f x y -> body) in cc f]}

    (with any number of alias bindings in between; the alias may also hold
    the lambda itself).  Returns the code label of the unfolded function
    whose first parameter is [f]. *)
and resolve_eta lctx (name : string) (rhs : aexp) : string option =
  let rec go e subst =
    match e with
    | ALet (Nonrec, alias, ACexp (CAtom (AVar target)), rest) ->
      go rest ((alias, target) :: subst)
    | ALet (Nonrec, alias, ACexp (CFun (params, body)), rest) ->
      let lab = fresh_label_name lctx "eta_f" in
      register_function lctx lab params body;
      go rest ((alias, lab) :: subst)
    | ACexp (CApp (AVar f, AVar self)) when self = name ->
      let lab =
        match List.assoc_opt f subst with
        | Some l -> l
        | None -> f
      in
      (match Hashtbl.find_opt lctx.params lab with
       | Some params when params <> [] && List.hd params = name -> Some lab
       | _ -> None)
    | _ -> None
  in
  go rhs []
;;

let from_aprogram (prog : aprogram) : Program.program =
  let lctx =
    { arity = Hashtbl.create 16
    ; labels = Hashtbl.create 16
    ; params = Hashtbl.create 16
    ; value_of = Hashtbl.create 16
    ; eta = Hashtbl.create 16
    ; consts = Hashtbl.create 16
    ; funcs_out = []
    ; fresh = 0
    }
  in
  (* Pass 1: register every top-level function so that definitions can be
     unfolded and converted in any order. *)
  List.iter
    (function
      | ADecl (_, name, ACexp (CFun (params, _))) ->
        Hashtbl.replace lctx.arity name (List.length params);
        Hashtbl.replace lctx.labels name ();
        Hashtbl.replace lctx.params name params
      | _ -> ())
    prog;
  (* Pass 2: resolve the fixpoint shape of top-level [let rec] bindings. *)
  List.iter
    (function
      | ADecl (Rec, name, rhs) ->
        (match rhs with
         | ACexp (CFun _) -> () (* plain recursive lambda: a normal label *)
         | _ ->
           (match resolve_eta lctx name rhs with
            | Some target ->
              let params = Hashtbl.find lctx.params target in
              Hashtbl.replace lctx.eta name target;
              Hashtbl.replace lctx.arity name (List.length params - 1);
              Hashtbl.replace lctx.value_of name target
            | None -> ()))
      | _ -> ())
    prog;
  (* Pass 3: inline every int/bool global so that uses never depend on the
     order in which declarations are converted. *)
  List.iter
    (function
      | ADecl (_, name, ACexp (CAtom (AInt n))) -> Hashtbl.replace lctx.consts name n
      | ADecl (_, name, ACexp (CAtom (ABool b))) ->
        Hashtbl.replace lctx.consts name (if b then 1 else 0)
      | _ -> ())
    prog;
  (* Pass 4: functions become definitions; everything else builds the body
     of [main] in program order.  The fold runs over the reversed item
     list so that each new binding wraps the continuation converted so
     far — uses always sit inside the scope of their definitions. *)
  let conv_item (main : Program.body option) (item : item) : Program.body option =
    match item with
    | ADecl (_, name, ACexp (CFun (params, body))) ->
      let body' = conv_aexp lctx [] body in
      lctx.funcs_out <- { Program.name; params; body = body' } :: lctx.funcs_out;
      main
    | ADecl (Rec, name, _) when Hashtbl.mem lctx.eta name ->
      let target = Hashtbl.find lctx.eta name in
      let knot = Program.ClosRec (target, Hashtbl.find lctx.arity name + 1) in
      Some (Program.Let (name, knot, Option.value main ~default:(Program.Const 0)))
    | ADecl (_, _, ACexp (CAtom (AInt _))) | ADecl (_, _, ACexp (CAtom (ABool _))) -> main
    | ADecl (_, name, rhs) ->
      (* A non-constant, non-function global: compute it at the start of
         main and keep it in a stack slot for the rest of main. *)
      Some
        (Program.Let
           (name, conv_aexp lctx [] rhs, Option.value main ~default:(Program.Const 0)))
    | AExp e ->
      Some
        (match main with
         | None -> conv_aexp lctx [] e
         | Some m -> Program.Let ("main_expr", conv_aexp lctx [] e, m))
  in
  let main_body =
    match List.fold_left conv_item None (List.rev prog) with
    | Some b -> b
    | None -> Program.Const 0
  in
  { Program.name = "main"; params = []; body = main_body } :: lctx.funcs_out
;;

(* ---------------------------------------------------------------------- *)
(* Stage 2: Program.program -> Asm.t                                       *)
(* ---------------------------------------------------------------------- *)

(** Per-program emission state. *)
type cstate =
  { mutable next_slot : int
    (** next free local slot (scope-aware: released when a [Let] scope
          ends) *)
  ; mutable nlocals : int (** peak number of live local slots *)
  ; mutable nspill : int (** peak number of spill slots *)
  ; mutable label : int (** fresh branch-label counter *)
  ; fnames : string list
    (** all function labels of the program: a [Var] outside [penv]/[lenv]
        that names one of these is a first-class function reference *)
  ; mutable tramps : (string * int * int) list
    (** trampolines (label, arity, captured), in first-use order *)
  ; mutable static_clos : string list
    (** functions referenced as values, needing a [.rodata] record *)
  ; uses_print_int : bool ref
  }

let make_cstate fnames =
  { next_slot = 0
  ; nlocals = 0
  ; nspill = 0
  ; label = 0
  ; fnames
  ; tramps = []
  ; static_clos = []
  ; uses_print_int = ref false
  }
;;

let fresh_label st kind =
  let n = st.label in
  st.label <- n + 1;
  Format.sprintf ".L%s%d" kind n
;;

let need_trampoline st f arity k =
  if not (List.exists (fun (g, a, c) -> g = f && a = arity && c = k) st.tramps)
  then st.tramps <- (f, arity, k) :: st.tramps
;;

let need_static_closure st f =
  if not (List.mem f st.static_clos) then st.static_clos <- f :: st.static_clos
;;

(** Conservative static analysis used for the frame layout: peak number of
    live local slots and peak spill depth.  Must mirror [compile_body]. *)
let rec analyze_body st ~depth (e : Program.body) : unit =
  let bump_spill d = if d > st.nspill then st.nspill <- d in
  match e with
  | Const _ | Var _ | ClosRec _ -> ()
  | BinOp (_, l, r) ->
    analyze_body st ~depth l;
    analyze_body st ~depth r;
    bump_spill (depth + 1)
  | If (_, t, f) ->
    analyze_body st ~depth t;
    analyze_body st ~depth f
  | Let (_, v, rest) ->
    let i = st.next_slot in
    st.next_slot <- i + 1;
    if st.next_slot > st.nlocals then st.nlocals <- st.next_slot;
    analyze_body st ~depth v;
    analyze_body st ~depth rest;
    st.next_slot <- i
  | App (callee, args) ->
    (* argument i is compiled at depth + i so that spills of nested calls
       never clobber the slots of the already-stashed arguments *)
    List.iteri (fun i a -> analyze_body st ~depth:(depth + i) a) args;
    bump_spill (depth + List.length args);
    (match callee with
     | Program.Value c ->
       analyze_body st ~depth:(depth + List.length args) c;
       bump_spill (depth + List.length args + 1)
     | Program.Direct _ -> ())
  | Closure (_, _, cargs) ->
    List.iteri (fun i a -> analyze_body st ~depth:(depth + i) a) cargs;
    bump_spill (depth + List.length cargs + 1)
  | PrintInt e -> analyze_body st ~depth e
;;

(** Compile one body so that the result ends up in [a0].  [penv] maps
    parameters to their argument registers, [lenv] maps in-scope [let]-
    bound names to stack slots.  [depth] is the current spill depth. *)
let rec compile_body st ~depth lenv (e : Program.body) : A.instr list =
  let spill d = Callconv.spill_offset ~nlocals:st.nlocals d in
  let slot i = Callconv.local_offset i in
  match e with
  | Const n -> [ A.Li (Reg.A0, n) ]
  | Var v ->
    (match List.assoc_opt v lenv with
     | Some i -> [ A.Ld (Reg.A0, slot i, Reg.Sp) ]
     | None ->
       if List.mem v st.fnames
       then (
         need_static_closure st v;
         [ A.La (Reg.A0, Callconv.static_closure_label v) ])
       else
         unsupported
           "unbound variable '%s' at code generation (locals: [%s])"
           v
           (String.concat ", " (List.map fst lenv)))
  | BinOp (op, l, r) ->
    (* special cases first: no spill, no extra instruction *)
    let const_of = function
      | Program.Const n -> Some n
      | _ -> None
    in
    (match l, op, const_of r, const_of l with
     | Program.Const 0, Program.Sub, _, _ ->
       compile_body st ~depth lenv r @ [ A.Neg (Reg.A0, Reg.A0) ]
     | Program.Const 0, Program.Add, _, _ -> compile_body st ~depth lenv r
     | _, Program.Add, _, Some c ->
       compile_body st ~depth lenv r @ [ A.Addi (Reg.A0, Reg.A0, c) ]
     | _, (Program.Add | Program.Sub), Some c, _ ->
       compile_body st ~depth lenv l
       @ [ A.Ld (Reg.T0, spill depth, Reg.Sp)
         ; A.Addi (Reg.A0, Reg.T0, if op = Program.Sub then -c else c)
         ]
     | _ ->
       compile_body st ~depth lenv l
       @ [ A.Sd (Reg.A0, spill depth, Reg.Sp) ]
       @ compile_body st ~depth lenv r
       @ [ A.Ld (Reg.T0, spill depth, Reg.Sp) ]
       @
         (match op with
         | Program.Add -> [ A.Add (Reg.A0, Reg.T0, Reg.A0) ]
         | Program.Sub -> [ A.Sub (Reg.A0, Reg.T0, Reg.A0) ]
         | Program.Mul -> [ A.Mul (Reg.A0, Reg.T0, Reg.A0) ]
         | Program.Div -> [ A.Div (Reg.A0, Reg.T0, Reg.A0) ]
         | Program.Mod -> [ A.Rem (Reg.A0, Reg.T0, Reg.A0) ]
         | Program.Lt -> [ A.Slt (Reg.A0, Reg.T0, Reg.A0) ]
         | Program.Gt -> [ A.Slt (Reg.A0, Reg.A0, Reg.T0) ]
         | Program.Le -> [ A.Slt (Reg.A0, Reg.A0, Reg.T0); A.Xori (Reg.A0, Reg.A0, 1) ]
         | Program.Ge -> [ A.Slt (Reg.A0, Reg.T0, Reg.A0); A.Xori (Reg.A0, Reg.A0, 1) ]
         | Program.Eq -> [ A.Sub (Reg.T1, Reg.T0, Reg.A0); A.Seqz (Reg.A0, Reg.T1) ]
         | Program.Ne ->
           [ A.Sub (Reg.T1, Reg.T0, Reg.A0)
           ; A.Seqz (Reg.A0, Reg.T1)
           ; A.Xori (Reg.A0, Reg.A0, 1)
           ]))
  | If (c, t, f) ->
    let l_else = fresh_label st "else" in
    let l_end = fresh_label st "endif" in
    compile_body st ~depth lenv c
    @ [ A.Beq (Reg.A0, Reg.Zero, l_else) ]
    @ compile_body st ~depth lenv t
    @ [ A.J l_end; A.Label l_else ]
    @ compile_body st ~depth lenv f
    @ [ A.Label l_end ]
  | Let (x, v, rest) ->
    let i = st.next_slot in
    st.next_slot <- i + 1;
    if st.next_slot > st.nlocals then st.nlocals <- st.next_slot;
    let code_v = compile_body st ~depth lenv v in
    let code_rest = compile_body st ~depth ((x, i) :: lenv) rest in
    st.next_slot <- i;
    code_v @ [ A.Sd (Reg.A0, slot i, Reg.Sp) ] @ code_rest
  | App (callee, args) ->
    if List.length args > Callconv.max_args
    then
      unsupported
        "call of '%s' has %d arguments, the backend supports at most %d"
        (match callee with
         | Program.Direct f -> f
         | Program.Value _ -> "<indirect>")
        (List.length args)
        Callconv.max_args;
    let nargs = List.length args in
    let compiled =
      List.mapi
        (fun i arg ->
           compile_body st ~depth lenv arg @ [ A.Sd (Reg.A0, spill (depth + i), Reg.Sp) ])
        args
    in
    let loads =
      List.mapi (fun i _ -> A.Ld (Callconv.arg_reg i, spill (depth + i), Reg.Sp)) args
    in
    (match callee with
     | Program.Direct f -> List.concat compiled @ loads @ [ A.Jal f ]
     | Program.Value c ->
       (* indirect call: stash the closure pointer beyond the arguments,
          then jump through [t0] with the record in [s0] *)
       compile_body st ~depth lenv c
       @ [ A.Sd (Reg.A0, spill (depth + nargs), Reg.Sp) ]
       @ List.concat compiled
       @ loads
       @ [ A.Ld (Reg.S0, spill (depth + nargs), Reg.Sp)
         ; A.Ld (Reg.T0, 0, Reg.S0)
         ; A.Jalr Reg.T0
         ])
  | Closure (f, arity, cargs) ->
    let k = List.length cargs in
    need_trampoline st f arity k;
    let compiled =
      List.mapi
        (fun i arg ->
           compile_body st ~depth lenv arg @ [ A.Sd (Reg.A0, spill (depth + i), Reg.Sp) ])
        cargs
    in
    let stores =
      List.mapi
        (fun i _ ->
           [ A.Ld (Reg.T1, spill (depth + i), Reg.Sp)
           ; A.Sd (Reg.T1, 8 + (8 * i), Reg.A0)
           ])
        cargs
    in
    List.concat compiled
    @ [ A.Li (Reg.A0, Callconv.closure_size k)
      ; A.Call "malloc"
      ; A.Sd (Reg.A0, spill (depth + k), Reg.Sp)
      ; A.La (Reg.T0, Callconv.trampoline_label f k)
      ; A.Sd (Reg.T0, 0, Reg.A0)
      ]
    @ List.concat stores
    @ [ A.Ld (Reg.A0, spill (depth + k), Reg.Sp) ]
  | ClosRec (f, arity) ->
    (* closure record whose single captured argument is itself *)
    need_trampoline st f arity 1;
    [ A.Li (Reg.A0, Callconv.closure_size 1)
    ; A.Call "malloc"
    ; A.La (Reg.T0, Callconv.trampoline_label f 1)
    ; A.Sd (Reg.T0, 0, Reg.A0)
    ; A.Sd (Reg.A0, 8, Reg.A0)
    ]
  | PrintInt e ->
    st.uses_print_int := true;
    compile_body st ~depth lenv e @ [ A.Jal "print_int"; A.Li (Reg.A0, 0) ]
;;

(* Trampoline for [(f, arity, k)]: entered with the closure record in [s0]
   and the remaining [arity - k] arguments in [a0]...; reloads the [k]
   captured arguments from the record, shifts everything into [a0]... and
   tail-jumps to [f]. *)
let trampoline (f, arity, k) : A.instr list =
  let m = arity - k in
  let t i = List.nth [ Reg.T0; Reg.T1; Reg.T2; Reg.T3; Reg.T4; Reg.T5 ] i in
  let a i = Callconv.arg_reg i in
  [ A.Label (Callconv.trampoline_label f k) ]
  @ List.init m (fun i -> A.Mov (t i, a i))
  @ List.init k (fun j -> A.Ld (a j, 8 + (8 * j), Reg.S0))
  @ List.init m (fun i -> A.Mov (a (k + i), t i))
  @ [ A.J f ]
;;

(* [print_int] helper: formats [a0] as decimal (with a minus sign when
   needed) into a buffer on its own stack frame and writes it to stdout via
   the [write] syscall.  Frame layout: [0..23(sp)] digit buffer, [32(sp)]
   saved [ra]. *)
let print_int_helper : A.instr list =
  let loop = ".Lprint_int_loop" in
  [ A.Label "print_int"
  ; A.Addi (Reg.Sp, Reg.Sp, -40)
  ; A.Sd (Reg.Ra, 32, Reg.Sp)
  ; A.Mov (Reg.T0, Reg.A0)
  ; A.Addi (Reg.T2, Reg.Sp, 24)
    (* negative numbers: emit '-', work with the absolute value *)
  ; A.Bge (Reg.T0, Reg.Zero, loop)
  ; A.Li (Reg.T3, 45) (* '-' *)
  ; A.Addi (Reg.T2, Reg.T2, -1)
  ; A.Sb (Reg.T3, 0, Reg.T2)
  ; A.Neg (Reg.T0, Reg.T0)
  ; A.Label loop
  ; A.Li (Reg.T3, 10)
  ; A.Rem (Reg.T4, Reg.T0, Reg.T3)
  ; A.Div (Reg.T0, Reg.T0, Reg.T3)
  ; A.Addi (Reg.T2, Reg.T2, -1)
  ; A.Addi (Reg.T4, Reg.T4, 48) (* '0' *)
  ; A.Sb (Reg.T4, 0, Reg.T2)
  ; A.Bne (Reg.T0, Reg.Zero, loop) (* write(1, buf, sp+24 - buf) *)
  ; A.Li (Reg.A0, 1)
  ; A.Mov (Reg.A1, Reg.T2)
  ; A.Addi (Reg.A2, Reg.Sp, 24)
  ; A.Sub (Reg.A2, Reg.A2, Reg.T2)
  ; A.Li (Reg.A7, 64) (* SYS_write *)
  ; A.Ecall
  ; A.Ld (Reg.Ra, 32, Reg.Sp)
  ; A.Addi (Reg.Sp, Reg.Sp, 40)
  ; A.Ret
  ]
;;

let compile_function (st : cstate) (f : Program.function_) : A.instr list =
  if List.length f.params > Callconv.max_args
  then
    unsupported
      "function '%s' has %d parameters, the backend supports at most %d"
      f.name
      (List.length f.params)
      Callconv.max_args;
  (* Frame layout must be known before emission (offsets are baked into
     [ld]/[sd]), hence the analysis pass first.  The per-function counters
     are reset in place: [tramps]/[static_clos]/[uses_print_int] must keep
     accumulating across the whole program. *)
  st.nspill <- 0;
  st.label <- 0;
  (* parameters occupy the first slots and are spilled in the prologue *)
  st.next_slot <- List.length f.params;
  st.nlocals <- List.length f.params;
  analyze_body st ~depth:0 f.body;
  let frame = Callconv.frame_size ~nlocals:st.nlocals ~max_spill:st.nspill in
  let lenv = List.mapi (fun i p -> p, i) f.params in
  let code = compile_body st ~depth:0 lenv f.body in
  let prologue =
    [ A.Label f.name
    ; A.Addi (Reg.Sp, Reg.Sp, -frame)
    ; A.Sd (Reg.Ra, 0, Reg.Sp)
    ; A.Sd (Reg.S0, 8, Reg.Sp)
    ]
    @ List.mapi
        (fun i _ -> A.Sd (Callconv.arg_reg i, Callconv.local_offset i, Reg.Sp))
        f.params
  in
  let epilogue =
    [ A.Ld (Reg.Ra, 0, Reg.Sp)
    ; A.Ld (Reg.S0, 8, Reg.Sp)
    ; A.Addi (Reg.Sp, Reg.Sp, frame)
    ; A.Ret
    ]
  in
  let body =
    if f.name = "main"
    then (
      (* force exit code 0 unless the body already ends with exactly that *)
      match List.rev code with
      | A.Li (Reg.A0, 0) :: _ -> code
      | _ -> code @ [ A.Li (Reg.A0, 0) ])
    else code
  in
  prologue @ body @ epilogue
;;

let compile_program (prog : Program.program) : Asm.t =
  let fnames = List.map (fun f -> f.Program.name) prog in
  let st = make_cstate fnames in
  (* NB: the three parts are computed as separate bindings: the functions
     must be compiled *before* the trampoline/helper lists are read, since
     compiling them is what fills [st.tramps]/[st.uses_print_int]. *)
  let part1 = List.concat_map (compile_function st) prog in
  let part2 = List.concat_map trampoline (List.rev st.tramps) in
  let part3 = if !(st.uses_print_int) then print_int_helper else [] in
  let text = part1 @ part2 @ part3 in
  let data = List.rev_map (fun f -> Callconv.static_closure_label f, f) st.static_clos in
  { A.text; data }
;;
