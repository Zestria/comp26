[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Middleend.Anf
module A = Asm

(* Reaching these branches means the ANF invariant was violated (builtins and
   print_int never appear as first-class values); this is an internal error. *)
[@@@zanuda "-exc_failwith"]

type binding =
  | Slot of int
  | Print_int

(* The code generator threads a small mutable accumulator (label counter,
   current instruction buffer, slot counter, finished functions); this is the
   natural shape for emitting code and is confined to this module. *)
[@@@zanuda "-mutability_check"]

type gen =
  { mutable functions : A.function_ list
  ; mutable counter : int
  ; mutable code : A.instruction list
  ; mutable slots : int
  }
[@@zanuda "-mutability_check"]

let push g instruction = g.code <- instruction :: g.code
let offset slot = Int64.of_int (8 * slot)

let alloc g =
  let slot = g.slots in
  g.slots <- slot + 1;
  slot
;;

let fresh g prefix =
  g.counter <- g.counter + 1;
  Printf.sprintf "%s%d" prefix g.counter
;;

let ld g register slot = push g (A.Ld (register, offset slot, A.Sp))
let st g register slot = push g (A.Sd (register, offset slot, A.Sp))

let mv g destination source =
  if destination <> source then push g (A.Addi (destination, source, 0L))
;;

let save g =
  let slot = alloc g in
  st g A.A0 slot;
  slot
;;

let constant g = function
  | Ast.Integer value ->
    push g (A.Li (A.A0, Int64.of_int value));
    push g (A.Call "mmml_int")
  | Ast.Boolean value ->
    push g (A.Li (A.A0, if value then 1L else 0L));
    push g (A.Call "mmml_bool")
  | Ast.Unit -> push g (A.Li (A.A0, 10L))
;;

let imm g env self = function
  | Local id ->
    (match List.assoc_opt id.stamp env with
     | Some (Slot slot) -> ld g A.A0 slot
     | Some Print_int -> failwith "codegen: print_int used as a value"
     | None ->
       (match self with
        | Some (stamp, label, arity) when stamp = id.stamp ->
          push g (A.La (A.A0, label));
          push g (A.Li (A.A1, Int64.of_int arity));
          push g (A.Call "mmml_make_closure")
        | _ -> failwith ("codegen: unbound local " ^ id.hint)))
  | Constant value -> constant g value
  | External name -> push g (A.La (A.A0, name))
  | Builtin _ -> failwith "codegen: builtin used as a value"
;;

let truth g condition swap invert =
  let true_label = fresh g "cmp" in
  let end_label = fresh g "cmp_end" in
  let left, right = if swap then A.A1, A.A0 else A.A0, A.A1 in
  push g (A.Branch (condition, left, right, true_label));
  push g (A.Li (A.A0, if invert then 6L else 2L));
  push g (A.Jump end_label);
  push g (A.Label true_label);
  push g (A.Li (A.A0, if invert then 2L else 6L));
  push g (A.Label end_label)
;;

let arithmetic = function
  | Add -> Some A.Add
  | Subtract -> Some A.Sub
  | Multiply -> Some A.Mul
  | Divide -> Some A.Div
  | Equal | Not_equal | Less | Less_equal | Greater | Greater_equal -> None
;;

let comparison = function
  | Equal -> Some (A.Eq, false, false)
  | Not_equal -> Some (A.Ne, false, false)
  | Less -> Some (A.Lt, false, false)
  | Greater -> Some (A.Lt, true, false)
  | Less_equal -> Some (A.Lt, true, true)
  | Greater_equal -> Some (A.Lt, false, true)
  | Add | Subtract | Multiply | Divide -> None
;;

let unboxed g env self left right =
  imm g env self left;
  let left_value = save g in
  imm g env self right;
  let right_value = save g in
  ld g A.A0 left_value;
  push g (A.Call "mmml_unbox_int");
  let left_int = save g in
  ld g A.A0 right_value;
  push g (A.Call "mmml_unbox_int");
  push g (A.Addi (A.A1, A.A0, 0L));
  ld g A.A0 left_int
;;

let print_int g env self argument =
  imm g env self argument;
  push g (A.Call "mmml_print_int")
;;

let rec compound g env self = function
  | Value immediate -> imm g env self immediate
  | Apply (function_, argument) ->
    (match function_ with
     | Local id when List.assoc_opt id.stamp env = Some Print_int ->
       print_int g env self argument
     | External "print_int" -> print_int g env self argument
     | _ ->
       imm g env self function_;
       let function_value = save g in
       imm g env self argument;
       mv g A.A1 A.A0;
       ld g A.A0 function_value;
       push g (A.Call "mmml_apply"))
  | Unary_primitive (Positive, operand) -> imm g env self operand
  | Unary_primitive (Negate, operand) ->
    imm g env self operand;
    push g (A.Call "mmml_unbox_int");
    push g (A.Binop (A.Sub, A.A0, A.Zero, A.A0));
    push g (A.Call "mmml_int")
  | Binary_primitive (operator, left, right) ->
    unboxed g env self left right;
    (match arithmetic operator with
     | Some operation ->
       push g (A.Binop (operation, A.A0, A.A0, A.A1));
       push g (A.Call "mmml_int")
     | None ->
       (match comparison operator with
        | Some (condition, swap, invert) -> truth g condition swap invert
        | None -> assert false))
  | Lambda (parameter, body) ->
    let label, arity = build g parameter body None in
    push g (A.La (A.A0, label));
    push g (A.Li (A.A1, Int64.of_int arity));
    push g (A.Call "mmml_make_closure")
  | If (condition, yes, no) ->
    imm g env self condition;
    push g (A.Call "mmml_unbox_bool");
    let else_label = fresh g "else" in
    let end_label = fresh g "end" in
    push g (A.Branch (A.Eq, A.A0, A.Zero, else_label));
    term g env self ~tail:None yes;
    push g (A.Jump end_label);
    push g (A.Label else_label);
    term g env self ~tail:None no;
    push g (A.Label end_label)

and term g env self ~tail = function
  | Result value ->
    compound g env self value;
    (match tail with
     | Some label -> push g (A.Jump label)
     | None -> ())
  | Let (id, Value (External "print_int"), continuation) ->
    term g ((id.stamp, Print_int) :: env) self ~tail continuation
  | Let (id, value, continuation) ->
    compound g env self value;
    term g ((id.stamp, Slot (save g)) :: env) self ~tail continuation
  | Let_rec (id, parameter, body, continuation) ->
    let label, arity = build g parameter body (Some id) in
    push g (A.La (A.A0, label));
    push g (A.Li (A.A1, Int64.of_int arity));
    push g (A.Call "mmml_make_closure");
    term g ((id.stamp, Slot (save g)) :: env) self ~tail continuation

and parameters reversed = function
  | Result (Lambda (parameter, body)) -> parameters (parameter :: reversed) body
  | body -> List.rev reversed, body

and assemble g ~name ~exported =
  let return_address = g.slots in
  let frame = ((8 * (g.slots + 1)) + 15) land lnot 15 in
  { A.name
  ; exported
  ; body =
      [ A.Addi (A.Sp, A.Sp, Int64.of_int (-frame))
      ; A.Sd (A.Ra, offset return_address, A.Sp)
      ]
      @ List.rev g.code
      @ [ A.Ld (A.Ra, offset return_address, A.Sp)
        ; A.Addi (A.Sp, A.Sp, Int64.of_int frame)
        ; A.Ret
        ]
  }

and build g first_parameter body self =
  let parameters, body = parameters [ first_parameter ] body in
  let arity = List.length parameters in
  let label = fresh g "code" in
  let self = Option.map (fun id -> id.stamp, label, arity) self in
  let saved_code = g.code
  and saved_slots = g.slots in
  g.code <- [];
  g.slots <- 0;
  let env =
    List.mapi
      (fun index parameter ->
         let slot = alloc g in
         push g (A.Ld (A.T0, Int64.of_int (8 * index), A.A0));
         st g A.T0 slot;
         parameter.stamp, Slot slot)
      parameters
  in
  let exit = fresh g "exit" in
  term g env self ~tail:(Some exit) body;
  push g (A.Label exit);
  g.functions <- assemble g ~name:label ~exported:false :: g.functions;
  g.code <- saved_code;
  g.slots <- saved_slots;
  label, arity
;;

let compile program =
  let g = { functions = []; counter = 0; code = []; slots = 0 } in
  let definition env = function
    | Definition (id, value) ->
      term g env None ~tail:None value;
      (id.stamp, Slot (save g)) :: env
    | Recursive_definition (id, parameter, body) ->
      let label, arity = build g parameter body (Some id) in
      push g (A.La (A.A0, label));
      push g (A.Li (A.A1, Int64.of_int arity));
      push g (A.Call "mmml_make_closure");
      (id.stamp, Slot (save g)) :: env
  in
  let (_ : (int * binding) list) = List.fold_left definition [] program in
  push g (A.Li (A.A0, 10L));
  let exit = fresh g "main_exit" in
  push g (A.Jump exit);
  push g (A.Label exit);
  assemble g ~name:"mmml_program_main" ~exported:true :: List.rev g.functions
;;
