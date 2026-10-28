open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Names = Map.Make (String)

type t = { bytes : int; slots : int Names.t; call_words : int }
let argument_offset index = index * Abi.word_size

let rec collect_expr (names, max_arguments) = function
  | ACExpr expression -> collect_computation (names, max_arguments) expression
  | ALet (pattern, expression, body) ->
    let names =
      match pattern with PVar name -> Names.add name () names | PUnit | PUnderscore -> names
    in
    collect_expr (collect_computation (names, max_arguments) expression) body

and collect_computation (names, max_arguments) = function
  | CApp (_, arguments) -> names, max max_arguments (List.length arguments)
  | CIf (_, if_true, if_false) ->
    collect_expr (collect_expr (names, max_arguments) if_true) if_false
  | CImm _ | CBinOp _ | CUnOp _ -> names, max_arguments
;;

let create parameters bodies =
  let names =
    List.fold_left
      (fun names -> function PVar name -> Names.add name () names | PUnit | PUnderscore -> names)
      Names.empty parameters
  in
  let names, call_words = List.fold_left collect_expr (names, 0) bodies in
  let first_slot = (call_words + 2) * Abi.word_size in
  let slots, end_slot =
    Names.fold
      (fun name () (slots, offset) -> Names.add name offset slots, offset + Abi.word_size)
      names (Names.empty, first_slot)
  in
  let bytes = end_slot + Abi.word_size in
  let bytes = ((bytes + Abi.stack_alignment - 1) / Abi.stack_alignment) * Abi.stack_alignment in
  { bytes; slots; call_words }
;;

let size frame = frame.bytes
let callee_slot frame = frame.call_words * Abi.word_size
let operand_slot frame = (frame.call_words + 1) * Abi.word_size
let slot frame name =
  match Names.find_opt name frame.slots with
  | Some offset -> offset
  | None -> invalid_arg ("Frame.slot: unallocated name " ^ name)
;;

let prologue output frame parameters =
  adjust_stack output (-frame.bytes);
  store output Ra ~base:Sp ~offset:(frame.bytes - Abi.word_size);
  List.iteri
    (fun index -> function
      | PUnit | PUnderscore -> ()
      | PVar name ->
        let destination = slot frame name in
        if index < Abi.register_argument_count
        then store output (List.nth argument_registers index) ~base:Sp ~offset:destination
        else (
          load output T0 ~base:Sp
            ~offset:(frame.bytes + argument_offset (index - Abi.register_argument_count));
          store output T0 ~base:Sp ~offset:destination))
    parameters
;;

let epilogue output frame =
  load output Ra ~base:Sp ~offset:(frame.bytes - Abi.word_size);
  adjust_stack output frame.bytes;
  instruction output Ret
;;
