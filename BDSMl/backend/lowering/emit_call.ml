open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Frame = Allocation.Frame

type environment = (string * int) list
type context = { output : buffer; symbols : Symbols.t; frame : Frame.t }

let allocates context environment = function
  | ImmId name when not (List.mem_assoc name environment) ->
    (match Symbols.find context.symbols name with
    | Some (Symbols.Function _) -> true
    | Some (Symbols.Global _) | None -> false)
  | ImmId _ | ImmInt _ | ImmBool _ | ImmUnit -> false
;;

let value context environment destination immediate =
  let output = context.output in
  match immediate with
  | ImmInt number -> instruction output (Li (destination, Int64.of_int number))
  | ImmBool boolean ->
    instruction output (Li (destination, if boolean then Abi.true_value else Abi.false_value))
  | ImmUnit -> instruction output (Li (destination, Abi.unit_value))
  | ImmId name ->
    (match List.assoc_opt name environment with
    | Some offset -> load output destination ~base:Sp ~offset
    | None ->
      match Symbols.require context.symbols name with
      | Symbols.Global symbol ->
        instruction output (La (T5, symbol));
        load output destination ~base:T5 ~offset:0
      | Symbols.Function function_ ->
        instruction output (La (A0, function_.symbol));
        instruction output (Li (A1, Int64.of_int function_.arity));
        instruction output (Call Abi.Runtime.make_closure);
        if destination <> A0 then instruction output (Mv (destination, A0)))
;;

let direct_target context environment function_ count =
  match function_ with
  | ImmId name when not (List.mem_assoc name environment) ->
    (match Symbols.require context.symbols name with
    | Symbols.Function function_ when function_.arity = count -> Some function_.symbol
    | Symbols.Function _ | Symbols.Global _ -> None)
  | ImmId _ -> None
  | ImmInt _ | ImmBool _ | ImmUnit ->
    raise (Symbols.Error "RISC-V: a literal cannot be applied as a function")
;;

let call context environment function_ arguments =
  let output = context.output in
  let count = List.length arguments in
  let target = direct_target context environment function_ count in
  match target with
  | Some symbol when not (List.exists (allocates context environment) arguments) ->
    (* No argument load calls runtime, so registers can be filled directly. *)
    List.iteri
      (fun index argument ->
        if index < Abi.register_argument_count
        then value context environment (List.nth argument_registers index) argument
        else (
          value context environment T0 argument;
          store output T0 ~base:Sp
            ~offset:(Frame.argument_offset (index - Abi.register_argument_count))))
      arguments;
    instruction output (Call symbol)
  | _ ->
    (match target with
     | Some _ -> ()
     | None ->
       value context environment A0 function_;
       store output A0 ~base:Sp ~offset:(Frame.callee_slot context.frame));
    List.iteri
      (fun index argument ->
        value context environment A0 argument;
        store output A0 ~base:Sp ~offset:(Frame.argument_offset index))
      arguments;
    (match target with
     | Some symbol ->
       (* Load registers before moving high arguments down into the outgoing
          stack area. Sources and destinations can overlap. *)
       List.iteri
         (fun index register ->
           if index < count
           then load output register ~base:Sp ~offset:(Frame.argument_offset index))
         argument_registers;
       List.iteri
         (fun index _ ->
           if index >= Abi.register_argument_count then (
             load output T0 ~base:Sp ~offset:(Frame.argument_offset index);
             store output T0 ~base:Sp
               ~offset:(Frame.argument_offset (index - Abi.register_argument_count))))
         arguments;
       instruction output (Call symbol)
     | None ->
       load output A0 ~base:Sp ~offset:(Frame.callee_slot context.frame);
       instruction output (Li (A1, Int64.of_int count));
       address output A2 ~base:Sp ~offset:0;
       instruction output (Call Abi.Runtime.apply))
;;
