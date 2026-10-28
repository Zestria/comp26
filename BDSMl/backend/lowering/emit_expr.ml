open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Frame = Allocation.Frame

let binary context environment operator left right =
  let output = context.Emit_call.output in
  let right_allocates = Emit_call.allocates context environment right in
  Emit_call.value context environment T0 left;
  if right_allocates then
    store output T0 ~base:Sp ~offset:(Frame.operand_slot context.frame);
  Emit_call.value context environment T1 right;
  if right_allocates then
    load output T0 ~base:Sp ~offset:(Frame.operand_slot context.frame);
  let emit_op op = instruction output op in
  match operator with
  | Plus -> emit_op (Binary (Add, A0, T0, T1))
  | Minus -> emit_op (Binary (Sub, A0, T0, T1))
  | Mult -> emit_op (Binary (Mul, A0, T0, T1))
  | Div ->
    let nonzero = Symbols.fresh_label context.symbols in
    emit_op (Bnez (T1, nonzero));
    emit_op (Call Abi.Runtime.division_by_zero);
    emit output (Label nonzero);
    emit_op (Binary (Div, A0, T0, T1))
  | Equal | NotEqual ->
    emit_op (Binary (Xor, A0, T0, T1));
    emit_op (Unary ((if operator = Equal then Seqz else Snez), A0, A0))
  | Less -> emit_op (Binary (Slt, A0, T0, T1))
  | More -> emit_op (Binary (Slt, A0, T1, T0))
  | ELess ->
    emit_op (Binary (Slt, A0, T1, T0));
    emit_op (Xori (A0, A0, 1))
  | EMore ->
    emit_op (Binary (Slt, A0, T0, T1));
    emit_op (Xori (A0, A0, 1))
  | And | Or ->
    raise (Symbols.Error "RISC-V: ANF must lower short-circuit operators to CIf")
;;

let rec expression context environment = function
  | ACExpr computation -> emit_computation context environment computation
  | ALet (pattern, computation, body) ->
    emit_computation context environment computation;
    let environment =
      match pattern with
      | PUnit | PUnderscore -> environment
      | PVar name ->
        let offset = Frame.slot context.Emit_call.frame name in
        store context.output A0 ~base:Sp ~offset;
        (name, offset) :: environment
    in
    expression context environment body

and emit_computation context environment = function
  | CImm immediate -> Emit_call.value context environment A0 immediate
  | CApp (function_, arguments) -> Emit_call.call context environment function_ arguments
  | CBinOp (operator, left, right) -> binary context environment operator left right
  | CUnOp (operator, operand) ->
    Emit_call.value context environment A0 operand;
    instruction context.output (Unary ((match operator with Neg -> Neg | Not -> Seqz), A0, A0))
  | CIf (condition, if_true, if_false) ->
    let if_true_label = Symbols.fresh_label context.symbols in
    let if_false_label = Symbols.fresh_label context.symbols in
    let end_label = Symbols.fresh_label context.symbols in
    Emit_call.value context environment A0 condition;
    (* Keep conditional branches short, independent of branch body size. *)
    instruction context.output (Bnez (A0, if_true_label));
    instruction context.output (Jump if_false_label);
    emit context.output (Label if_true_label);
    expression context environment if_true;
    instruction context.output (Jump end_label);
    emit context.output (Label if_false_label);
    expression context environment if_false;
    emit context.output (Label end_label)
;;
