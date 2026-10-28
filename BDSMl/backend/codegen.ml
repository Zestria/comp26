open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Frame = Allocation.Frame
module Symbols = Lowering.Symbols
module Emit_expr = Lowering.Emit_expr

let start_function output symbol =
  emit output (Directive (Align 2));
  emit output (Directive (Function symbol));
  emit output (Label symbol)
;;

let emit_function output symbols name parameters body =
  let symbol =
    match Symbols.require symbols name with
    | Symbols.Function function_ -> function_.symbol
    | Symbols.Global _ -> raise (Symbols.Error ("RISC-V: expected a function: " ^ name))
  in
  let frame = Frame.create parameters [ body ] in
  let environment =
    List.filter_map
      (function PVar name -> Some (name, Frame.slot frame name) | PUnit | PUnderscore -> None)
      parameters
  in
  start_function output symbol;
  Frame.prologue output frame parameters;
  Emit_expr.expression { output; symbols; frame } environment body;
  Frame.epilogue output frame;
  emit output (Directive (End_function symbol))
;;

let initializers program =
  List.filter_map
    (function
      | ADecl (_, _, [], body) | AEval body -> Some body
      | ADecl (_, _, _ :: _, _) -> None)
    program
;;

let emit_main output symbols program =
  let frame = Frame.create [] (initializers program) in
  emit output (Directive (Global "main"));
  start_function output "main";
  Frame.prologue output frame [];
  let context = Lowering.Emit_call.{ output; symbols; frame } in
  List.iter
    (function
      | AEval body -> Emit_expr.expression context [] body
      | ADecl (_, pattern, [], body) ->
        Emit_expr.expression context [] body;
        (match pattern with
        | PUnit | PUnderscore -> ()
        | PVar name ->
          match Symbols.require symbols name with
          | Symbols.Global symbol ->
            instruction output (La (T5, symbol));
            store output A0 ~base:T5 ~offset:0
          | Symbols.Function _ ->
            raise (Symbols.Error ("RISC-V: expected a global value: " ^ name)))
      | ADecl (_, _, _ :: _, _) -> ())
    program;
  instruction output (Li (A0, 0L));
  Frame.epilogue output frame;
  emit output (Directive (End_function "main"))
;;

let generate program =
  try
    let symbols = Symbols.create program in
    let output = create () in
    (match Symbols.globals symbols with
    | [] -> ()
    | globals ->
      emit output (Directive Bss);
      List.iter
        (fun symbol ->
          emit output (Directive (Align 3));
          emit output (Label symbol);
          emit output (Directive (Zero_bytes Abi.word_size)))
        globals);
    emit output (Directive Text);
    List.iter
      (function
        | ADecl (_, PVar name, (_ :: _ as parameters), body) ->
          emit_function output symbols name parameters body
        | ADecl _ | AEval _ -> ())
      program;
    emit_main output symbols program;
    emit output (Directive Non_executable_stack);
    Ok (Assembly.Asm_printer.print (finish output))
  with
  | Symbols.Error message -> Error message
;;
