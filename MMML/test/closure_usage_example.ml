let () =
  let source =
    "let make x = let f y = x + y in f let add10 = make 10 let result = add10 5"
  in
  let ast =
    match Parser_menhir.parse_program source with
    | Ok ast -> ast
    | Error error -> failwith (Error_monad.show_error error)
  in
  let converted =
    match Middleend.Closure_conversion.convert ~externals:[ "+" ] ast with
    | Ok ast -> ast
    | Error error ->
      failwith (Format.asprintf "%a" Middleend.Closure_conversion.pp_error error)
  in
  assert (Closure_test_invariants.check ~externals:[ "+" ] converted = Ok ());
  assert (
    (Closure_eval.source ~result_name:"result" converted).outcome = Ok (Ast.Integer 15));
  Format.printf "%a@." Pretty_printer.pp_program converted
;;
