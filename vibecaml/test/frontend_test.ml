open Vibecaml

let assert_equal expected actual =
  if not (String.equal expected actual)
  then failwith ("ожидалось: " ^ expected ^ "; получено: " ^ actual)
;;

let () =
  let open Ast in
  let addition =
    Expr_app
      (Expr_app (Expr_ident "+", Expr_constant (Const_int 1)), Expr_constant (Const_int 2))
  in
  assert_equal "((+ 1) 2)" (Pprint.string_of_expression addition);
  let pair =
    Expr_tuple (Expr_constant (Const_int 1), Expr_constant (Const_bool true), [])
  in
  assert_equal "(1, true)" (Pprint.string_of_expression pair);
  match
    Config.parse_arguments
      [| "vibecaml"; "--parser"; "menhir"; "--ext-glued-keywords"; "--dump-ast" |]
  with
  | Ok config ->
    assert (config.parser = Config.Menhir);
    assert config.extensions.glued_keywords;
    assert (not config.extensions.operator_declarations);
    assert config.dump_ast;
    assert (not config.dump_pretty_printed)
  | Error message -> failwith message
;;
