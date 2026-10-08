open Middleend.Closure_ir

let p name = Ast.PVariable name
let local name = Reference (Local name)

let program ?(externals = []) body =
  { externals; definitions = [ Definition (p "result", body) ] }
;;

let identity =
  Make_closure ({ parameter = p "x"; capture_names = []; body = local "x" }, [])
;;

let make_adder =
  let inner =
    { parameter = p "y"
    ; capture_names = [ "x" ]
    ; body = Call (Call (Reference (External "+"), Reference (Env_slot 0)), local "y")
    }
  in
  Make_closure
    ( { parameter = p "x"; capture_names = []; body = Make_closure (inner, [ Local "x" ]) }
    , [] )
;;

let check_valid value =
  match validate value with
  | Ok () -> ()
  | Error errors ->
    failwith
      (String.concat "\n" (List.map (Format.asprintf "%a" pp_validation_error) errors))
;;

let check_invalid expected value =
  match validate value with
  | Error errors when List.exists (fun error -> error.problem = expected) errors -> ()
  | _ -> failwith "validator did not report the expected problem"
;;

let () =
  check_valid (program identity);
  check_valid (program ~externals:[ "+" ] make_adder);
  check_invalid Self_outside_code (program (Reference Self));
  check_invalid Environment_outside_code (program (Reference (Env_slot 0)));
  check_invalid (Undeclared_external "missing") (program (Reference (External "missing")));
  let closed body =
    Make_closure ({ parameter = p "arg"; capture_names = []; body }, [])
  in
  check_invalid
    (Unbound_local "x")
    (program (Let (p "x", Constant (Ast.Integer 1), closed (local "x"))));
  List.iter
    (fun index ->
       let code =
         { parameter = p "arg"
         ; capture_names = [ "x" ]
         ; body = Reference (Env_slot index)
         }
       in
       check_invalid
         (Invalid_slot (index, 1))
         (program
            (Let (p "x", Constant (Ast.Integer 1), Make_closure (code, [ Local "x" ])))))
    [ -1; 1 ];
  check_invalid
    (Capture_count_mismatch (1, 0))
    (program
       (Make_closure
          ({ parameter = p "arg"; capture_names = [ "x" ]; body = local "arg" }, [])));
  check_invalid
    (Duplicate_capture_name "x")
    (program
       ~externals:[ "x" ]
       (Make_closure
          ( { parameter = p "arg"; capture_names = [ "x"; "x" ]; body = local "arg" }
          , [ External "x"; External "x" ] )));
  (* Capture expressions use the creation scope, not the new code's parameter. *)
  check_invalid
    (Unbound_local "arg")
    (program
       (Make_closure
          ( { parameter = p "arg"; capture_names = [ "arg" ]; body = local "arg" }
          , [ Local "arg" ] )));
  let value = program ~externals:[ "+" ] make_adder in
  let first = Format.asprintf "%a" pp_program value in
  let second = Format.asprintf "%a" pp_program value in
  if first <> second || String.length first = 0 then failwith "unstable rendering";
  print_endline "closure IR: constructors, scope, layouts and rendering passed"
;;
