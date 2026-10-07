open Anf_example

let rec generate env depth =
  let number = QCheck.Gen.map (fun value -> Num value) (QCheck.Gen.int_range (-10) 10) in
  let leaf =
    match env with
    | [] -> number
    | _ ->
      QCheck.Gen.oneof
        [ number; QCheck.Gen.map (fun name -> Var name) (QCheck.Gen.oneof_list env) ]
  in
  if depth = 0
  then leaf
  else (
    let child = generate env (depth - 1) in
    let binding =
      QCheck.Gen.oneof
        (List.map
           (fun name ->
             QCheck.Gen.map2
               (fun rhs body -> Let (name, rhs, body))
               child
               (generate (name :: env) (depth - 1)))
           [ "x"; "y"; "anf0" ])
    in
    QCheck.Gen.oneof_weighted
      [ 4, leaf
      ; 2, QCheck.Gen.map2 (fun left right -> Add (left, right)) child child
      ; 2, binding
      ; ( 1
        , QCheck.Gen.map3
            (fun condition yes no -> If (condition, yes, no))
            child
            child
            child )
      ])
;;

let rec shrink expression yield =
  let emit candidate = if closed_source Names.empty candidate then yield candidate in
  let shrink_child rebuild child = shrink child (fun smaller -> emit (rebuild smaller)) in
  match expression with
  | Num value -> QCheck.Shrink.int value (fun smaller -> emit (Num smaller))
  | Var _ -> ()
  | Add (left, right) ->
    emit left;
    emit right;
    shrink_child (fun smaller -> Add (smaller, right)) left;
    shrink_child (fun smaller -> Add (left, smaller)) right
  | Let (name, rhs, body) ->
    emit rhs;
    emit body;
    shrink_child (fun smaller -> Let (name, smaller, body)) rhs;
    shrink_child (fun smaller -> Let (name, rhs, smaller)) body
  | If (condition, yes, no) ->
    emit yes;
    emit no;
    shrink_child (fun smaller -> If (smaller, yes, no)) condition;
    shrink_child (fun smaller -> If (condition, smaller, no)) yes;
    shrink_child (fun smaller -> If (condition, yes, smaller)) no
;;

let arbitrary =
  QCheck.make
    ~print:show
    ~shrink
    (QCheck.Gen.sized (fun size -> generate [] (min 4 size)))
;;

let semantic =
  QCheck.Test.make
    ~name:"ANF preserves integer result"
    ~count:1000
    arbitrary
    (fun source ->
       let expected = eval_source [] source in
       let actual = eval_term [] (normalize source) in
       if expected = actual
       then true
       else
         QCheck.Test.fail_reportf
           "source=%s expected=%d actual=%d"
           (show source)
           expected
           actual)
;;

let scope =
  QCheck.Test.make
    ~name:"ANF preserves scope and uses unique binders"
    ~count:1000
    arbitrary
    (fun source ->
       closed_source Names.empty source && scoped Names.empty (normalize source))
;;

let regressions =
  let source = Let ("x", Num 7, Add (Let ("x", Num 2, Var "x"), Var "x")) in
  let free = Add (Add (Num 1, Num 2), Var "anf0") in
  QCheck.Test.make
    ~name:"shadowing, free-name collision, and branches"
    ~count:1
    QCheck.unit
    (fun () ->
       eval_term [] (normalize source) = 9
       && scoped (Names.singleton "anf0") (normalize free)
       && eval_term [ "anf0", 10 ] (normalize free) = 13
       && eval_term [] (normalize (If (Num 0, Var "unbound", Num 42))) = 42
       && normalize source = normalize source)
;;

let () = QCheck_runner.run_tests_main [ semantic; scope; regressions ]
