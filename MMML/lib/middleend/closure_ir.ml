type reference =
  | Local of string
  | External of string
  | Env_slot of int
  | Self

type expr =
  | Constant of Ast.constant
  | Reference of reference
  | Make_closure of code * reference list
  | Call of expr * expr
  | Let of Ast.pattern * expr * expr
  | If of expr * expr * expr

and code =
  { parameter : Ast.pattern
  ; capture_names : string list
  ; body : expr
  }

type definition = Definition of Ast.pattern * expr

type program =
  { externals : string list
  ; definitions : definition list
  }

type validation_problem =
  | Unbound_local of string
  | Undeclared_external of string
  | Environment_outside_code
  | Self_outside_code
  | Invalid_slot of int * int
  | Capture_count_mismatch of int * int
  | Duplicate_capture_name of string

type validation_error =
  { path : string list
  ; problem : validation_problem
  }

module Names = Set.Make (String)

let bind names = function
  | Ast.PWildcard -> names
  | Ast.PVariable name -> Names.add name names
;;

let validate program =
  let externals = Names.of_list program.externals in
  let report path problem errors = { path = List.rev path; problem } :: errors in
  let reference locals slots path reference errors =
    match reference with
    | Local name ->
      if Names.mem name locals then errors else report path (Unbound_local name) errors
    | External name ->
      if Names.mem name externals
      then errors
      else report path (Undeclared_external name) errors
    | Self ->
      if Option.is_some slots then errors else report path Self_outside_code errors
    | Env_slot index ->
      (match slots with
       | None -> report path Environment_outside_code errors
       | Some count ->
         if index >= 0 && index < count
         then errors
         else report path (Invalid_slot (index, count)) errors)
  in
  let rec expression locals slots path expr errors =
    match expr with
    | Constant _ -> errors
    | Reference value -> reference locals slots path value errors
    | Call (callee, argument) ->
      let errors = expression locals slots ("callee" :: path) callee errors in
      expression locals slots ("argument" :: path) argument errors
    | Let (pattern, rhs, body) ->
      let errors = expression locals slots ("let.rhs" :: path) rhs errors in
      expression (bind locals pattern) slots ("let.body" :: path) body errors
    | If (condition, yes, no) ->
      let errors = expression locals slots ("if.condition" :: path) condition errors in
      let errors = expression locals slots ("if.then" :: path) yes errors in
      expression locals slots ("if.else" :: path) no errors
    | Make_closure (code, captures) ->
      let count = List.length code.capture_names in
      let actual = List.length captures in
      let errors =
        if count = actual
        then errors
        else report path (Capture_count_mismatch (count, actual)) errors
      in
      let _, errors =
        List.fold_left
          (fun (seen, errors) name ->
             let errors =
               if Names.mem name seen
               then report path (Duplicate_capture_name name) errors
               else errors
             in
             Names.add name seen, errors)
          (Names.empty, errors)
          code.capture_names
      in
      let _, errors =
        List.fold_left
          (fun (index, errors) capture ->
             ( index + 1
             , reference
                 locals
                 slots
                 (Printf.sprintf "capture[%d]" index :: path)
                 capture
                 errors ))
          (0, errors)
          captures
      in
      expression
        (bind Names.empty code.parameter)
        (Some count)
        ("code" :: path)
        code.body
        errors
  in
  let _, _, errors =
    List.fold_left
      (fun (index, locals, errors) (Definition (pattern, rhs)) ->
         let errors =
           expression locals None [ Printf.sprintf "definition[%d]" index ] rhs errors
         in
         index + 1, bind locals pattern, errors)
      (0, Names.empty, [])
      program.definitions
  in
  match List.rev errors with
  | [] -> Ok ()
  | errors -> Error errors
;;

let pp_validation_error formatter { path; problem } =
  Format.fprintf formatter "%s: " (String.concat "/" path);
  match problem with
  | Unbound_local name -> Format.fprintf formatter "unbound local %S" name
  | Undeclared_external name -> Format.fprintf formatter "undeclared external %S" name
  | Environment_outside_code ->
    Format.pp_print_string formatter "environment outside code"
  | Self_outside_code -> Format.pp_print_string formatter "self outside code"
  | Invalid_slot (index, count) ->
    Format.fprintf formatter "capture slot %d outside layout of size %d" index count
  | Capture_count_mismatch (expected, actual) ->
    Format.fprintf formatter "expected %d captures, got %d" expected actual
  | Duplicate_capture_name name ->
    Format.fprintf formatter "duplicate capture label %S" name
;;

let pp_reference formatter = function
  | Local name -> Format.fprintf formatter "local(%S)" name
  | External name -> Format.fprintf formatter "external(%S)" name
  | Env_slot index -> Format.fprintf formatter "env[%d]" index
  | Self -> Format.pp_print_string formatter "self"
;;

let pp_list printer =
  Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ") printer
;;

let rec pp_expr formatter = function
  | Constant value -> Ast.pp_constant formatter value
  | Reference value -> pp_reference formatter value
  | Call (callee, argument) ->
    Format.fprintf formatter "@[<hov 2>call(%a,@ %a)@]" pp_expr callee pp_expr argument
  | Let (pattern, rhs, body) ->
    Format.fprintf
      formatter
      "@[<v 2>let %a = %a in@ %a@]"
      Ast.pp_pattern
      pattern
      pp_expr
      rhs
      pp_expr
      body
  | If (condition, yes, no) ->
    Format.fprintf
      formatter
      "@[<v 2>if %a then@ %a@ else@ %a@]"
      pp_expr
      condition
      pp_expr
      yes
      pp_expr
      no
  | Make_closure (code, captures) ->
    Format.fprintf
      formatter
      "@[<v 2>closure [%a] {@ param %a; slots [%a];@ %a@;<1 -2>}@]"
      (pp_list pp_reference)
      captures
      Ast.pp_pattern
      code.parameter
      (pp_list (fun f name -> Format.fprintf f "%S" name))
      code.capture_names
      pp_expr
      code.body
;;

let pp_program formatter program =
  let pp_definition f (Definition (pattern, rhs)) =
    Format.fprintf f "@[<v 2>bind %a =@ %a@]" Ast.pp_pattern pattern pp_expr rhs
  in
  Format.fprintf
    formatter
    "@[<v>externals [%a]@,%a@]"
    (pp_list (fun f name -> Format.fprintf f "%S" name))
    program.externals
    (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "@,") pp_definition)
    program.definitions
;;
