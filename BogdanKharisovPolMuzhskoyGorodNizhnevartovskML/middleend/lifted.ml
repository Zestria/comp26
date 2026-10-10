[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** All function code is declared at program scope. Function values inside
    expressions only carry a code label and captured values. *)
type closure =
  { code : Id.t
  ; captures : Anf.atom list
  }

type expression = closure Anf.term

type function_definition =
  { code : Id.t
  ; environment : Id.t list
  ; parameter : Id.t
  ; parameters : Id.t list
  ; body : expression
  }

type program =
  { functions : function_definition list
  ; body : expression
  }

type error =
  | Binding of Lambda.invariant_error
  | Duplicate_code of Id.t
  | Unknown_code of Id.t
  | Capture_count of Id.t * int * int

let error_to_string = function
  | Binding error -> Lambda.invariant_error_to_string error
  | Duplicate_code id -> "Duplicate code label " ^ Id.name id
  | Unknown_code id -> "Unknown code label " ^ Id.name id
  | Capture_count (id, expected, actual) ->
    Printf.sprintf
      "Code %s needs %d environment slots but has %d captures"
      (Id.name id)
      expected
      actual
;;

let validate program =
  let ( let* ) = Result.bind in
  let binding_error error = Binding error in
  let rec collect declarations = function
    | [] -> Ok declarations
    | (definition : function_definition) :: _ when Id.Map.mem definition.code declarations
      -> Error (Duplicate_code definition.code)
    | definition :: rest ->
      collect (Id.Map.add definition.code definition declarations) rest
  in
  let* declarations = collect Id.Map.empty program.functions in
  let function_ declared scope (closure : closure) =
    match Id.Map.find_opt closure.code declarations with
    | None -> Error (Unknown_code closure.code)
    | Some definition ->
      let expected = List.length definition.environment in
      let actual = List.length closure.captures in
      if expected <> actual
      then Error (Capture_count (closure.code, expected, actual))
      else
        let* () =
          Result.map_error binding_error (Anf.Scope.atoms scope closure.captures)
        in
        Ok declared
  in
  let rec definitions declared = function
    | [] ->
      Anf.Scope.term ~error:binding_error function_ declared Id.Set.empty program.body
    | (definition : function_definition) :: rest ->
      let* declared, scope =
        Result.map_error
          binding_error
          (Anf.Scope.bind_all
             declared
             Id.Set.empty
             (definition.environment @ (definition.parameter :: definition.parameters)))
      in
      let* declared =
        Anf.Scope.term ~error:binding_error function_ declared scope definition.body
      in
      definitions declared rest
  in
  Anf.Scope.run (fun declared -> definitions declared program.functions)
;;
