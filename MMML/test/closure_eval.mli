(** Test-only semantic oracles. Both use callee-before-argument evaluation;
    this does not specify a machine ABI or the whole MiniML runtime. *)

type runtime_error =
  | Division_by_zero
  | Unbound_name of string
  | Expected_integer
  | Expected_boolean
  | Not_callable
  | Non_ground_result
  | Invalid_ir
  | Fuel_exhausted
  | Unsupported_recursion

type observation =
  { outcome : (Ast.constant, runtime_error) result
  ; trace : int list
  }

val external_names : string list
val source : ?fuel:int -> result_name:string -> Ast.program -> observation

val target
  :  ?fuel:int
  -> result_name:string
  -> Middleend.Closure_ir.program
  -> observation

val show_observation : observation -> string
