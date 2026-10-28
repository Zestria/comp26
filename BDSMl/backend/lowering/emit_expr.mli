(** Emit into a preallocated frame, leaving the result in [a0]. Branches join
    once; branch-local bindings never escape into the other branch. *)
val expression : Emit_call.context -> Emit_call.environment -> BDSMl_middleend.Anf.Anf_ast.aexpr -> unit
