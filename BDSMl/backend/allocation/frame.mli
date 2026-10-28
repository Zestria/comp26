type t

(** Stable slots for alpha-renamed parameters/locals. The low end of the frame
    is a reusable outgoing argument/argv area, followed by two temporaries,
    locals, and saved [ra]. [sp] stays 16-byte aligned. *)
val create : BDSMl_frontend.Ast.pattern list -> BDSMl_middleend.Anf.Anf_ast.aexpr list -> t
val size : t -> int
val slot : t -> string -> int
val callee_slot : t -> int
val operand_slot : t -> int
val argument_offset : int -> int
val prologue : Assembly.Asm.buffer -> t -> BDSMl_frontend.Ast.pattern list -> unit
val epilogue : Assembly.Asm.buffer -> t -> unit
