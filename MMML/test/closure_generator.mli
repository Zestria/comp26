(** A typed, scope-aware, recursion-free test language. Type annotations are
    test metadata only; [program] erases them into the existing parser AST. *)
type term

val arbitrary : term QCheck.arbitrary
val program : term -> Ast.program
val well_formed : term -> bool
val shrinks : term -> term list
