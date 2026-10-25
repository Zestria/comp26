module Anf = SchonfinkelML_middleend.Anf

type item =
  | Instruction of Common.instr
  | Label of string

module Monad : sig
  type 'a t = item list -> ('a * item list, Common.error) result

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val emit : Common.instr Common.checked -> unit t
  val label : string -> unit t
  val start : unit t
  val function_label : string -> unit t
  val run : unit t -> (item list, Common.error) result
end

val get_rtl : Anf.aexpr -> (item list, Common.error) result
