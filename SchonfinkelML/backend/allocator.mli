module Allocator : sig
  val registers : Common.reg list
  val next_register : Common.reg list ref
  val alloc : unit -> Common.reg
  val find_var : (string * 'a) list -> string -> 'a
end
