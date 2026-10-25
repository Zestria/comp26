val string_of_reg : Common.reg -> string
val write_instr : out_channel -> Common.instr -> unit
val write_item : out_channel -> Emitter.item -> unit
val write : out_channel -> Emitter.item list -> unit
