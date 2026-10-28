(** Registers in the RV64 integer ABI. [T6] is reserved by memory helpers. *)
type register =
  | Zero | Ra | Sp | Gp | Tp
  | T0 | T1 | T2 | T3 | T4 | T5 | T6
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7

type binary = Add | Sub | Mul | Div | Xor | Slt | And | Or
type unary = Neg | Seqz | Snez

(** GNU assembler instructions and pseudo-instructions; arithmetic is RV64,
    not the sign-extending RV32 [*w] instructions. *)
type instruction =
  | Li of register * int64
  | La of register * string
  | Mv of register * register
  | Binary of binary * register * register * register
  | Unary of unary * register * register
  | Addi of register * register * int
  | Xori of register * register * int
  | Ld of register * int * register
  | Sd of register * int * register
  | Beqz of register * string
  | Bnez of register * string
  | Jump of string
  | Call of string
  | Ret

type directive =
  | Text | Bss | Global of string | Align of int
  | Function of string | End_function of string | Zero_bytes of int
  | Non_executable_stack

type line = Instruction of instruction | Directive of directive | Label of string
type program = line list

val argument_registers : register list

(** An append-only instruction builder, local to one compilation. *)
type buffer
val create : unit -> buffer
val emit : buffer -> line -> unit
val instruction : buffer -> instruction -> unit
val finish : buffer -> program

(** Signed 12-bit immediates; larger offsets use [li] and [add]. These
    helpers clobber [T6]; neither the data register nor base may be [T6]. *)
val fits_immediate : int -> bool
val load : buffer -> register -> base:register -> offset:int -> unit
val store : buffer -> register -> base:register -> offset:int -> unit
val address : buffer -> register -> base:register -> offset:int -> unit
val adjust_stack : buffer -> int -> unit
