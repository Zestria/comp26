type register =
  | Zero | Ra | Sp | Gp | Tp
  | T0 | T1 | T2 | T3 | T4 | T5 | T6
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7

type binary = Add | Sub | Mul | Div | Xor | Slt | And | Or
type unary = Neg | Seqz | Snez

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
let argument_registers = [ A0; A1; A2; A3; A4; A5; A6; A7 ]
type buffer = { mutable reversed : line list }
let create () = { reversed = [] }
let emit buffer line = buffer.reversed <- line :: buffer.reversed
let instruction buffer op = emit buffer (Instruction op)
let finish buffer = List.rev buffer.reversed
let fits_immediate offset = offset >= -2048 && offset <= 2047

let check_scratch data base =
  if data = T6 || base = T6 then invalid_arg "Asm: T6 is reserved for address expansion"
;;

let address buffer destination ~base ~offset =
  check_scratch destination base;
  if fits_immediate offset
  then instruction buffer (Addi (destination, base, offset))
  else (
    instruction buffer (Li (T6, Int64.of_int offset));
    instruction buffer (Binary (Add, destination, base, T6)))
;;

let load buffer destination ~base ~offset =
  check_scratch destination base;
  if fits_immediate offset
  then instruction buffer (Ld (destination, offset, base))
  else (
    instruction buffer (Li (T6, Int64.of_int offset));
    instruction buffer (Binary (Add, T6, base, T6));
    instruction buffer (Ld (destination, 0, T6)))
;;

let store buffer source ~base ~offset =
  check_scratch source base;
  if fits_immediate offset
  then instruction buffer (Sd (source, offset, base))
  else (
    instruction buffer (Li (T6, Int64.of_int offset));
    instruction buffer (Binary (Add, T6, base, T6));
    instruction buffer (Sd (source, 0, T6)))
;;

let adjust_stack buffer amount = address buffer Sp ~base:Sp ~offset:amount
