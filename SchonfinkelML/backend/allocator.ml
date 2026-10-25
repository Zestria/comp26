open Common

(* Stupidest register allocator ever award goes to... *)

module Allocator = struct
  let registers =
    [ T0
    ; T1
    ; T2
    ; T3
    ; T4
    ; T5
    ; T6
    ; S1
    ; S2
    ; S3
    ; S4
    ; S5
    ; S6
    ; S7
    ; S8
    ; S9
    ; S10
    ; S11
    ; A0
    ; A1
    ; A2
    ; A3
    ; A4
    ; A5
    ; A6
    ; A7
    ]
  ;;

  let next_register = ref registers

  let alloc () =
    match !next_register with
    | r :: rest ->
      next_register := rest;
      r
    | [] -> failwith "get_rtl: register allocation exhausted"
  ;;

  let rec find_var env name =
    match env with
    | (name', reg) :: rest -> if name = name' then reg else find_var rest name
    | [] -> failwith ("get_rtl: unbound variable " ^ name)
  ;;
end
