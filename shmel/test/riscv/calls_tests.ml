[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_backend
open Shmel_middleend
open Riscv

let load rd = function
  | Anf.ImmInt n -> Ok [ Li (rd, n) ]
  | Anf.ImmVar "f" -> Ok [ La (rd, "closure_f") ]
  | Anf.ImmVar x -> Error ("unknown operand " ^ x)
;;

let () =
  let f = Anf.ImmVar "f" in
  assert (
    Calls.call ~load f [ Anf.ImmInt 2; Anf.ImmInt 3 ]
    = Ok
        [ I (Addi, Sp, Sp, -16)
        ; Li (T 0, 2)
        ; Sd (T 0, 0, Sp)
        ; Li (T 0, 3)
        ; Sd (T 0, 8, Sp)
        ; La (A 0, "closure_f")
        ; Li (A 1, 2)
        ; Mv (A 2, Sp)
        ; Call "shmel_apply"
        ; I (Addi, Sp, Sp, 16)
        ]);
  let odd = Result.get_ok (Calls.call ~load f [ Anf.ImmInt 1 ]) in
  assert (List.hd odd = I (Addi, Sp, Sp, -16));
  let large =
    Result.get_ok (Calls.call ~load f (List.init 300 (fun i -> Anf.ImmInt i)))
  in
  assert (List.hd large = Li (T 1, -2400));
  assert (List.mem (Li (T 1, 2048)) large);
  assert (List.mem (Sd (T 0, 0, T 1)) large);
  assert (List.rev large |> List.take 2 = [ R (Add, Sp, Sp, T 1); Li (T 1, 2400) ]);
  assert (Calls.call ~load f [ Anf.ImmVar "missing" ] = Error "unknown operand missing");
  assert (Calls.call ~load (Anf.ImmVar "missing") [] = Error "unknown operand missing");
  let hooks = Calls.hooks ~load_global:(fun rd x -> Ok [ La (rd, x) ]) in
  assert (hooks.load_global (A 0) "global" = Ok [ La (A 0, "global") ]);
  assert (hooks.call ~load f [] = Calls.call ~load f [])
;;
