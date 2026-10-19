[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module type S = sig
  val context : Llvm.llcontext
  val module_ : Llvm.llmodule
  val builder : Llvm.llbuilder
  val build_store : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue

  val build_call
    :  Llvm.lltype
    -> ?name:string
    -> Llvm.llvalue
    -> Llvm.llvalue list
    -> Llvm.llvalue

  val lookup_func : string -> (Llvm.llvalue, string) result
  val has_toplevel_func : string -> bool
  val build_add : ?name:string -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_sub : ?name:string -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_mul : ?name:string -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_sdiv : ?name:string -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue

  val build_icmp
    :  ?name:string
    -> Llvm.Icmp.t
    -> Llvm.llvalue
    -> Llvm.llvalue
    -> Llvm.llvalue

  val set_metadata
    :  Llvm.llvalue
    -> string
    -> ('a, Format.formatter, unit, Llvm.llvalue) format4
    -> 'a

  val build_ptrtoint : ?name:string -> Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue
  val build_inttoptr : ?name:string -> Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue
  val build_pointercast : ?name:string -> Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue
  val const_int : Llvm.lltype -> int -> Llvm.llvalue
  val params : Llvm.llvalue -> Llvm.llvalue array
  val pp_value : Format.formatter -> Llvm.llvalue -> unit
end

val make : Llvm.llcontext -> Llvm.llbuilder -> Llvm.llmodule -> (module S)
