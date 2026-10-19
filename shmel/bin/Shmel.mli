[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Llvm

(** Helpers over the LLVM builder API, bound to one context, builder and module. *)
module type S = sig
  val context : Llvm.llcontext
  val module_ : Llvm.llmodule
  val builder : Llvm.llbuilder
  val build_store : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_call : lltype -> ?name:string -> llvalue -> llvalue list -> llvalue
  val lookup_func : string -> (llvalue, string) result
  val has_toplevel_func : string -> bool
  val build_add : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sub : ?name:string -> llvalue -> llvalue -> llvalue
  val build_mul : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sdiv : ?name:string -> llvalue -> llvalue -> llvalue
  val build_icmp : ?name:string -> Icmp.t -> llvalue -> llvalue -> llvalue

  (** [set_metadata v kind fmt] sets metadata to value [v] of kind [k].
      Returns this value [v]. Useful for attaching debugging *)
  val set_metadata
    :  llvalue
    -> string
    -> ('a, Format.formatter, unit, llvalue) format4
    -> 'a

  val build_ptrtoint : ?name:string -> llvalue -> lltype -> llvalue
  val build_inttoptr : ?name:string -> llvalue -> lltype -> llvalue
  val build_pointercast : ?name:string -> llvalue -> lltype -> llvalue

  (** Just aliases *)

  val const_int : Llvm.lltype -> int -> Llvm.llvalue
  val params : Llvm.llvalue -> Llvm.llvalue array
  val pp_value : Format.formatter -> llvalue -> unit
end

(** [make context builder module_] binds the helpers to the given LLVM state. *)
val make : llcontext -> llbuilder -> llmodule -> (module S)
