(** [Fresh] -- генерация свежих имён и сбор всех идентификаторов программы.

    (Альфа-преобразование этого модуля сейчас не касается: переименование
    биндеров уже делает [Anf_conv].)

    TODO(Анастасия): эти функции -- твоя часть, ниже контракт.  Сейчас внутри
    реализация, перенесённая из копий в [Closure_conversion] и
    [Lambda_lifting]; можешь заменить её своей, не меняя сигнатуры. *)

(* вообще можешь и сигнатуры менять, но тогда в CC и LL адаптируй *)

module SSet : Set.S with type elt = string

(** All identifiers that occur in the program, binders and uses alike. *)
val program_idents : Anf.program -> SSet.t

(* тип генератора *)
type t

(** Create a generator from the set of all identifiers of the program. *)
val create : taken:SSet.t -> t

(** Create a generator directly from an ANF program. *)
val create_from_program : Anf.program -> t

(** A fresh name based on [base].  The [suffix] distinguishes passes; the
    result is not among [taken] and not generated before. *)
val fresh : ?suffix:string -> t -> string -> string
