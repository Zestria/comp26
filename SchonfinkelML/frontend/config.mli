[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

[@@@zanuda "-mutability_check"] (* Config is required to use with assignment '<-' *)

(** Which syntactic analyser to run. *)
type parser_kind =
  | Top_down (** The recursive-descent parser. *)
  | Bottom_up (** The Menhir-generated LR parser. *)

val string_of_parser_kind : parser_kind -> string
val parser_kind_of_string : string -> (parser_kind, string) result

(** How far down the compiler pipeline to run. *)
type stop_after =
  | Parsing (** Stop right after parsing. *)
  | Anf (** Stop after the conversion to A-normal form. *)

val stop_after_of_string : string -> (stop_after, string) result
val string_of_stop_after : stop_after -> string

(** The mutable configuration filled in by [Arg.parse]. *)
type config =
  { mutable output : string (** The output file, or ["-"] for stdout. *)
  ; mutable parser : parser_kind (** The selected parser. *)
  ; mutable parser_settings : string option
    (** The operator table passed with [--parser-settings], if any. *)
  ; mutable stop_after : stop_after option (** The stage to stop after, if any. *)
  }

val default_config : unit -> config
val usage_msg : string

(** [flag_specs cfg] is the list of options that fill [cfg], without help. *)
val flag_specs : config -> (string * Arg.spec * string) list

(** [make_specs cfg] is [flag_specs cfg] plus the [-h] help option. *)
val make_specs : config -> (string * Arg.spec * string) list
