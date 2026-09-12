(** Result computations carrying a positioned frontend diagnostic. *)

type error =
  { location : Location.point  (** Where the error was detected. *)
  ; message : string  (** Human-readable explanation without the location. *)
  }
[@@deriving eq]

(** The result type shared by frontend phases. *)
type 'a t = ('a, error) result

(** Lift [value] into a successful computation. *)
let return value = Ok value

(** [error location message] constructs an error value without raising an
    exception. The value is propagated by {!bind} and [( let* )]. *)
let error location message = Error { location; message }

(** Sequence a computation and a dependent continuation. *)
let bind computation continuation = Result.bind computation continuation

(** Transform the successful result of [computation]. *)
let map computation transform = Result.map transform computation

(** Let-operator syntax for error-propagating computations. *)
module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) = map
end

(** Print an error together with its line and column. *)
let pp_error formatter { location; message } =
  Format.fprintf formatter "%a: %s" Location.pp_point location message
;;

(** Render an error to a string. *)
let show_error error = Format.asprintf "%a" pp_error error
