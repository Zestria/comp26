[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Счётчик общий для всех проходов одной программы: иначе имена разных
   проходов могут совпасть. Сброс делает драйвер перед следующей программой.
   Исключение касается только этого намеренного изменяемого состояния;
   в конце модуля возвращаем проверку для следующих файлов Zanuda. *)
[@@@zanuda "-mutability_check"]

let counter = ref 0

let name base =
  incr counter;
  Printf.sprintf "%s__%d" base !counter
;;

let reset () = counter := 0

[@@@zanuda "+mutability_check"]
