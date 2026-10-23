[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module StringSet = Set.Make(String)

type t = {
  taken : StringSet.t;
  mutable next : int;
}

let make (taken : StringSet.t) : t = { taken; next = 0 }

let fresh (st : t) (prefix : string) : string =
  let rec loop () =
    let name = prefix ^ string_of_int st.next in
    st.next <- st.next + 1;
    if StringSet.mem name st.taken then loop () else name
  in
  loop ()