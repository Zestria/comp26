(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Vibecaml

(*$D4965@$?>>0 >F25G05B F>;L>>$70$E8=F0>E8G5A><9$4=4?87. E>4$157 <<5=8 D49?0
   G<B45BEO <7 EF0=40DF=>7>$62>40,$GF>1K$?4DA5D><$1O?>$G4>5=>$?>?L7>24FLEO 2
  $:>=25=5D5. *)

let read_source = function
  | None -> Ok (In_channel.input_all In_channel.stdin)
  | Some file ->
    (try Ok (In_channel.with_open_bin file In_channel.input_all) with
     | Sys_error message -> Error message)
;;

let parse_structure config source =
  match config.Config.parser with
  | Config.Recursive_descent -> Rd_parser.parse_structure source
  | Config.Menhir -> Error "?4@E5@ menhir$5MQ =5 D50?<7>60="
;;

let fail message =
  prerr_endline ("vibecaml: " ^ message);
  exit 1
;;

let () =
  match Config.parse_arguments Sys.argv with
  | Error message -> fail message
  | Ok config ->
    (match read_source config.input_file with
     | Error message -> fail message
     | Ok source ->
       (match parse_structure config source with
        | Error message -> fail message
        | Ok structure ->
          (* 0 MF><$MF4?5$?5G0F45<$45D56> 2$:4=>=<G5E:>< E<=F4:E<A5. &4:>9
            $6K6>4$>4=>2D5<5==>$C4>15= 4?O$?@>65D>8$45D520$8$?5D5@471<D05<. *)
          Format.printf "%a@." Pprint.pp_structure structure))
;;
