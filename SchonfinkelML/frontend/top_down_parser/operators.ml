[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Common = Frontend.Common

type assoc =
  | Left
  | Right
  | Nonassoc

type fixity =
  | Prefix
  | Infix of assoc

type entry =
  { sym : string
  ; tok : Common.token
  ; fixity : fixity
  ; prec : int
  }

type t = entry list

let token_of_sym = function
  | "+" -> Some Common.PLUS
  | "-" -> Some Common.MINUS
  | "*" -> Some Common.TIMES
  | "/" -> Some Common.DIV
  | "=" -> Some Common.EQ
  | "<" -> Some Common.LT
  | ">" -> Some Common.GT
  | "<=" -> Some Common.LE
  | ">=" -> Some Common.GE
  | _ -> None
;;

let default =
  [ { sym = "="; tok = Common.EQ; fixity = Infix Left; prec = 4 }
  ; { sym = "<"; tok = Common.LT; fixity = Infix Left; prec = 4 }
  ; { sym = ">"; tok = Common.GT; fixity = Infix Left; prec = 4 }
  ; { sym = "<="; tok = Common.LE; fixity = Infix Left; prec = 4 }
  ; { sym = ">="; tok = Common.GE; fixity = Infix Left; prec = 4 }
  ; { sym = "+"; tok = Common.PLUS; fixity = Infix Left; prec = 6 }
  ; { sym = "-"; tok = Common.MINUS; fixity = Infix Left; prec = 6 }
  ; { sym = "*"; tok = Common.TIMES; fixity = Infix Left; prec = 7 }
  ; { sym = "/"; tok = Common.DIV; fixity = Infix Left; prec = 7 }
  ; { sym = "-"; tok = Common.MINUS; fixity = Prefix; prec = 9 }
  ]
;;

let fixity_word = function
  | Prefix -> "prefix"
  | Infix Left -> "left"
  | Infix Right -> "right"
  | Infix Nonassoc -> "nonassoc"
;;

let fixity_of_word = function
  | "prefix" -> Some Prefix
  | "left" -> Some (Infix Left)
  | "right" -> Some (Infix Right)
  | "nonassoc" -> Some (Infix Nonassoc)
  | _ -> None
;;

let known_operators = "+ - * / = < > <= >="

(* Splits on every kind of space, dropping empty words, so that a table may be
   written on one line or spread over several. *)
let words s =
  let blank = function
    | '\t' | '\n' | '\r' -> ' '
    | c -> c
  in
  String.map blank s |> String.split_on_char ' ' |> List.filter (fun w -> w <> "")
;;

(* Two entries collide when they use the same spelling with the same arity: a
   spelling may be both an infix and a prefix operator, as [-] is. *)
let same_fixity a b =
  match a, b with
  | Prefix, Prefix -> true
  | Infix _, Infix _ -> true
  | Prefix, Infix _ | Infix _, Prefix -> false
;;

let of_string s =
  let entry_of_sym sym fixity prec =
    match token_of_sym sym with
    | None ->
      Error
        (Printf.sprintf "unknown operator %S (expected one of: %s)" sym known_operators)
    | Some tok -> Ok { sym; tok; fixity; prec }
  in
  let rec add_syms acc fixity prec = function
    | [] -> Ok acc
    | sym :: syms ->
      (match entry_of_sym sym fixity prec with
       | Error _ as e -> e
       | Ok entry ->
         if List.exists (fun e -> e.sym = sym && same_fixity e.fixity fixity) acc
         then
           Error (Printf.sprintf "operator %S is defined twice with the same fixity" sym)
         else add_syms (entry :: acc) fixity prec syms)
  in
  let rec add_groups acc = function
    | [] -> Ok (List.rev acc)
    | [] :: rest -> add_groups acc rest
    | [ fixity_word; prec_word ] :: _ ->
      Error (Printf.sprintf "group %S %S has no operators" fixity_word prec_word)
    | (fixity_word :: prec_word :: syms) :: rest ->
      (match fixity_of_word fixity_word with
       | None ->
         Error
           (Printf.sprintf "expected left, right, nonassoc or prefix, got %S" fixity_word)
       | Some fixity ->
         (match int_of_string_opt prec_word with
          | None ->
            Error
              (Printf.sprintf
                 "expected a precedence number after %S, got %S"
                 fixity_word
                 prec_word)
          | Some prec ->
            (match add_syms acc fixity prec syms with
             | Error _ as e -> e
             | Ok acc -> add_groups acc rest)))
    | [ word ] :: _ ->
      Error (Printf.sprintf "group %S has a fixity but no precedence" word)
  in
  add_groups [] (List.map words (String.split_on_char ';' s))
;;

let string_of t =
  let render (fixity, prec, syms) =
    Printf.sprintf
      "%s %d %s"
      (fixity_word fixity)
      prec
      (String.concat " " (List.rev syms))
  in
  let rec loop acc = function
    | [] -> acc
    | e :: rest ->
      let acc =
        match acc with
        | (fixity, prec, syms) :: acc when fixity = e.fixity && prec = e.prec ->
          (fixity, prec, e.sym :: syms) :: acc
        | _ -> (e.fixity, e.prec, [ e.sym ]) :: acc
      in
      loop acc rest
  in
  String.concat "; " (List.rev_map render (loop [] t))
;;

let pp ppf t = Format.pp_print_string ppf (string_of t)

let rec find_infix t tok =
  match t with
  | [] -> None
  | e :: rest ->
    (match e.fixity with
     | Infix _ when e.tok = tok -> Some e
     | _ -> find_infix rest tok)
;;

let rec find_prefix t tok =
  match t with
  | [] -> None
  | e :: rest ->
    (match e.fixity with
     | Prefix when e.tok = tok -> Some e
     | _ -> find_prefix rest tok)
;;
