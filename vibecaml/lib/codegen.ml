[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
open Riscv

exception Unsupported of string

let error format =
  Printf.ksprintf (fun message -> raise (Unsupported ("Codegen: " ^ message))) format
;;

type function_symbol =
  { code : label
  ; closure : label
  ; arity : int
  }

type global =
  | Function of function_symbol
  | Value of label

type destination =
  | Result
  | Slot of int

let argument_registers = [ A0; A1; A2; A3; A4; A5; A6; A7 ]
let align16 bytes = (bytes + 15) / 16 * 16

let rec locals = function
  | ALet (name, rhs, body) -> name :: (action_locals rhs @ locals body)
  | ACExpr rhs -> action_locals rhs

and action_locals = function
  | CIf (_, yes, no) -> locals yes @ locals no
  | _ -> []
;;

let rec argument_space = function
  | ALet (_, rhs, body) -> max (action_space rhs) (argument_space body)
  | ACExpr rhs -> action_space rhs

and action_space = function
  | CApp (_, arguments) -> List.length arguments
  | CIf (_, yes, no) -> max (argument_space yes) (argument_space no)
  | _ -> 0
;;

let builtin_arity = function
  | "print_int" | "~-" -> 1
  | "+"
  | "-"
  | "*"
  | "/"
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr"
  | "="
  | "<>"
  | "<"
  | "<="
  | ">"
  | ">="
  | "&&"
  | "||" -> 2
  | name -> error "неподдержанное имя или операция %S" name
;;

(* Индекс делает отображение инъективным даже для a' и a_, которые после
   очистки имеют одинаковую читаемую часть. Префикс отделяет код от runtime. *)
let symbol kind index name =
  let readable =
    String.map
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
        | _ -> '_')
      name
  in
  label (Printf.sprintf "vc_%s_%s_%d" kind readable index)
;;

let generate program =
  try
    let bindings = List.concat_map snd program in
    let globals =
      List.mapi
        (fun index (name, parameters, _) ->
           let global =
             match parameters with
             | [] -> Value (symbol "value" index name)
             | _ ->
               Function
                 { code = symbol "fn" index name
                 ; closure = symbol "closure" index name
                 ; arity = List.length parameters
                 }
           in
           name, global)
        bindings
    in
    let names = List.map fst globals in
    if List.length names <> List.length (List.sort_uniq String.compare names)
    then error "повторное имя верхнего уровня: сначала нужен rename";
    (* Обратные списки позволяют выдавать инструкции за линейное время.
       Всё состояние принадлежит одному запуску, а не модулю в целом. *)
    let output = ref [] in
    let closures = ref [] in
    let wrappers = ref [] in
    let emit ?comm instruction = output := Instr (instruction, comm) :: !output in
    let mark name = output := Label name :: !output in
    let directive value = output := Directive value :: !output in
    let use_closure function_ =
      if not (List.mem function_ !closures) then closures := function_ :: !closures;
      function_.closure
    in
    let builtin name =
      match List.assoc_opt name !wrappers with
      | Some function_ -> function_
      | None ->
        let index = List.length !wrappers in
        let function_ =
          { code = symbol "builtin" index name
          ; closure = symbol "builtin_closure" index name
          ; arity = builtin_arity name
          }
        in
        wrappers := (name, function_) :: !wrappers;
        function_
    in
    let compile_function code parameters bodies =
      let local_names =
        parameters @ List.concat_map (fun (body, _) -> locals body) bodies
      in
      if
        List.length local_names <> List.length (List.sort_uniq String.compare local_names)
      then error "повторное локальное имя: нарушен контракт rename/CC";
      let slots = List.mapi (fun index name -> name, -24 - (8 * index)) local_names in
      let outgoing =
        List.fold_left (fun n (body, _) -> max n (argument_space body)) 0 bodies
      in
      let frame = align16 (16 + (8 * (List.length local_names + outgoing))) in
      (* И положительный addi эпилога, и все смещения должны влезть в imm12.
         Большие кадры отвергаем до передачи инструкций принтеру с assert. *)
      if frame > 2032 then error "кадр %d байт превышает поддерживаемые 2032" frame;
      let slot name =
        match List.assoc_opt name slots with
        | Some offset -> offset
        | None -> error "нет слота для %S" name
      in
      let load register = function
        | ImmNum n -> emit (Li (register, n))
        | ImmBool value -> emit (Li (register, if value then 1 else 0))
        | ImmUnit -> emit (Li (register, 0))
        | ImmId name ->
          (match List.assoc_opt name slots with
           | Some offset -> emit (Ld (register, offset, S0))
           | None ->
             (match List.assoc_opt name globals with
              | Some (Value address) ->
                emit (La (register, address));
                emit (Ld (register, 0, register))
              | Some (Function function_) -> emit (La (register, use_closure function_))
              | None -> emit (La (register, use_closure (builtin name)))))
      in
      let save = function
        | Result -> ()
        | Slot offset -> emit (Sd (A0, offset, S0))
      in
      let fail_divisor () =
        let safe = fresh_label () in
        emit (Bne (T1, Zero, safe));
        emit (Li (A0, 0));
        emit (Call (label "vc_fail"));
        mark safe
      in
      let inline name arguments =
        match name, arguments with
        | "print_int", [ value ] ->
          load A0 value;
          emit (Call (label "vc_print_int"));
          (* write возвращает число байтов, а функция ML должна вернуть unit. *)
          emit (Li (A0, 0))
        | "~-", [ value ] ->
          load T0 value;
          emit (Neg (A0, T0))
        | _, [ left; right ] ->
          load T0 left;
          load T1 right;
          (match name with
           | "+" -> emit (Add (A0, T0, T1))
           | "-" -> emit (Sub (A0, T0, T1))
           | "*" -> emit (Mul (A0, T0, T1))
           | "/" ->
             fail_divisor ();
             emit (Div (A0, T0, T1))
           | "mod" ->
             fail_divisor ();
             emit (Rem (A0, T0, T1))
           | "land" | "&&" -> emit (And (A0, T0, T1))
           | "lor" | "||" -> emit (Or (A0, T0, T1))
           | "lxor" -> emit (Xor (A0, T0, T1))
           | "lsl" -> emit (Sll (A0, T0, T1))
           | "lsr" -> emit (Srl (A0, T0, T1))
           | "asr" -> emit (Sra (A0, T0, T1))
           | "=" | "<>" ->
             emit (Xor (A0, T0, T1));
             emit (if name = "=" then Seqz (A0, A0) else Snez (A0, A0))
           | "<" -> emit (Slt (A0, T0, T1))
           | ">" -> emit (Slt (A0, T1, T0))
           | "<=" ->
             emit (Slt (A0, T1, T0));
             emit (Seqz (A0, A0))
           | ">=" ->
             emit (Slt (A0, T0, T1));
             emit (Seqz (A0, A0))
           | _ -> error "неподдержанная операция %S" name)
        | _ -> error "неверная арность операции %S" name
      in
      let rec expression destination = function
        | ALet (name, rhs, body) ->
          action (Slot (slot name)) rhs;
          expression destination body
        | ACExpr rhs -> action destination rhs
      and action destination = function
        | CIf (condition, yes, no) ->
          let otherwise = fresh_label () in
          let finish = fresh_label () in
          (match condition with
           | CondImm value ->
             load T0 value;
             emit (Beq (T0, Zero, otherwise))
           | CondCmp (operator, left, right) ->
             load T0 left;
             load T1 right;
             emit
               (match operator with
                | "=" -> Bne (T0, T1, otherwise)
                | "<>" -> Beq (T0, T1, otherwise)
                | "<" -> Bge (T0, T1, otherwise)
                | "<=" -> Blt (T1, T0, otherwise)
                | ">" -> Bge (T1, T0, otherwise)
                | ">=" -> Blt (T0, T1, otherwise)
                | _ -> error "неподдержанное условие %S" operator));
          (* Обе ветки пишут в одно место. Общий хвост не копируем. *)
          expression destination yes;
          emit (J finish);
          mark otherwise;
          expression destination no;
          mark finish
        | rhs ->
          (match rhs with
           | CImmExpr value -> load A0 value
           | CApp (function_, arguments) -> application function_ arguments
           | CTuple (first, second, rest) ->
             let fields = first :: second :: rest in
             let bytes = 8 * List.length fields in
             if bytes > 2047 then error "кортеж слишком велик для inline-аллокации";
             let enough = fresh_label () in
             emit (Mv (A0, S10));
             emit (Addi (T1, S10, bytes));
             emit (La (T2, label "vc_heap_end"));
             emit (Ld (T2, 0, T2));
             emit (Bge (T2, T1, enough));
             emit (Li (A0, 1));
             emit (Call (label "vc_fail"));
             mark enough;
             emit (Mv (S10, T1));
             List.iteri
               (fun index value ->
                  load T0 value;
                  emit (Sd (T0, 8 * index, A0)))
               fields
           | CField (value, index, size) ->
             if size < 2 || index < 0 || index >= size || index > 255
             then error "неверное поле %d кортежа размера %d" index size;
             load T0 value;
             emit (Ld (A0, 8 * index, T0))
           | CIf _ -> error "условие попало в обработчик простого действия");
          save destination
      and application function_ arguments =
        let known =
          match function_ with
          | ImmId name when not (List.mem_assoc name slots) -> List.assoc_opt name globals
          | _ -> None
        in
        let intrinsic =
          match function_ with
          | ImmId name
            when (not (List.mem_assoc name slots)) && not (List.mem_assoc name globals) ->
            Some name
          | ImmId _ -> None
          | _ -> error "применение константы как функции"
        in
        match known, intrinsic with
        | None, Some name when List.length arguments = builtin_arity name ->
          inline name arguments
        | Some (Function function_), _ when List.length arguments = function_.arity ->
          List.iteri
            (fun index value ->
               if index < 8
               then load (List.nth argument_registers index) value
               else (
                 load T0 value;
                 emit (Sd (T0, 8 * (index - 8), Sp))))
            arguments;
          emit (Call function_.code)
        | _ ->
          (* argv лежит в заранее выделенной нижней части кадра. Вызов
             может испортить t*/a*, но живые значения остаются в слотах. *)
          List.iteri
            (fun index value ->
               load T0 value;
               emit (Sd (T0, 8 * index, Sp)))
            arguments;
          load A0 function_;
          emit (Li (A1, List.length arguments));
          emit (Mv (A2, Sp));
          emit (Call (label "vc_apply"))
      in
      directive Text;
      directive (Globl code);
      mark code;
      emit ~comm:"кадр: ra, fp, локальные и исходящие аргументы" (Addi (Sp, Sp, -frame));
      emit (Sd (Ra, frame - 8, Sp));
      emit (Sd (S0, frame - 16, Sp));
      emit (Addi (S0, Sp, frame));
      List.iteri
        (fun index name ->
           if index < 8
           then emit (Sd (List.nth argument_registers index, slot name, S0))
           else (
             emit (Ld (T0, 8 * (index - 8), S0));
             emit (Sd (T0, slot name, S0))))
        parameters;
      List.iter
        (fun (body, global_result) ->
           expression Result body;
           match global_result with
           | None -> ()
           | Some address ->
             emit (La (T0, address));
             emit (Sd (A0, 0, T0)))
        bodies;
      (match parameters with
       | [] -> emit (Li (A0, 0))
       | _ -> ());
      (* Эпилог общий, и здесь нет TCO: это отдельное задание. *)
      emit (Ld (Ra, frame - 8, Sp));
      emit (Ld (S0, frame - 16, Sp));
      emit (Addi (Sp, Sp, frame));
      emit Ret
    in
    List.iter
      (fun (name, parameters, body) ->
         match List.assoc name globals with
         | Function function_ -> compile_function function_.code parameters [ body, None ]
         | Value _ -> ())
      bindings;
    (* Каждое значение вычисляется в порядке исходных объявлений. Хвост
       маленького ANF-блока сохраняет результат в его глобальный слот. *)
    let values =
      List.filter
        (function
          | _, [], _ -> true
          | _ -> false)
        bindings
    in
    let value_bodies =
      List.map
        (fun (name, _, body) ->
           match List.assoc name globals with
           | Value address -> body, Some address
           | Function _ -> error "ожидалось глобальное значение %S" name)
        values
    in
    compile_function (label "vc_main") [] value_bodies;
    List.iter
      (fun (name, function_) ->
         let parameters =
           List.init function_.arity (fun n -> "builtin_arg" ^ string_of_int n)
         in
         let body =
           ACExpr (CApp (ImmId name, List.map (fun name -> ImmId name) parameters))
         in
         compile_function function_.code parameters [ body, None ])
      (List.rev !wrappers);
    directive Data;
    directive (Align 3);
    List.iter
      (fun (_, global) ->
         match global with
         | Value address ->
           mark address;
           directive (Dword 0)
         | Function _ -> ())
      globals;
    List.iter
      (fun function_ ->
         mark function_.closure;
         directive (Dword_label function_.code);
         directive (Dword function_.arity);
         directive (Dword 0))
      (List.rev !closures);
    Ok (List.rev !output)
  with
  | Unsupported message -> Error message
;;
