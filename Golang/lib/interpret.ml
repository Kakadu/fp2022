(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type read_result =
  | Ok of string
  | FileNotFound

let read_file path =
  let readline file =
    try Some (input_line file) with
    | End_of_file -> None
  in
  let rec readlines file acc =
    match readline file with
    | Some line -> readlines file (acc ^ line)
    | None -> acc
  in
  if Sys.file_exists path
  then (
    let file = open_in path in
    Ok (readlines file ""))
  else FileNotFound
;;

type interpret_result =
  | Ok
  | FileNotFound
  | CE of string

let add_true_false ast =
  let _true = Ast.GlobalVarDecl ("true", Const (Bool true)) in
  let _false = Ast.GlobalVarDecl ("false", Const (Bool false)) in
  _true :: _false :: ast
;;

let show_ast f =
  Ast.show_source_file
    (fun fmt id -> Ppx_deriving_runtime.Format.pp_print_string fmt (Ident.name id))
    f
;;

let normalize ast =
  let ( let* ) = Result.bind in
  let eval ast = Result.map_error (fun e -> [ e ]) (Eval.eval ast) in
  let result =
    let ast = add_true_false ast in
    let* ast = Lookup.lookup ast in
    let* _ = Typecheck.check ast in
    let* _ = Termination_check.check_file ast in
    let* _ = eval ast in
    Ok ast
  in
  match result with
  | Ok _ -> Ok
  | Error errs ->
    let errs =
      Base.List.fold_left errs ~init:"" ~f:(fun acc err ->
        String.concat "" [ acc; err; "\n" ])
    in
    CE errs
;;

let interpret inpath =
  match read_file inpath with
  | Ok s ->
    (match Parse.parse_file s with
     | Ok s ->
       let s = normalize s in
       s
     | Error r -> CE r)
  | FileNotFound -> FileNotFound
;;
