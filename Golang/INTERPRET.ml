(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Golang_lib

let synopsis = "gocaml FILEPATH"

let help =
  Format.sprintf
    "gocaml -- Go compiler written in OCaml\nUsage: %s\n\nExample: gocaml main.go"
    synopsis
;;

let args =
  match Array.to_list Sys.argv with
  | [ _; inpath ] -> Some inpath
  | _ -> None
;;

let run_compiler inpath =
  match Interpret.interpret inpath with
  | Ok -> ()
  | FileNotFound -> print_endline (Format.sprintf "File %s not found!" inpath)
  | CE err ->
    let () = print_endline "Error!" in
    let () = print_endline err in
    ()
;;

let () =
  match args with
  | Some inpath -> run_compiler inpath
  | _ ->
    print_endline help;
    exit 1
;;
