(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ruby_lib

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let run_topaz (filename : string) : unit =
  read_whole_file filename |> Interpret.run_expr |> print_endline
;;

let help =
  String.concat
    ""
    [ "topaz -- Ruby interpreter written in OCaml\n"; "Usage:\n"; "topaz [FILENAME]" ]
;;

let arg =
  match Array.to_list Sys.argv with
  | [ _; inpath ] -> Some inpath
  | _ -> None
;;

let () =
  match arg with
  | Some filename -> run_topaz filename
  | _ ->
    print_endline help;
    exit 1
;;
