(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open SQLyd_lib

let main () =
  let _ =
    ANSITerminal.print_string [ ANSITerminal.green ] "\n\nStart using the database.\n";
    print_endline "Please enter commands below.\n";
    print_string "> ";
    let rec recursive_parse db =
      try
        let line = read_line () in
        let current_db = Parser.parse db line in
        print_string "\n> ";
        recursive_parse current_db
      with
      | Parser.Malformed m ->
        print_endline m;
        print_string "\n> ";
        recursive_parse db
      | Parser.Empty ->
        print_endline "The command cannot be empty. \n";
        print_string "\n> ";
        recursive_parse db
    in
    recursive_parse (Rep.create_empty_database "parent")
  in
  ()
;;

let () = main ()
