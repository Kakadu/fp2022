open Prolog_lib
open Core
open Types
open Db

type opts = { mutable debug : bool }

let print_error err =
  let message =
    match err with
    | Critical x -> x
    | EvalError _ -> "false."
  in
  Caml.Format.printf "%s\n" message
;;

let rec try_read_and_continue get_next =
  try
    let user_input = input_line stdin in
    if Base.equal_string user_input "N"
    then get_next_variables get_next
    else Caml.Format.printf "\n"
  with
  | End_of_file -> ()

and get_next_variables get_next =
  match get_next () with
  | Ok (InterpretationResult (subs, get_next)) ->
    Caml.Format.printf ";\n";
    Print.print_substitution subs;
    (match get_next with
     | Some get_next ->
       Caml.Format.printf " ";
       try_read_and_continue get_next
     | None -> Caml.Format.printf ".\n")
  | Error _ -> Caml.Format.printf ".\n"

and run_query program_text query_text opts =
  let open Prolog_lib.Interpret in
  let module D = Db (Result) in
  let db = D.prepare program_text in
  match db with
  | Error er -> Caml.Format.printf "%s \n" (failure_to_string er)
  | Ok db ->
    let module I =
      Interpret
        (Result)
        (struct
          let debug = opts.debug
          let db = db
        end)
    in
    (match I.run query_text with
     | Ok (InterpretationResult (subs, get_next)) ->
       let no_substitutions = Utils.is_empty subs in
       if no_substitutions
       then Caml.Format.printf "true.\n"
       else (
         Print.print_substitution subs;
         match get_next with
         | Some get_next ->
           Caml.Format.printf " ";
           try_read_and_continue get_next
         | None -> Caml.Format.printf ".\n")
     | Error err -> print_error err)
;;

let rec run_next_query program_text opts =
  try
    let query_text = input_line stdin in
    run_query program_text query_text opts;
    run_next_query program_text opts
  with
  | End_of_file -> ()
;;

let () =
  let opts = { debug = false } in
  let input_file = ref "" in
  let open Caml.Arg in
  parse
    [ "--debug", Unit (fun () -> opts.debug <- true), "Enable debug mode" ]
    (fun filename -> input_file := filename)
    "./repl <file.pl> ";
  try
    let program_text = In_channel.read_all !input_file in
    run_next_query program_text opts
  with
  | Sys_error er ->
    Caml.Format.eprintf "Error: %s" er;
    Caml.exit 1
;;
