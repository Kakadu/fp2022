open Parser

exception EmptyInputStringError of string
exception GetPathError of string
exception UnknownCommand of string

let read_all_file_text file_path =
  Stdio.In_channel.input_all (Unix.in_channel_of_descr file_path)
;;

let try_read_file_text file_path =
  try read_all_file_text file_path with
  | Unix.Unix_error (Unix.EBADF, "read", "") -> ""
;;

let read_command () =
  try read_line () with
  | End_of_file -> ""
;;

let split_string_and_delete_spaces command =
  List.filter
    (fun x -> not (String.equal x " " || String.equal x ""))
    (String.split_on_char ' ' command)
;;

let command_list command =
  let command_args = split_string_and_delete_spaces command in
  match command_args with
  | h :: tl when h = "menhir" ->
    if List.length tl = 0
    then (
      let () = print_endline "ATTENTION: No flags in your command" in
      [])
    else (
      let rec interpret_flags flags =
        match flags with
        | h' :: tl' ->
          (match h' with
           | "--interpret" -> ("switch", "--interpret") :: interpret_flags tl'
           | s ->
             (try
                ("text", try_read_file_text (Unix.openfile s [] 0)) :: interpret_flags tl'
              with
              | _ -> raise (UnknownCommand s)))
        | _ -> []
      in
      interpret_flags tl)
  | [ s ] -> raise (UnknownCommand s)
  | _ -> raise (UnknownCommand "")
;;

exception NoFile of string

let get_parser_and_tree_parser command =
  try
    let _, text =
      List.find
        (fun x ->
          let l, _ = x in
          if l = "text" then true else false)
        (command_list command)
    in
    Ok (genParser text, genTreeParser text)
  with
  | Not_found -> Error "ATTENTION: No path in your command"
  | UnknownCommand s -> Error ("Unknown command, switch or bad path: " ^ s)
;;
