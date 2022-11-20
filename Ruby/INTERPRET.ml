open Ruby_lib

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let run_topaz (filename : string) : unit =
  let _ = read_whole_file filename |> Parser.parse |> Interpret.run |> Utils.string_of_value in ()
;;

let help =
  "topaz -- Ruby interpreter written in OCaml\n" ^ "Usage:\n" ^ "topaz [FILENAME]"
;;

let arg =
  match Array.to_list Sys.argv with
  | [ _; inpath ] -> Some inpath
  | _ -> None
;;

let () =
  match arg with
  | Some filename ->  run_topaz filename
  | _ ->
    print_endline help;
    exit 1
;;
