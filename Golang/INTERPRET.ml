open Golang

let synopsis = "gocaml FILEPATH"

let help =
  "gocaml -- Go compiler written in OCaml\n"
  ^ "Usage: "
  ^ synopsis
  ^ "\n\n"
  ^ "Example: gocaml main.go"
;;

let args =
  match Array.to_list Sys.argv with
  | [ _; inpath ] -> Some inpath
  | _ -> None
;;

let run_compiler inpath =
  match Interpret.interpret inpath with
  | Ok -> ()
  | FileNotFound -> print_endline ("File " ^ inpath ^ " not found!")
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
