open Interpret.InterpretResult
open Interpret
open Parser
open Format

let rec print result =
  let rec print_list delimiter = function
    | [ head ] ->
      print head;
      printf "%c " delimiter
    | head :: tail ->
      print head;
      printf "%c " delimiter;
      print_list delimiter tail
    | [] -> ()
  in
  match result with
  | VInt value -> printf "%d" value
  | VChar value -> printf "%C" value
  | VBool value -> printf "%B" value
  | VString value -> printf "%S" value
  | VUnit -> printf "()"
  | VList list ->
    printf "[";
    print_list ';' list;
    printf "]"
  | VTuple tuple ->
    printf "(";
    print_list ',' tuple;
    printf ")"
  | VADT (name, arguments_list) ->
    printf "%s " name;
    List.iter
      (fun elem ->
        print elem;
        printf " ")
      arguments_list
  | VFun _ -> printf "Not a value."
;;

let print_run code =
  match parse code with
  | Ok ast ->
    (match run ast with
     | Ok result -> print result
     | Error error -> printf "%s" error)
  | Error error ->
    printf "%s" error;
    printf "\n"
;;
