open Interpret.InterpretResult
open Interpret
open Inferencer
open Parser
open Format

let rec print result =
  let rec print_list delimiter = function
    | [ head ] -> print head
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
  | VADT (name, argument) ->
    printf "%s " name;
    (match argument with
     | Some argument -> print argument
     | None -> ())
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

let print_typ code =
  match parse code with
  | Ok ast ->
    List.iter
      (fun elem ->
        print_result elem;
        printf "\n")
      ast
  | Error error ->
    printf "%s" error;
    printf "\n"
;;