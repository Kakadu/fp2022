(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Type

exception Malformed of string

let rec print_list to_string = function
  | [] -> ()
  | h :: t ->
    print_string (to_string h);
    print_string " ";
    print_list to_string t
;;

let rec print_list_newline to_string = function
  | [] -> ()
  | h :: t ->
    print_endline (to_string h);
    print_list_newline to_string t
;;

let key_value_pair_to_string to_string (k, v) =
  Printf.sprintf "key: %s, value: %s" (string_of_int k) (to_string v)
;;

let terminal_to_string (t : terminal) =
  match t with
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | String x -> x
;;

let rec find elem = function
  | [] -> raise (Malformed "command does not have the correct subcommand")
  | h :: t -> if h = elem then 0 else 1 + find elem t
;;

let rec sublist i j = function
  | [] -> raise (Malformed "empty list")
  | h :: t ->
    let tail = if j = 0 then [] else sublist (i - 1) (j - 1) t in
    if i > 0 then tail else h :: tail
;;

let get_this_command (tokens : token list) : token list =
  let eoq_index = find (EndOfQuery EOQ) tokens in
  sublist 0 (eoq_index - 1) tokens
;;

let get_list_after_where (tokens : token list) : token list =
  let where_index = find (SubCommand Where) tokens in
  sublist (where_index + 1) (List.length tokens - 1) tokens
;;

let get_other_commands (tokens : token list) : token list =
  let eoq_index = find (EndOfQuery EOQ) tokens in
  sublist (eoq_index + 1) (List.length tokens - 1) tokens
;;

let duplicate_in_list f lst = List.length lst <> List.length (List.sort_uniq f lst)

let rec function_to_list f init =
  match f init with
  | None -> []
  | Some (x, next) -> x :: function_to_list f next
;;

let range n =
  let in_range x = if x >= n then None else Some (x, x + 1) in
  function_to_list in_range 0
;;

let reverse_association_list lst = List.map (fun (a, b) -> b, a) lst

let extract = function
  | Some i -> i
  | None -> 0
;;

let max list =
  let max_helper list =
    let f max x =
      match max with
      | None -> Some x
      | Some m -> if m > x then Some m else Some x
    in
    List.fold_left f None list
  in
  extract (max_helper list)
;;
