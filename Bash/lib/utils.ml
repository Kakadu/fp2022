(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: CC0-1.0 *)

let some_pred preds el = List.exists (fun fn -> fn el) preds
let all_pred preds el = List.for_all (fun fn -> fn el) preds

(* computes diff between l1 and l2 *)
let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

(* info about standard dile descriptors *)

(* standard file descriptors (for writing more readable code) *)
type std_fd =
  | StdIn
  | StdOut
  | StdErr

let fd_to_int = function
  | StdIn -> 0
  | StdOut -> 1
  | StdErr -> 2
;;

(* List of standart fds *)
let std_fds = [ Unix.stdin; Unix.stdout; Unix.stderr ]

(* structures *)

module IntMap = struct
  include Map.Make (Int)

  let from_list l = of_seq (List.to_seq (List.mapi (fun i el -> i, el) l))

  let pp pp_v ppf m =
    Format.(iter (fun k v -> fprintf ppf "@[%a=%a@] " pp_print_int k pp_v v) m)
  ;;
end

module StrMap = struct
  include Map.Make (String)

  let from_list l = of_seq (List.to_seq l)

  let pp pp_v ppf m =
    Format.(iter (fun k v -> fprintf ppf "@[%a=%a@] " pp_print_string k pp_v v) m)
  ;;
end
