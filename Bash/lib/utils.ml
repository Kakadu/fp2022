(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: CC0-1.0 *)

let some_pred preds el = List.exists (fun fn -> fn el) preds
let all_pred preds el = List.for_all (fun fn -> fn el) preds

(* computes diff between l1 and l2 *)
let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

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
