(** Copyright 2021-2022, Chizhov Anton *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Angstrom

module type MONAD_FAIL = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <|> ) : 'a t -> (unit -> 'a t) -> 'a t
end

(* Checks if one of predicates is true *)
let some_pred preds el = List.exists (fun fn -> fn el) preds

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

(* computes product of pools *)
let product pools =
  let result = ref [ [] ] in
  List.iter
    (fun pool ->
      result
        := List.concat_map (fun y -> List.map (fun x -> List.append x [ y ]) !result) pool)
    pools;
  !result
;;

let%test _ =
  let input = [ [ "first" ]; [ "second" ] ] in
  product input = [ [ "first"; "second" ] ]
;;

(* computes diff between l1 and l2 *)
let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

(* structures *)

module IntMap = struct
  include Map.Make (Int)

  let from_list l = of_seq (List.to_seq (List.mapi (fun i el -> i, el) l))
  let add_list l = add_seq (List.to_seq l)

  let pp pp_v ppf m =
    Format.(iter (fun k v -> fprintf ppf "@[%a=%a@] " pp_print_int k pp_v v) m)
  ;;
end

module StrMap = struct
  include Map.Make (String)

  let from_list l = of_seq (List.to_seq l)
  let add_list l = add_seq (List.to_seq l)

  let pp pp_v ppf m =
    Format.(iter (fun k v -> fprintf ppf "@[%a=%a@] " pp_print_string k pp_v v) m)
  ;;
end
