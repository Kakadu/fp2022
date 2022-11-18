(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t =
  | Created of 'a Array.t
  | Empty of int * 'a

let use f = function
  | Created a -> f a
  | Empty (size, value) -> f (Array.make size value)
;;

let length = function
  | Created a -> Array.length a
  | Empty (s, _) -> s
;;

let get t ind =
  match t with
  | Created a -> Array.get a ind
  | Empty (_, value) -> value
;;

let set t i v =
  let n = use Array.copy t in
  n.(i) <- v;
  Created n
;;

let make size value = Empty (size, value)

let to_list = function
  | Created a -> Array.to_list a
  | Empty (size, value) -> List.init size (fun _ -> value)
;;

let of_list l = Created (Array.of_list l)
let map f t = Created (use (Array.map f) t)
let fold_left f s t = use (Array.fold_left f s) t
let fold_right f t s = use (Array.fold_right f) t s

let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
 fun pp_v ppf arr ->
  Format.fprintf ppf "@[[@[";
  List.iter (fun v -> Format.fprintf ppf "%a; " pp_v v) (to_list arr);
  Format.fprintf ppf "@]]@]"
;;
