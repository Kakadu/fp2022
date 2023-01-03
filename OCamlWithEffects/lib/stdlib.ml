(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let list =
  {|

let head list = match list with
  | [] -> None
  | head :: tail -> Some head

let tail list = match list with
  | [] -> None
  | head :: tail -> Some tail

let rec map f list = match list with
  | [] -> []
  | head :: tail -> f head :: map f tail

let reverse =
  let rec helper acc list = match list with
    | [] -> acc
    | head :: tail -> helper (head :: acc) tail
  in helper []

let rec fold_left f acc list = match list with 
  | [] -> acc
  | head :: tail -> fold_left f (f acc head) tail

let rec fold_right f ini list = match list with 
  | [] -> ini
  | head :: tail -> f head (fold_right f ini tail)

let rec take n list = match list with 
  | [] -> []
  | head :: tail -> if n <= 0 then [] else head :: take (n - 1) tail

let rec drop n list = if n <= 0 then list else
  (match list with 
  | [] -> []
  | head :: tail -> if n = 1 then tail else drop (n - 1) tail)

let rec at n list = match list with 
  | [] -> None
  | head :: tail -> if n <= 0 then Some head else at (n - 1) tail

let rec exists predicate list = match list with
  | [] -> false
  | head :: tail -> if predicate head then true else exists predicate tail

let rec find predicate list = match list with
  | [] -> None
  | head :: tail -> if predicate head then Some head else find predicate tail

let rec filter predicate list = match list with
  | [] -> []
  | head :: tail -> if predicate head then head :: (filter predicate tail) else filter predicate tail

let rec length list = match list with
  | [] -> 0
  | head :: tail -> 1 + length tail

let rec for_all predicate list = match list with
  | [] -> true
  | head :: tail -> if predicate head then for_all predicate tail else false

let rec for_any predicate list = match list with
  | [] -> false
  | head :: tail -> if predicate head then true else for_any predicate tail

let rec remove_last list = match list with
  | [] -> []
  | [head] -> []
  | head :: tail -> head :: remove_last tail

|}
;;

let integer =
  {|

let succ x = x + 1

let pred x = x - 1

let mod x y = x - y * (x / y)

let rec gcd x y = if x = 0 || y = 0 then x + y else gcd y (mod x y)

let lcm x y = (x * y) / (gcd x y)

|}
;;

let pair =
  {|

let fst pair = match pair with (x, _) -> x

let snd pair = match pair with (_, y) -> y

|}
;;

let queue =
  {|

let empty_queue = []

let enqueue queue k = k :: queue

let dequeue queue = remove_last queue

|}
;;

let effect =
  {|

  effect Get : int effect

  effect Set : int -> unit effect

  let get = perform Get

  let set value = perform (Set value)

  let run main =
    match main () with
      | effect Get -> continue 42
      | effect (Set y) -> continue ()
      | value -> value

|}
;;

let stdlib = [ list; integer; pair; queue; effect ]
