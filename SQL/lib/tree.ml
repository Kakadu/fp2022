(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Type

exception Duplicate
exception NotFound

open Helper

(* the following is the type of a tree: 'a tree = | EmptyLeaf | Leaf of
   (int * 'a) | Node of (int * 'a * 'a tree * 'a tree) *)

let empty = EmptyLeaf

let is_empty = function
  | EmptyLeaf -> true
  | _ -> false
;;

let rec size = function
  | EmptyLeaf -> 0
  | Leaf _ -> 1
  | Node (_, _, l, r) -> 1 + size l + size r
;;

let rec get key = function
  | EmptyLeaf -> raise NotFound
  | Leaf (k, v) -> if k = key then v else raise NotFound
  | Node (k, v, l, r) -> if k = key then v else if k < key then get key r else get key l
;;

let rec insert key_value_pair tree =
  let key = fst key_value_pair in
  let value = snd key_value_pair in
  match tree with
  | EmptyLeaf -> Leaf (key, value)
  | Leaf (k, v) ->
    if k = key
    then raise Duplicate
    else (
      let new_leaf = Leaf (key, value) in
      if k < key
      then Node (k, v, EmptyLeaf, new_leaf)
      else Node (k, v, new_leaf, EmptyLeaf))
  | Node (k, v, l, r) ->
    if k = key
    then raise Duplicate
    else (
      let new_leaf = Leaf (key, value) in
      if k < key
      then
        if is_empty r
        then Node (k, v, l, new_leaf)
        else Node (k, v, l, insert (key, value) r)
      else if is_empty l
      then Node (k, v, new_leaf, r)
      else Node (k, v, insert (key, value) l, r))
;;

let rec update key new_value = function
  | EmptyLeaf -> EmptyLeaf
  | Leaf (k, v) ->
    if k = key then Leaf (k, new_value) else raise (Failure "key does not exist in tree")
  | Node (k, v, l, r) ->
    if k = key
    then Node (k, new_value, l, r)
    else if k > key
    then Node (k, v, update key new_value l, r)
    else Node (k, v, l, update key new_value r)
;;

let rec get_min = function
  | EmptyLeaf -> raise NotFound
  | Leaf (k, v) -> k, v
  | Node (_, _, l, _) -> get_min l
;;

let rec delete key = function
  | EmptyLeaf -> raise NotFound
  | Leaf (k, v) -> if k = key then EmptyLeaf else raise NotFound
  | Node (k, v, l, r) ->
    if k = key
    then
      if is_empty l
      then r
      else if is_empty r
      then l
      else (
        let new_k, new_v = get_min r in
        Node (new_k, new_v, l, delete new_k r))
    else if k < key
    then Node (k, v, l, delete key r)
    else Node (k, v, delete key l, r)
;;

let rec inorder = function
  | EmptyLeaf -> []
  | Leaf (k, v) -> [ k, v ]
  | Node (k, v, l, r) -> inorder l @ [ k, v ] @ inorder r
;;

let rec fold f init = function
  | EmptyLeaf -> init
  | Leaf (k, v) -> f init v init
  | Node (k, v, l, r) ->
    let l' = fold f init l in
    let r' = fold f init r in
    f l' v r'
;;

let rec map f = function
  | EmptyLeaf -> EmptyLeaf
  | Leaf (k, v) -> Leaf (k, f v)
  | Node (k, v, l, r) -> Node (k, f v, map f l, map f r)
;;

let filter_based_on_value f tree =
  let all_key_value_pairs = inorder tree in
  let rec filter_helper f = function
    | h :: t -> if f (snd h) then h :: filter_helper f t else filter_helper f t
    | _ -> []
  in
  let filtered_pairs = filter_helper f all_key_value_pairs in
  let rec generate_tree = function
    | h :: t -> insert h (generate_tree t)
    | _ -> EmptyLeaf
  in
  generate_tree filtered_pairs
;;

let get_key (f : 'a * int -> bool) tree =
  let key_val_assoc_list = inorder tree in
  let val_key_assoc_list = reverse_association_list key_val_assoc_list in
  let val_key_pair = List.filter f val_key_assoc_list in
  try snd (List.nth val_key_pair 0) with
  | Failure _ -> raise NotFound
  | Invalid_argument _ -> raise NotFound
;;

let get_key_col (f : column * int -> bool) tree =
  let key_val_assoc_list = inorder tree in
  let val_key_assoc_list = reverse_association_list key_val_assoc_list in
  let val_key_pair = List.filter f val_key_assoc_list in
  try snd (List.nth val_key_pair 0) with
  | Failure _ -> raise NotFound
  | Invalid_argument _ -> raise NotFound
;;

let filter_based_on_key f tree =
  let all_key_value_pairs = inorder tree in
  let rec filter_helper f = function
    | h :: t -> if f (fst h) then h :: filter_helper f t else filter_helper f t
    | _ -> []
  in
  let filtered_pairs = filter_helper f all_key_value_pairs in
  let rec generate_tree = function
    | h :: t -> insert h (generate_tree t)
    | _ -> EmptyLeaf
  in
  generate_tree filtered_pairs
;;

let rec generate_new_key = function
  | EmptyLeaf -> 0
  | Leaf (k, v) -> k + 1
  | Node (k, _, _, r) ->
    (match r with
     | EmptyLeaf -> k + 1
     | _ -> generate_new_key r)
;;

let update_key tree =
  let all_key_value_pairs = inorder tree in
  let rec generate_tree tree = function
    | h :: t ->
      insert (generate_new_key (generate_tree tree t), snd h) (generate_tree tree t)
    | _ -> EmptyLeaf
  in
  generate_tree empty all_key_value_pairs
;;

let rec to_value_list = function
  | EmptyLeaf -> []
  | Leaf (k, v) -> [ v ]
  | Node (k, v, l, r) -> to_value_list l @ [ v ] @ to_value_list r
;;
