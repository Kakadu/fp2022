(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* binary search tree data sturcture *)

open Type

exception Duplicate
exception NotFound

val get : int -> 'a tree -> 'a
val size : 'a tree -> int
val insert : int * 'a -> 'a tree -> 'a tree
val delete : int -> 'a tree -> 'a tree
val update : int -> 'a -> 'a tree -> 'a tree

(* returns a list of the values in the tree, in order *)
val inorder : 'a tree -> (int * 'a) list
val empty : 'a tree

(* bottom-up fold on tree *)
val fold : ('a -> 'b -> 'a -> 'a) -> 'a -> 'b tree -> 'a
val map : ('a -> 'b) -> 'a tree -> 'b tree
val filter_based_on_value : ('a -> bool) -> 'a tree -> 'a tree
val filter_based_on_key : (int -> bool) -> 'a tree -> 'a tree
val generate_new_key : 'a tree -> int
val get_key : ('a * int -> bool) -> 'a tree -> int
val get_key_col : (column * int -> bool) -> column tree -> int
val update_key : 'a tree -> 'a tree

(* create a list containing values, sorted in ascending order *)
val to_value_list : 'a tree -> 'a list
