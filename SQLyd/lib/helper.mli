(** Copyright 2022-2023, Danil Yevdokimov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Helper methods for other modules to use *)

open Type

(* Prints a list of elements, separated by spaces *)
val print_list : ('a -> string) -> 'a list -> unit

(* Prints a list of elements, separated by newlines *)
val print_list_newline : ('a -> string) -> 'a list -> unit

(* Convert a key-value pair to a string *)
val key_value_pair_to_string : ('a -> string) -> int * 'a -> string

(* [get_list_after_where tokens] return the sublist of tokens after the
    where keyword *)
val get_list_after_where : token list -> token list

(* [get_this_command tokens] return the sublist up to everything before
    the first EOQ *)
val get_this_command : token list -> token list

(* [get_other_commands tokens] return the sublist of everything after
    EOQ, to pass into parse_query *)
val get_other_commands : token list -> token list

(* Check if a list contains duplicates *)
val duplicate_in_list : ('a -> 'a -> int) -> 'a list -> bool

(* [range n] generates a list of integer [0, 1,  (n-)] *)
val range : int -> int list

(* [reverse_association lst] reverse the (k, v) pair in association
    list *)
val reverse_association_list : (int * 'a) list -> ('a * int) list

(* [max lst] return the largest element in [lst] *)
val max : int list -> int
