(** Copyright 2021-2022, andreyizrailev and contributors *)

(* Phantom types *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils

type byte
type word
type dword

(* Needed for pretty printing *)
val pp_byte : Format.formatter -> byte -> unit
val pp_word : Format.formatter -> word -> unit
val pp_dword : Format.formatter -> dword -> unit

(* Operand types *)
type 'a reg [@@deriving show { with_path = false }]
type 'a const [@@deriving show { with_path = false }]

(* Check integer size *)
val int_is_byte_const : int -> bool
val int_is_word_const : int -> bool
val int_is_dword_const : int -> bool

(* Converters *)
val int_to_byte_const : int -> byte const
val int_to_word_const : int -> word const
val int_to_dword_const : int -> dword const
val int_to_byte_reg : int -> byte reg
val int_to_word_reg : int -> word reg
val int_to_dword_reg : int -> dword reg
val byte_reg_name_list : string list
val word_reg_name_list : string list
val dword_reg_name_list : string list
val reg_name_to_byte_reg : string -> byte reg
val reg_name_to_word_reg : string -> word reg
val reg_name_to_dword_reg : string -> dword reg

(* Get internal register id by its name *)
val reg_name_to_id : string -> int

(* Get integer value of a constant *)
val const_val : 'a const -> int

(* These two functions require us to have ids of all dword registers as keys
     in the reg_map *)
val reg_val_get : 'a reg -> int IntMap.t -> int

(* If the value to set is too big, only the last bytes are considered.
     E.g. if we try to set "ah" to 0xABC, it will be set to 0xBC *)
val reg_val_set : 'a reg -> int -> int IntMap.t -> int IntMap.t
