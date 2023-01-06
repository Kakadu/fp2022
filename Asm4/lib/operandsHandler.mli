(** Copyright 2021-2023, andreyizrailev and contributors *)

(* Phantom types *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open MonadError

module OperandsHandler (M : MonadError) : sig
  open M

  type byte
  type word
  type dword
  type xmm

  (* Needed for pretty printing *)
  val pp_byte : Format.formatter -> byte -> unit
  val pp_word : Format.formatter -> word -> unit
  val pp_dword : Format.formatter -> dword -> unit
  val pp_xmm : Format.formatter -> xmm -> unit

  (* Operand types *)
  type 'a reg [@@deriving show { with_path = false }]
  type 'a const [@@deriving show { with_path = false }]

  (* Check integer size *)
  val int_is_byte_const : int -> bool
  val int_is_word_const : int -> bool
  val int_is_dword_const : int -> bool

  (* Converters *)
  val int_to_byte_const : int -> byte const t
  val int_to_word_const : int -> word const t
  val int_to_dword_const : int -> dword const t
  val int_to_byte_reg : int -> byte reg t
  val int_to_word_reg : int -> word reg t
  val int_to_dword_reg : int -> dword reg t
  val byte_reg_name_list : string list
  val word_reg_name_list : string list
  val dword_reg_name_list : string list
  val xmm_reg_name_list : string list
  val reg_name_to_byte_reg : string -> byte reg t
  val reg_name_to_word_reg : string -> word reg t
  val reg_name_to_dword_reg : string -> dword reg t
  val reg_name_to_xmm_reg : string -> xmm reg t

  (* Get internal register id by its name *)
  val reg_name_to_id : string -> int t

  (* Get register id *)
  val reg_to_id : 'a reg -> int

  (* Get integer value of a constant *)
  val const_val : 'a const -> int

  (* Construct a constant from value *)
  val const_from_val : int -> 'a const

  (* eax register *)
  val reg_eax : dword reg

  (* Id of eax register *)
  val reg_eax_id : int

  (* Get list of related registers, e.g. for ah we get
     [eax, ax, ah, al] *)
  val reg_get_related_regs : 'a reg -> int list

  (* These two functions require us to have ids of all dword registers as keys
     in the reg_map *)
  val reg_val_get : 'a reg -> int IntMap.t -> int t

  (* If the value to set is too big, only the last bytes are considered.
     E.g. if we try to set "ah" to 0xABC, it will be set to 0xBC *)
  val reg_val_set : 'a reg -> int -> int IntMap.t -> int IntMap.t t

  (* Special case of get and set for xmm registers since their values are int lists,
   so they require a separate reg_map *)
  val xmm_reg_val_get : xmm reg -> int list IntMap.t -> int list t
  val xmm_reg_val_set : xmm reg -> int list -> int list IntMap.t -> int list IntMap.t t
end
