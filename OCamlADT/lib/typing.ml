(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type named_id = int [@@deriving show { with_path = false }]
type name = string [@@deriving show { with_path = false }]
type adt_name = string [@@deriving show { with_path = false }]

type base_type =
  | Bool
  | Int
  | String
  | Nil
  | Unit
[@@deriving show { with_path = false }]

type t =
  | TypeVariable of named_id
  | BaseT of base_type
  | NamedT of name * t option
  | AdtT of (adt_name * t option) list
  | ArrowT of t * t
  | TupleT of t list
  | ListT of t
[@@deriving show { with_path = false }]

let number_t = BaseT Int
let bool_t = BaseT Bool
let string_t = BaseT String
let unit_t = BaseT Unit
let nil_t = BaseT Nil
let arrow_t left_type right_type = ArrowT (left_type, right_type)
let tuple_t type_list = TupleT type_list
let list_t t = ListT t
let adt_t ts = AdtT ts

type scheme = (int, Base.Int.comparator_witness) Base.Set.t * t
