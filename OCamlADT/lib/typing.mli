(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type named_id = int [@@deriving show { with_path = false }]
type name = string [@@deriving show { with_path = false }]
type adt_name = string [@@deriving show { with_path = false }]

(** Base type *)
type base_type =
  | Bool
  | Int
  | String
  | Nil
  | Unit
[@@deriving show { with_path = false }]

(* General type *)
type t =
  | TypeVariable of named_id (** For type variables *)
  | BaseT of base_type (** Base types: int, string *)
  | NamedT of name * t option (** Named: color, list, smth *)
  | AdtT of (adt_name * t option) list (** ADT: One | Two of int | Three *)
  | ArrowT of t * t (** Arguments taking: t -> t *)
  | TupleT of t list (** Tuple types: t * t *)
  | ListT of t (** List type: int list *)
[@@deriving show { with_path = false }]

val number_t : t
val bool_t : t
val string_t : t
val unit_t : t
val nil_t : t
val arrow_t : t -> t -> t
val tuple_t : t list -> t
val list_t : t -> t
val adt_t : (adt_name * t option) list -> t

type scheme = (int, Base.Int.comparator_witness) Base.Set.t * t
