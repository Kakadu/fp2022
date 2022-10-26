(** Copyright 2021-2022, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type key_t = string [@@deriving show]

type field_t = {
  field_type : _type;
  field_key : key_t;
  is_const : bool;
  sub_tree : expr option;
}
[@@deriving show { with_path = false }]

type method_t = {
  method_mod : modifier list (* added for async *);
  method_type : _type;
  method_key : key_t;
  args : (_type * string) list;
  body : stmt;
}
[@@deriving show { with_path = false }]

module KeyMap = struct
  (* Delete formatter *)
  include Map.Make (String)

  let pp pp_v ppf map =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) map;
    Format.fprintf ppf "@]]@]"
end

type class_t = {
  class_key : key_t;
  field_map : field_t KeyMap.t;
  method_map : method_t KeyMap.t;
  parent_key : key_t option;
  dec_class : c_sharp_class;
}
[@@deriving show { with_path = false }]
