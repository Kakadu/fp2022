(** Copyright 2021-2022, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ListStack = struct
  type 'a t = 'a list

  let empty = []
  let push x s = x :: s

  let peek = function
    | [] -> None
    | h :: _ -> Some h
  ;;

  let pop = function
    | [] -> None
    | _ :: tl -> Some tl
  ;;
end

(* Stdlib.List with some custom utility functions *)
module List = struct
  include Stdlib.List

  (* Get index of an element in the 'a list.
   [equal] checks if two 'a elemets are equal *)
  let index_of_elem elem equal lst =
    let rec helper idx = function
      | [] -> None
      | h :: tl -> if equal h elem then Some idx else helper (idx + 1) tl
    in
    helper 0 lst
  ;;
end

module IntMap = struct
  include Map.Make (Int)

  (* Pretty printer for int IntMap *)
  let pp_int_intmap =
    let helper ppf m = iter (fun k v -> Format.fprintf ppf "%d -> %d@\n" k v) m in
    Format.printf "%a" helper
  ;;
end
