(** Copyright 2021-2023, andreyizrailev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ListStack = struct
  type 'a t = 'a list

  let empty = []
  let push x s = x :: s

  let peek = function
    | [] -> None
    | h :: _ -> Some h
  ;;

  (* You must be sure that the stack is not empty when calling the function *)
  let pop = function
    | [] -> None
    | _ :: tl -> Some tl
  ;;

  let pp pp_v ppf m = Format.(List.iter (fun k -> fprintf ppf "@[%a@] " pp_v k) m)
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

  let pp pp_v ppf m = Format.(iter (fun k -> fprintf ppf "@[%a@] " pp_v k) m)
end

module IntMap = struct
  include Map.Make (Int)

  (* Pretty printer for int IntMap *)
  let pp pp_v ppf m =
    Format.(iter (fun k v -> fprintf ppf "@[%a=%a@] " pp_print_int k pp_v v) m)
  ;;
end

module StringMap = struct
  include Map.Make (String)

  (* Pretty printer for int IntMap *)
  let pp pp_v ppf m =
    Format.(iter (fun k v -> fprintf ppf "@[%a=%a@] " pp_print_string k pp_v v) m)
  ;;
end
