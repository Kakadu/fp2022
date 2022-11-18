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
