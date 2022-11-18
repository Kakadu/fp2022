(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

include Map.Make (String)

let pp pp_v ppf map =
  Format.fprintf ppf "@[[@[";
  iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@];@\n" k pp_v v) map;
  Format.fprintf ppf "@]]@]"
;;
