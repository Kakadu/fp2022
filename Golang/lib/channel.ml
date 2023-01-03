(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Event

type 'v t = { chan : 'v channel }

let create () = { chan = new_channel () }

let send { chan } v =
  let event = send chan v in
  sync event
;;

let receive { chan } =
  let event = receive chan in
  sync event
;;
