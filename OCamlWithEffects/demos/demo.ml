(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlWithEffectsLib.Printer

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  print_run code
;;
