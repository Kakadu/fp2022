(** Copyright 2021-2022, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Interpreter_tests

let s = Stdio.In_channel.input_all stdin
let () = interpret s false
