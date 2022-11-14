(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* ------------------------------------------------------ *)
(* ------------------- Helper functions ----------------- *)
(* ------------------------------------------------------ *)

(* ------------------------------------------------------ *)
(* -------------------- Parser tests -------------------- *)
(* ------------------------------------------------------ *)

open Parser

(* ----------------- Basic constructors ----------------- *)

let%test _ = parse integer "19" = Ok 19
let%test _ = parse integer "+19" = Ok 19
let%test _ = parse integer "-19" = Ok ~-19
let%test _ = parse identifier "abc" = Ok "abc"
let%test _ = parse identifier "ab1" = Ok "ab1"
let%test _ = parse identifier "_ab" = Ok "_ab"
