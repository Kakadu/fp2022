(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Convert string to char array *)
let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let implode x = String.of_seq (List.to_seq x)

(* isChar and isDigit *)
let isChar x =
  (Char.code x >= 65 && Char.code x <= 90) || (Char.code x >= 97 && Char.code x <= 122)
;;

let isDigit x = Char.code x >= 48 && Char.code x <= 57
