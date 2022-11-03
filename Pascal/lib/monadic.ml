(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let return a tl = a, tl

let swap f x y =
  let y, x = f y x in
  x, y
;;

let ( >>= ) f1 f2 arg =
  let h, tl = f1 arg in
  f2 h tl
;;

let ( let* ) = ( >>= )

let ( => ) f1 f2 =
  let* r = f1 in
  return (f2 r)
;;

let use p w =
  let r, _ = p w in
  r
;;
