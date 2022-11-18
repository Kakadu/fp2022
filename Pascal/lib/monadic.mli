(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val return : 'a -> 'b -> 'a * 'b
val swap : ('b -> 'a -> 'c * 'd) -> 'a -> 'b -> 'd * 'c
val ( >>= ) : ('a -> 'b * 'c) -> ('b -> 'c -> 'd) -> 'a -> 'd
val ( let* ) : ('a -> 'b * 'c) -> ('b -> 'c -> 'd) -> 'a -> 'd
val ( => ) : ('a -> 'b * 'c) -> ('b -> 'd) -> 'a -> 'd * 'c
val use : ('a -> 'b * 'c) -> 'a -> 'b
