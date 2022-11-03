(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type t = world list

(**
Finds info about variable.
Goes from head of world list to tail.
Returns (rev world list before variable * variable * world list with and after variable).
*)
val load_all_opt : name -> t -> (t * (name * (vtype * variable)) * t) option

(**
Finds info about variable.
Goes from head of world list to tail.
Returns (rev world list before variable * variable * world list with and after variable).
*)
val load_all : name -> t -> t * (name * (vtype * variable)) * t

(**
Finds info about variable.
Goes from head of world list to tail.
Returns the found variable.
*)
val load_opt : name -> t -> (vtype * variable) option

(**
Finds info about variable.
Goes from head of world list to tail.
Returns the found variable.
*)
val load : name -> t -> vtype * variable

val replace : name -> vtype * variable -> t -> t
val mem : name -> t -> bool

(**
Returns pair of (rev worlds after root word * root world)    
*)
val root : t -> t * t
