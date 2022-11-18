(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type t = world list

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

val replace : name -> vtype * value -> t -> t
val mem : name -> t -> bool

(**
Returns pair of (rev worlds after root word * root world)    
*)
val root : t -> t * t

val load_fun_all_opt : name -> t -> (t * (vtype * value) * t) option

(**
Finds info about function in root world.
Returns the found function.
*)
val load_fun_opt : name -> t -> (vtype * value) option

(**
Finds info about function in root world.
Returns the found function.
*)
val load_fun : name -> t -> vtype * value

(**
Finds info about const function in root world.
Returns the found function.
*)
val load_const_fun_opt : name -> t -> (vtype * value) option

(**
Finds info about const function in root world.
Returns the found function.
*)
val load_const_fun : name -> t -> vtype * value
