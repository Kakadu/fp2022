(** Copyright 2021-2022, Kakadu, Furetur and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'v t =
  { data : 'v option ref
  ; m : Mutex.t
  ; notfull : Condition.t
  ; notempty : Condition.t
  }

let create () =
  { data = ref None
  ; m = Mutex.create ()
  ; notfull = Condition.create ()
  ; notempty = Condition.create ()
  }
;;

let send chan v =
  Mutex.lock chan.m;
  while Option.is_some !(chan.data) do
    Condition.wait chan.notfull chan.m
  done;
  chan.data := Some v;
  Condition.signal chan.notempty;
  Mutex.unlock chan.m
;;

let receive chan =
  Mutex.lock chan.m;
  while Option.is_none !(chan.data) do
    Condition.wait chan.notempty chan.m
  done;
  match !(chan.data) with
  | Some x ->
    chan.data := None;
    Condition.signal chan.notfull;
    Mutex.unlock chan.m;
    x
  | None ->
    Mutex.unlock chan.m;
    failwith "Internal error"
;;
