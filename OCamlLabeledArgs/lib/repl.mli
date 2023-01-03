(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Read-eval-print-loop for our mini-language. *)

(** [run_repl] is the main entry to the line-by-line REPL *)
val run_repl : bool -> unit -> unit

(** [run_single] is the main entry to the single-run REPL *)
val run_single : bool -> unit
