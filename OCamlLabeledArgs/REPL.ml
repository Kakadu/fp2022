(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlLabeledArgs_lib.Repl

type opts =
  { mutable batch : bool
  ; mutable debug_build : bool
  }

let () =
  let opts = { batch = false; debug_build = true } in
  let open Caml.Arg in
  parse
    [ ( "-"
      , Unit (fun () -> opts.batch <- true)
      , "Read from stdin single program, instead of running full REPL" )
    ; "-debug", Unit (fun () -> opts.debug_build <- false), "Debug build"
    ]
    (fun _ ->
      Caml.Format.eprintf "Positioned arguments are not supported\n";
      Caml.exit 1)
    "Read-Eval-Print-Loop for the subset of OCaml language with labeled arguments";
  if opts.batch then run_single opts.debug_build else run_repl opts.debug_build ()
;;
