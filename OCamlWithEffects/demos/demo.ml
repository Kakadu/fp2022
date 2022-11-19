open OCamlWithEffectsLib.Printer

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  print_run code
;;