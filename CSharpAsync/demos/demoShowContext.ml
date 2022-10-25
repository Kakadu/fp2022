open Csharp_lib.Interpreter_tests

let s = Stdio.In_channel.input_all stdin
let () = interpret s false
