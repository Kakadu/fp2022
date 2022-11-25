open Asm.Interpreter
open Asm.Parser

let fib =
  {|
   section .code
     fibonachch:
       mov rbx, 0x2A
       mov rax, 0x0
       mov rcx, 1
       cmp rbx, 1
       je fibonachchEnd
       cmp rbx, 2
       je fibonachchTwo
       sub rbx, 1
     fibonachchStart:
       sub rbx, 1
       xor rax, rcx
       xor rcx, rax
       xor rax, rcx
       add rax, rcx
       cmp rbx, 0
       je fibonachchEnd
       jmp fibonachchStart
     fibonachchTwo:
       mov rax, 1
     fibonachchEnd:
       ret
   |}

(* let fac =
   {|
     section .text
       factorial:
         mov rax, 0xa
         mov rbx, rax
       factorialStart:
         dec rbx
         cmp rbx, 0
         je factorialEnd
         imul rax, rbx
         jmp factorialStart
       factorialEnd:
         ret
   |} *)

let () =
  let open Interpret (Result) in
  let env = prep r_list in
  match eval fib with
  | Parsed ast -> (
      match interpret env ast with
      | Ok env -> print_string @@ show_envr env
      | Error msg -> print_string msg)
  | Failed msg -> print_string msg
