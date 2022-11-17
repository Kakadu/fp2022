open Asm.Interpreter
open Asm.Parser

let fib =
  {|
   section .code
     fibbonach:
       mov rbx, 0x2A
       mov rax, 0x0
       mov rcx, 1
       cmp rbx, 1
       je fibbonachEnd
       cmp rbx, 2
       je fibbonachTwo
       sub rbx, 1
     fibbonachStart:
       sub rbx, 1
       xor rax, rcx
       xor rcx, rax
       xor rax, rcx
       add rax, rcx
       cmp rbx, 0
       je fibbonachEnd
       jmp fibbonachStart
     fibbonachTwo:
       mov rax, 1
     fibbonachEnd:
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
  let env = prep MapVar.empty r_list in
  let env = interpret env (eval fib) in
  match env with
  | Ok env -> print_string @@ show_envr env
  | Error msg -> failwith msg
