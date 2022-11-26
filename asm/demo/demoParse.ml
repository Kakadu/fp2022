open Asm.Parser
open Asm.Ast

(* answer in RAX *)
let fib =
  {|
    section .code
      fibonachch:
        push rbp
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
        pop rbp
        ret
    |}

(* answer in RAX *)
(* let fac =
   {|
      section .text
        factorial:
          push rbp
          mov rax, 0xa
          mov rbx, rax
        factorialStart:
          sub rbx, 1
          cmp rbx, 0
          je factorialEnd
          imul rax, rbx
          jmp factorialStart
        factorialEnd:
          pop rbp
          ret
     |} *)

let () =
  match parse fib with
  | Ok ast -> print_endline @@ show_ast ast
  | Error msg -> print_endline msg
