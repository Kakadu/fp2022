open Asm.Interpreter
open Asm.Parser
open Int64

(* answer in RAX *)
(* let fib =
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
     |} *)

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

(* answer in xmm1*)
let scalar =
  {|
      section .data 
        a db 1, 2, 3
        b db 4, 5, 6
        c db 0xFF
      section .code 
        mov xmm0, a
        mov xmm1, b
        imul xmm0, xmm1
        xor xmm1, xmm1
        add xmm1, xmm0
        and xmm1, c
        shr xmm0, 8
        add xmm1, xmm0
        and xmm1, c
        shr xmm0, 8
        add xmm1, xmm0
        and xmm1, c
        shr xmm0, 8
      |}

let () =
  let open Interpret (Result) in
  let env = prep r_list in
  match eval scalar with
  | Parsed (Ast ast) -> (
      match interpret env [] ast with
      | Ok (env, s, _) ->
          List.iter print_endline (List.map to_string s);
          print_string @@ show_envr env
      | Error msg -> print_string msg)
  | Failed msg -> print_string msg
