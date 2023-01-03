open Asm.Interpreter
open Asm.Parser
open Int64

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
  let open Interpret (Result) in
  let env = prep r_list in
  match eval fib with
  | Parsed (Ast ast) -> (
      match interpret env [] ast with
      | Ok (env, s, _) ->
          List.iter print_endline (List.map to_string s);
          print_string @@ show_envr env
      | Error msg -> print_string msg)
  | Failed msg -> print_string msg
