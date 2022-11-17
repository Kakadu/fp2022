open Asm.Parser
open Asm.Ast

let fib =
  "section .code\n\
  \    fibbonach:\n\
  \      mov rbx, 0x2A\n\
  \      mov rax, 0x0\n\
  \      mov rcx, 1\n\
  \      cmp rbx, 1\n\
  \      je fibbonachEnd\n\
  \      cmp rbx, 2\n\
  \      je fibbonachTwo\n\
  \      sub rbx, 1\n\
  \    fibbonachStart:\n\
  \      sub rbx, 1\n\
  \      xor rax, rcx\n\
  \      xor rcx, rax\n\
  \      xor rax, rcx\n\
  \      add rax, rcx \n\
  \      cmp rbx, 0\n\
  \      je fibbonachEnd\n\
  \      jmp fibbonachStart\n\
  \    fibbonachTwo:\n\
  \      mov rax, 1\n\
  \    fibbonachEnd:\n\
  \      ret"

(* let fac =
     "section .text\n\
     \   factorial:\n\
     \      mov rax, 0xa\n\
     \      mov rbx, rax\n\
     \   factorialStart:\n\
     \      dec rbx\n\
     \      cmp rbx, 0\n\
     \      je factorialEnd\n\
     \      imul rax, rbx\n\
     \      jmp factorialStart\n\
     \   factorialEnd:\n\
     \        ret"
   ;; *)

let () =
  let rec pr_p = function
    | [] -> ()
    | h :: tl ->
        print_string @@ show_ast h;
        print_string "\n";
        pr_p tl
  in
  match parse fib with Ok ast -> pr_p ast | Error msg -> print_endline msg
