open Asm.Parser
open Asm.Ast

let rec pr_p = function
  | [] -> ()
  | h :: tl ->
    print_string @@ show_code_section h;
    print_string "\n";
    pr_p tl
;;

let code =
  "mov     rax,0x001\n\
  \       mov     rdi,rax\n\
  \      mov     rsi, 43 + 42 + 41 + 40 + 29\n\
  \       mov     rdx,14"
;;

let () = pr_p (eval code)
