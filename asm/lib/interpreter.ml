open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let ( >> ) x f = x >>= fun _ -> f
  let return = Result.ok
  let error = Result.error
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Interpret (M : MONADERROR) = struct
  open M

  module MapVar = struct
    include Map.Make (String)

    let pp pp_v ppf m =
      Format.fprintf ppf "@[[@[";
      iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) m;
      Format.fprintf ppf "@]]@]"
    ;;
  end

  type var =
    | Reg64 of int
    | Reg128 of string
    | Const of string
  [@@deriving show { with_path = false }]

  type envr = var MapVar.t [@@deriving show { with_path = false }]

  let r_list =
    [ (* "EFLAGS", Reg64 0 *)
      "CMPFLAG", Reg64 0
    ; "RAX", Reg64 0
    ; "RBX", Reg64 0
    ; "RCX", Reg64 0
    ; "RDX", Reg64 0
    ; "RSP", Reg64 0
    ; "RBP", Reg64 0
    ; "RSI", Reg64 0
    ; "RDI", Reg64 0
    ; "XMM0", Reg128 "0"
    ; "XMM1", Reg128 "0"
    ; "XMM2", Reg128 "0"
    ; "XMM3", Reg128 "0"
    ; "XMM4", Reg128 "0"
    ; "XMM5", Reg128 "0"
    ; "XMM6", Reg128 "0"
    ; "XMM7", Reg128 "0"
    ]
  ;;

  let is_64bitreg = function
    | "RAX" | "RBX" | "RCX" | "RDX" | "RSP" | "RBP" | "RSI" | "RDI" | "CMPFLAG" -> true
    | _ -> false
  ;;

  let prep env =
    let set k v env = MapVar.add k v env in
    let rec helper env = function
      | [] -> env
      | (k, v) :: tl -> helper (set k v env) tl
    in
    helper env
  ;;

  let rec ev f = function
    | Add (l, r) -> ev f l >>= fun l -> ev f r >>= fun r -> return (l + r)
    | Sub (l, r) -> ev f l >>= fun l -> ev f r >>= fun r -> return (l - r)
    | Mul (l, r) -> ev f l >>= fun l -> ev f r >>= fun r -> return (l * r)
    | Div (l, r) ->
      ev f r
      >>= fun r ->
      if r = 0 then error "Division by zero" else ev f l >>= fun l -> return (l / r)
    | const -> f const
  ;;

  let find_reg64_cont env name =
    return (MapVar.find name env)
    >>= function
    | Reg64 x -> return x
    | _ -> error "not a R64"
  ;;

  let change_reg64 env f = function
    | name when is_64bitreg name ->
      find_reg64_cont env name
      >>= fun reg -> return @@ MapVar.add name (Reg64 (f reg)) env
    | name -> error (name ^ " isnt a reg64")
  ;;

  let inter_one_args_cmd env arg1 =
    let helper fu = change_reg64 env fu arg1 in
    function
    | "INC" -> helper (fun x -> x + 1)
    | "DEC" -> helper (fun x -> x - 1)
    | "NOT" -> helper Int.neg
    | "NEG" -> helper (fun x -> -1 * x)
    (* | "PUSH" | "POP" | "CALL"  *)
    | x -> error (x ^ " is not implemented yet")
  ;;

  let inter_zero_args_cmd env = function
    | "RET" -> return env
    | "SYSCALL" | _ -> error "Not implemented yet"
  ;;

  let inter_two_args_cmd env arg1 arg2 cmd =
    let f = function
      | Ast.Const c -> return @@ int_of_string c
      | Ast.Reg reg_name -> find_reg64_cont env reg_name
      | _ -> error "Vars not implemented"
    in
    let helper fu = change_reg64 env fu arg1 in
    ev f arg2
    >>= fun arg2 ->
    match cmd with
    | "MOV" -> helper (fun _ -> arg2)
    | "ADD" -> helper (fun x -> x + arg2)
    | "SUB" -> helper (fun x -> x - arg2)
    | "IMUL" -> helper (fun x -> x * arg2)
    | "CMP" ->
      find_reg64_cont env arg1
      >>= fun arg1 -> change_reg64 env (fun _ -> if arg1 = arg2 then 1 else 0) "CMPFLAG"
    | "AND" -> helper (fun x -> Int.logand x arg2)
    | "XOR" -> helper (fun x -> Int.logxor x arg2)
    | "OR" -> helper (fun x -> Int.logor x arg2)
    | "SHL" | "SHR" | _ -> error "Not implemented yet"
  ;;

  let inter_cmd env = function
    | Args0 (Mnemonic cmd) -> inter_zero_args_cmd env cmd
    | Args1 (Mnemonic cmd, Ast.Reg x) -> inter_one_args_cmd env x cmd
    | Args2 (Mnemonic cmd, Ast.Reg x, y) -> inter_two_args_cmd env x y cmd
    | _ -> error "Isnt argsn"
  ;;

  let rec find_code_after_l (Label l) = function
    | [] -> error ("No such label: " ^ l)
    | Id (Label label) :: tl when label = l -> return tl
    | _ :: tl -> find_code_after_l (Label l) tl
  ;;

  let data_sec_inter env = function
    | _ -> return env
  ;;

  let rec code_sec_inter env ast =
    let jump env ast tl = function
      | Jmp (Mnemonic cmd, label) ->
        find_code_after_l label ast
        >>= fun code ->
        (match cmd with
        | "JMP" -> code_sec_inter env ast code
        | "JE" ->
          find_reg64_cont env "CMPFLAG"
          >>= fun flag ->
          if flag == 1 then code_sec_inter env ast code else code_sec_inter env ast tl
        | "JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" | _ -> error "")
      | _ -> error "Isnt jmp"
    in
    function
    | Command cmd :: tl ->
      (match cmd with
      | Args0 _ | Args1 _ | Args2 _ ->
        inter_cmd env cmd >>= fun env -> code_sec_inter env ast tl
      | Jmp _ -> jump env ast tl cmd)
    | Id _ :: tl -> code_sec_inter env ast tl
    | [] -> return env
  ;;

  let rec interpret env = function
    | [] -> return env
    | h :: tl ->
      (match h with
      | Code code -> code_sec_inter env code code >>= fun env -> interpret env tl
      | Data data -> data_sec_inter env data >>= fun env -> interpret env tl)
  ;;
end