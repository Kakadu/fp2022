open Ast

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type error_message = string

type environment = (id, value, id -> id -> bool) Base.Map.t

and value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VChar of char
  | VList of value list
  | VTuple of value list
  | VFun of expression * environment
  | VADT of data_constructor_name * value list

module Interpret (M : MONAD_FAIL) : sig
  val run : expression list -> ('a, error_message) M.t
end = struct
  let eval expression environment =
    match expression with
    | ELiteral literal ->
      (match literal with
       | LInt x -> VInt x
       | LString x -> VString x
       | LBool x -> VBool x
       | LChar x -> VChar x)
  ;;

  let run _ = M.fail "Not implemented"
end