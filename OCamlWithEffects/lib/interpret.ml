open Ast

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

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
