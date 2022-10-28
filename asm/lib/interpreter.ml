open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Interpret (M : MONADERROR) = struct
  open M

  let rec eval expr f = function
    | Add (l, r) -> eval expr f l >>= fun l -> eval expr f r >>= fun r -> return (l + r)
    | Sub (l, r) -> eval expr f l >>= fun l -> eval expr f r >>= fun r -> return (l - r)
    | Mul (l, r) -> eval expr f l >>= fun l -> eval expr f r >>= fun r -> return (l * r)
    | Div (l, r) ->
      eval expr f r
      >>= fun r ->
      if r = 0 then error "Division by zero" else eval expr f l >>= fun l -> return (l / r)
    | const -> f const
  ;;
end