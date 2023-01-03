module type MonadError = sig
  type 'a t

  val return : 'a -> 'a t
  val error : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Result : MonadError with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let error = Result.error
  let ( >>= ) = Result.bind
end
