open Base

type ident =
  { name : string
  ; scope : int
  }
[@@deriving show, ord]

let error_ident = { scope = -404; name = "<ERROR>" }
let builtin name = { scope = -1; name }
let as_builtin id = if id.scope = -1 then Some id.name else None
let ident ~scope name = { name; scope }
let name id = id.name
let eq_ident x y = compare_ident x y = 0

(* Table *)

module Ident_comparator = struct
  type t = ident

  let compare (x : t) (y : t) = Caml.compare x y
  let sexp_of_t x = Sexp.Atom (x.name ^ "@" ^ Int.to_string x.scope)

  include (val Comparator.make ~compare ~sexp_of_t)
end

type 'v tbl = (ident, 'v, Ident_comparator.comparator_witness) Map.t

let empty_tbl = Map.empty (module Ident_comparator)
