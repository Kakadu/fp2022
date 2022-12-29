open Ast
open Types

module Interpret (M : MonadFail) (C : Config) : sig
  val backtrack : choicepoints -> (unifier * choicepoints) result
  val run : string -> interp_res result
  val eval_builtin : term -> unifier -> choicepoints -> (unifier * choicepoints) result
end
