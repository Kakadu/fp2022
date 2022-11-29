open Ast

type type_variable_number = int
type identifier = string

type ground_type =
  | Int
  | String
  | Char
  | Bool
  | Unit
[@@deriving eq, show { with_path = false }]

type typ =
  | TVar of type_variable_number
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TGround of ground_type

let rec pp_type fmt typ =
  let open Format in
  match typ with
  | TGround x ->
    (match x with
     | Int -> fprintf fmt "int"
     | String -> fprintf fmt "string"
     | Char -> fprintf fmt "char"
     | Bool -> fprintf fmt "bool"
     | Unit -> fprintf fmt "unit")
  | TTuple ts ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun _ _ -> printf " * ") (fun fmt ty -> pp_type fmt ty))
      ts
  | TList l -> fprintf fmt "%a list" pp_type l
  | TArr (t1, t2) ->
    let fmt : _ format =
      match t1 with
      | TArr _ -> "(%a) -> %a"
      | _ -> "%a -> %a"
    in
    printf fmt pp_type t1 pp_type t2
  | TVar var -> fprintf fmt "%s" @@ "'" ^ Char.escaped (Char.chr (var + 97))
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

let int_typ = TGround Int
let bool_typ = TGround Bool
let arrow l r = TArr (l, r)

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `Occurs_check
  | `NoVariable of identifier
  | `UnificationFailed of typ * typ
  ]

let rec pp_error fmt (err : error) =
  let open Format in
  match err with
  | `Occurs_check -> fprintf fmt "Occurs check failed.\n"
  | `NoVariable identifier -> fprintf fmt "%s" identifier
  | `UnificationFailed (t1, t2) ->
    pp_type fmt t1;
    fprintf fmt " ";
    pp_type fmt t2
;;

let print_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
 ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple typ_list -> Base.List.exists typ_list ~f:(occurs_in v)
    | TList typ -> occurs_in v typ
    | TGround _ -> false
  ;;

  let free_vars =
    let empty_set = Base.Set.empty (module Base.Int) in
    let rec helper acc = function
      | TVar a -> Base.Set.add acc a
      | TArr (a, b) -> helper (helper acc a) b
      | TTuple typ_list ->
        Base.List.fold_right
          typ_list
          ~f:(fun t s -> Base.Set.union s (helper empty_set t))
          ~init:acc
      | TList typ -> helper acc typ
      | TGround _ -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> typ -> t R.t

  (** Getting value from substitution *)
  val find_exn : fresh -> t -> typ

  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* an association list. In real world replace it by Map *)
  type t = (fresh, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return @@ Base.Map.update empty k ~f:(fun _ -> v)
  ;;

  let find_exn k subst = Base.Map.find_exn subst k
  let find k subst = Base.Map.find subst k
  let remove subst k = Base.Map.remove subst k

  let apply s =
    let rec helper = function
      | TVar b ->
        (match find_exn b s with
        | exception Base.Not_found_s _ -> TVar b
        | x -> x)
      | TArr (l, r) -> TArr (helper l, helper r)
      | TTuple typ_list -> TTuple (Base.List.map typ_list ~f:helper)
      | TList typ -> TList (helper typ)
      | ground -> ground
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TGround l, TGround r when l = r -> return empty
    | TGround _, TGround _ -> fail (`UnificationFailed (l, r))
    | TVar a, TVar b when a = b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArr (l1, r1), TArr (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple typ_list_l, TTuple typ_list_r ->
      (match Base.List.zip typ_list_l typ_list_r with
      | Base.List.Or_unequal_lengths.Unequal_lengths -> fail (`UnificationFailed (l, r))
      | Base.List.Or_unequal_lengths.Ok zipped_list ->
        Base.List.fold_right
          zipped_list
          ~f:(fun (x, y) subst ->
            let* head_sub = unify x y in
            let* subst in
            compose head_sub subst)
          ~init:(return empty))
    | TList typ1, TList typ2 -> unify typ1 typ2
    | _ -> fail (`UnificationFailed (l, r))

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return @@ Base.Map.update acc k ~f:(fun _ -> v))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  and compose_all ss =
    Base.List.fold_left ss ~init:(return empty) ~f:(fun acc subst ->
      let* acc in
      compose acc subst)
  ;;
end

module VarSet = struct
  let fold_right f ini set =
    Base.Set.fold_right set ~init:ini ~f:(fun x acc ->
      let open R.Syntax in
      let* acc in
      f acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | s, t -> (not (Base.Set.mem s v)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | s, t -> Base.Set.diff (Type.free_vars t) s
  ;;

  let apply sub (s, t) =
    let s2 = Base.Set.fold s ~init:sub ~f:(fun acc k -> Subst.remove acc k) in
    s, Subst.apply s2 t
  ;;
end

module TypeEnv = struct
  type t = (identifier, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env id scheme = Base.Map.update env id ~f:(fun _ -> scheme)
  let empty = Base.Map.empty (module Base.String)

  let free_vars : t -> (type_variable_number, Base.Int.comparator_witness) Base.Set.t =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.Int))
      ~f:(fun ~key:_ ~data acc -> Base.Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Base.Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Base.Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> typ R.t =
 fun (set, t) ->
  VarSet.fold_right
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return @@ Subst.apply s typ)
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env ty ->
  let free = Base.Set.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  free, ty
;;

let lookup_env e map =
  match Base.Map.find map e with
  | None -> fail (`NoVariable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let infer =
  let rec (helper : TypeEnv.t -> expression -> (Subst.t * typ) R.t) =
   fun env -> function
    | ELiteral literal ->
      (match literal with
       | LInt _ -> return (Subst.empty, TGround Int)
       | LString _ -> return (Subst.empty, TGround String)
       | LChar _ -> return (Subst.empty, TGround Char)
       | LBool _ -> return (Subst.empty, TGround Bool)
       | LUnit -> return (Subst.empty, TGround Unit))
    | EIdentifier identifier -> lookup_env identifier env
    | EFun (arguments, body) ->
      (match arguments with
       | [] -> helper env body
       | hd :: tl ->
         let* tv = fresh_var in
         let env2 = TypeEnv.extend env hd (Base.Set.empty (module Base.Int), tv) in
         let* s, ty = helper env2 (EFun (tl, body)) in
         let trez = arrow (Subst.apply s tv) ty in
         return (s, trez))
      return (Subst.empty, arrow int_typ (arrow int_typ int_typ))
    | Parsetree.EVar "=" -> return (Subst.empty, arrow int_typ (arrow int_typ bool_typ))
    | Parsetree.EVar x -> lookup_env x env
    | ELam (PVar x, e1) ->
      let* tv = fresh_var in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s, ty = helper env2 e1 in
      let trez = Arrow (Subst.apply s tv, ty) in
      return (s, trez)
    | EApp (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (Arrow (t2, tv)) in
      let trez = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, trez)
    | EConst _n -> return (Subst.empty, Prim "int")
    | Parsetree.EIf (c, th, el) ->
      let* s1, t1 = helper env c in
      let* s2, t2 = helper env th in
      let* s3, t3 = helper env el in
      let* s4 = unify t1 (Prim "bool") in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      R.return (final_subst, Subst.apply s5 t2)
    | Parsetree.ELet (NonRecursive, PVar x, e1, e2) ->
      let* s1, t1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t2 = generalize env2 t1 in
      let* s2, t3 = helper (TypeEnv.extend env2 (x, t2)) e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (Subst.(final_subst), t3)
    | Parsetree.ELet (Recursive, PVar x, e1, e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s1, t1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s = Subst.compose s2 s1 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2 = helper TypeEnv.(extend (apply s env) (x, t2)) e2 in
      let* final_subst = Subst.compose s s2 in
      return (final_subst, t2)
      return (final_subst, t2) *)
  in
  helper
;;

let w e = Result.map snd (run (infer TypeEnv.empty e))

let print_result e =
  match w e with
  | Ok typ -> print_typ typ
  | Error x -> print_error x
;;

let%expect_test _ =
  print_result (EFun ([ "x" ], EUnaryOperation (Not, EIdentifier "x")));
  [%expect {|
    bool -> int
  |}]
;;
