(** Copyright 2021-2022, Kakadu, EmirVildanov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Type inference (unification).
   For better understanding see:
   * Kakadu implementation of miniml langauge inferencer
   [here](https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml)
   * Danila Pechenev & Ilya Dudnikov implementation of OCaml inferencer
   [here](https://github.com/Danila-Pechenev/fp2022/blob/master/OCamlWithEffects/lib/inferencer.ml) *)

open Ast
open Typing

type identifier = string

type error =
  [ `Occurs_check
  | `NoVariable of identifier
  | `UnificationFailed of Typing.t * Typing.t
  | `NotReachable
  | `NotImplementedYet
  ]

type scheme = (named_id, Base.Int.comparator_witness) Base.Set.t * Typing.t

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
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = Typing.t

  let rec occurs_in v =
    let occurs_in_variant variant =
      let _, t = variant in
      match t with
      | None -> false
      | Some t -> occurs_in v t
    in
    function
    | TypeVariable id -> id = v
    | NamedT (_, _) -> false
    | ArrowT (left, right) -> occurs_in v left || occurs_in v right
    | TupleT ts -> Base.List.exists ts ~f:(occurs_in v)
    | ListT t -> occurs_in v t
    | AdtT vs -> Base.List.exists vs ~f:occurs_in_variant
    | BaseT _ -> false
  ;;

  let free_vars =
    let empty_set = Base.Set.empty (module Base.Int) in
    let rec helper acc =
      let free_vars_variant set variant =
        let _, t = variant in
        match t with
        | None -> set
        | Some t -> helper set t
      in
      function
      | TypeVariable id -> Base.Set.add acc id
      | ArrowT (left, right) -> helper (helper acc left) right
      | TupleT ts ->
        Base.List.fold_right
          ts
          ~f:(fun t s -> Base.Set.union s (helper empty_set t))
          ~init:acc
      | ListT t -> helper acc t
      | BaseT _ -> acc
      | AdtT vs ->
        Base.List.fold_right
          vs
          ~f:(fun v s -> Base.Set.union s (free_vars_variant empty_set v))
          ~init:acc
      | NamedT (_, _) -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> Typing.t -> t R.t

  (** Getting value from substitution *)
  val find_exn : fresh -> t -> Typing.t

  val find : fresh -> t -> Typing.t option
  val apply : t -> Typing.t -> Typing.t
  val unify : Typing.t -> Typing.t -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  type t = (fresh, Typing.t, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let mapping key value =
    if Type.occurs_in key value then fail `Occurs_check else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return (Base.Map.update empty key ~f:(fun _ -> value))
  ;;

  let find_exn key subst = Base.Map.find_exn subst key
  let find key subst = Base.Map.find subst key
  let remove subst key = Base.Map.remove subst key

  let apply s =
    let rec helper =
      let apply_for_adt_variant v =
        let name, op = v in
        match op with
        | None -> name, None
        | Some t -> name, Some (helper t)
      in
      function
      | TypeVariable id ->
        (match find_exn id s with
         | exception Base.Not_found_s _ -> TypeVariable id
         | x -> x)
      | ArrowT (left, right) -> ArrowT (helper left, helper right)
      | TupleT ts -> TupleT (Base.List.map ts ~f:helper)
      | ListT t -> ListT (helper t)
      | BaseT t -> BaseT t
      | AdtT vs -> AdtT (Base.List.map vs ~f:apply_for_adt_variant)
      | NamedT (name, t) -> NamedT (name, t)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | BaseT left, BaseT right when left = right -> return empty
    | TypeVariable a, TypeVariable b when a = b -> return empty
    | TypeVariable b, t | t, TypeVariable b -> singleton b t
    | ArrowT (left1, right1), ArrowT (left2, right2) ->
      let* subs1 = unify left1 left2 in
      let* subs2 = unify (apply subs1 right1) (apply subs1 right2) in
      compose subs1 subs2
    | TupleT tsl, TupleT tsr ->
      (match Base.List.zip tsl tsr with
       | Base.List.Or_unequal_lengths.Unequal_lengths -> fail (`UnificationFailed (l, r))
       | Base.List.Or_unequal_lengths.Ok zipped_list ->
         Base.List.fold_right
           zipped_list
           ~f:(fun (x, y) subst ->
             let* head_sub = unify x y in
             let* subst = subst in
             compose head_sub subst)
           ~init:(return empty))
    | ListT t1, ListT t2 -> unify t1 t2
    (* | AdtT (name1, ts1), AdtT (name2, ts2) when name1 = name2 -> unify ts1 ts2 *)
    | _ -> fail (`UnificationFailed (l, r))

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Base.Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  and compose_all ss =
    Base.List.fold_left ss ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module VarSet = struct
  let fold_right f ini set =
    Base.Set.fold_right set ~init:ini ~f:(fun x acc ->
      let open R.Syntax in
      let* acc = acc in
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

  let free_vars : t -> (named_id, Base.Int.comparator_witness) Base.Set.t =
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
let fresh_var = fresh >>| fun n -> TypeVariable n (* Generate new fresh variable *)

let instantiate : scheme -> Typing.t R.t =
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
 fun env typ ->
  let free = Base.Set.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  free, typ
;;

let lookup_env e map =
  match Base.Map.find map e with
  | None -> fail (`NoVariable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let rec find_identifiers = function
  | BinaryOp (_, left, right) -> find_identifiers left @ find_identifiers right
  | UnaryOp (_, value) -> find_identifiers value
  | App (expr1, expr2) -> find_identifiers expr1 @ find_identifiers expr2
  | Var id -> [ id ]
  | Fun _ -> []
  | _ -> []
;;

let infer =
  let rec helper : TypeEnv.t -> expr -> (Subst.t * Typing.t) R.t =
   fun env -> function
    | Constant c ->
      (match c with
       | Int _ -> return (Subst.empty, number_t)
       | Str _ -> return (Subst.empty, string_t)
       | Bool _ -> return (Subst.empty, bool_t)
       | Unit -> return (Subst.empty, unit_t)
       | Nil -> return (Subst.empty, nil_t))
    | Var name ->
      (match name with
       | _ -> lookup_env name env)
    | UnaryOp (op, expr) ->
      (match op with
       | UnaryMinus ->
         let* subst, t = helper env expr in
         let* subst' = unify t number_t in
         let* final_subst = Subst.compose subst' subst in
         return (final_subst, number_t))
    | BinaryOp (op, left, right) ->
      let* subst_left, typ_left = helper env left in
      let* subst_right, typ_right = helper env right in
      (match op with
       | Plus | Minus | Mult | Divide ->
         let* subst' = unify typ_left number_t in
         let* subst'' = unify typ_right number_t in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; subst_left; subst_right ]
         in
         return (final_subst, number_t)
       | Eq ->
         let* subst' = unify typ_left typ_right in
         let* final_subst = Subst.compose_all [ subst'; subst_left; subst_right ] in
         return (final_subst, bool_t))
    | Fun (arg_name, expr) ->
      let* type_variable = fresh_var in
      let extended_env =
        TypeEnv.extend env arg_name (Base.Set.empty (module Base.Int), type_variable)
      in
      let* subst, t = helper extended_env expr in
      let result_type = arrow_t (Subst.apply subst type_variable) t in
      return (subst, result_type)
    | App (left, right) ->
      let* subst_left, t_left = helper env left in
      let* subst_right, t_right = helper (TypeEnv.apply subst_left env) right in
      let* type_variable = fresh_var in
      let* unified_subst =
        unify (arrow_t t_right type_variable) (Subst.apply subst_right t_left)
      in
      let result_type = Subst.apply unified_subst type_variable in
      let* final_subst = Subst.compose_all [ subst_left; subst_right; unified_subst ] in
      return (final_subst, result_type)
    | IfThenElse (if_expr, then_expr, else_expr) ->
      let* subst_condition, typ_condition = helper env if_expr in
      let* subst_true_branch, typ_true_branch = helper env then_expr in
      let* subst_false_branch, typ_false_branch = helper env else_expr in
      let* subst' = unify typ_condition bool_t in
      let* subst'' = unify typ_true_branch typ_false_branch in
      let* final_subst =
        Subst.compose_all
          [ subst_condition; subst_true_branch; subst_false_branch; subst'; subst'' ]
      in
      return (final_subst, Subst.apply final_subst typ_true_branch)
    | ADT (name, _) when name = nil_adt_name -> return (Subst.empty, nil_t)
    | ADT (name, exprs) when name = cons_adt_name ->
      (match exprs with
       | [ x; y ] ->
         let* head_subst, head_typ = helper env x in
         let rec substlist subst = function
           | ADT (name, _) when name = nil_adt_name -> return subst
           | ADT (name, inner_exprs) when name = cons_adt_name ->
             (match inner_exprs with
              | [ x; y ] ->
                let* elem_subst, elem_typ = helper env x in
                let* subst' = unify elem_typ head_typ in
                let* subst'' = Subst.compose_all [ subst; elem_subst; subst' ] in
                substlist subst'' y
              | _ -> fail `NotReachable)
           | _ -> fail `NotReachable
         in
         let* final_subst = substlist head_subst y in
         return (final_subst, list_t @@ Subst.apply final_subst head_typ)
       | _ -> fail `NotReachable)
    | Tuple list ->
      let rec subst_tuple subst = function
        | [] -> return (subst, [])
        | head :: tail ->
          let* head_subst, head_typ = helper env head in
          let* subst' = Subst.compose subst head_subst in
          let* final_subst, tail_typ = subst_tuple subst' tail in
          return (final_subst, head_typ :: tail_typ)
      in
      let* final_subst, ts = subst_tuple Subst.empty list in
      return (final_subst, tuple_t (List.map (Subst.apply final_subst) ts))
    | Cons (h, t) ->
      let* operand_subst, operand_typ = helper env h in
      let* list_subst, list_typ = helper env t in
      let* subst' = unify (list_t operand_typ) list_typ in
      let* final_subst = Subst.compose_all [ operand_subst; list_subst; subst' ] in
      return (final_subst, Subst.apply subst' list_typ)
    | Match (matched_expression, case_list) ->
      let* matched_subst, matched_type = helper env matched_expression in
      let head = Base.List.hd_exn case_list in
      let bootstrap_env env case =
        let identifiers = find_identifiers case in
        Base.List.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
          let* fresh_var = fresh_var in
          let* acc = acc in
          return (TypeEnv.extend acc id (Base.Set.empty (module Base.Int), fresh_var)))
      in
      let* env' = bootstrap_env env (fst head) in
      let* _, head_expression_type = helper env' (snd head) in
      let* subst' =
        Base.List.fold_right case_list ~init:(return Subst.empty) ~f:(fun case subst ->
          let* env'' = bootstrap_env env (fst case) in
          let* case_subst, case_type = helper env'' (fst case) in
          let case_type =
            match case_type with
            | case_type -> case_type
          in
          let* subst'' = unify case_type matched_type in
          let* computation_subst, computation_type = helper env'' (snd case) in
          let* subst''' = unify computation_type head_expression_type in
          let* subst = subst in
          Subst.compose_all [ subst'''; subst''; subst; case_subst; computation_subst ])
      in
      let* final_subst = Subst.compose subst' matched_subst in
      return (subst', Subst.apply final_subst head_expression_type)
    | ADT (name, content) ->
      let* type_name = return name in
      let rec subst_tuple subst = function
        | [] -> return (subst, [])
        | head :: tail ->
          let* head_subst, head_typ = helper env head in
          let* subst' = Subst.compose subst head_subst in
          let* final_subst, tail_typ = subst_tuple subst' tail in
          return (final_subst, head_typ :: tail_typ)
      in
      let* tuple_subst, ts = subst_tuple Subst.empty content in
      return
        ( tuple_subst
        , NamedT (type_name, Some (tuple_t (List.map (Subst.apply tuple_subst) ts))) )
    | Let (_, _, _, expr) -> helper env expr
    | LetIn (let_expr, in_expr) ->
      let handle_let subst env lexpr =
        let* identifier =
          match lexpr with
          | Let (_, name, _, _) -> return name
          | _ -> fail `NotReachable
        in
        let* fresh_var = fresh_var in
        let env' =
          TypeEnv.extend env identifier (Base.Set.empty (module Base.Int), fresh_var)
        in
        let* elem_subst, elem_typ = helper env' lexpr in
        let env'' = TypeEnv.apply elem_subst env' in
        let generalized_type = generalize env'' elem_typ in
        let* subst'' = Subst.compose subst elem_subst in
        return (subst'', TypeEnv.extend env'' identifier generalized_type)
      in
      let* subst', env' = handle_let Subst.empty env let_expr in
      let* subst_expr, t_expr = helper env' in_expr in
      let* final_subst = Subst.compose subst' subst_expr in
      return (final_subst, t_expr)
    | Type (_, _) -> fail `NotImplementedYet
  in
  helper
;;

let run_inference expression = Result.map snd (run (infer TypeEnv.empty expression))
let print_t t = Printf.printf "%s\n" (show t)

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `Occurs_check -> fprintf fmt "Occurs check failed.\n"
  | `NoVariable identifier -> fprintf fmt "No such variable: %s" identifier
  | `UnificationFailed (t1, t2) ->
    fprintf fmt "Unification failed: type of the expression is ";
    pp fmt t1;
    fprintf fmt " but expected type was ";
    pp fmt t2
  | `NotReachable -> fprintf fmt "Not reachable."
  | `NotImplementedYet -> fprintf fmt "Not implemented yet."
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;

let get_infered expression =
  match run_inference expression with
  | Ok typ -> print_t typ
  | Error x -> print_type_error x
;;

let%expect_test "Base Int type inference" =
  get_infered (Constant (Int 1));
  [%expect {| (BaseT Int) |}]
;;

let%expect_test "Base UnaryOp type inference" =
  get_infered (UnaryOp (UnaryMinus, Constant (Int 1)));
  [%expect {| (BaseT Int) |}]
;;

let%expect_test "Fun type inference" =
  get_infered (Fun ("x", Var "x"));
  [%expect {| (ArrowT ((TypeVariable 0), (TypeVariable 0))) |}]
;;

let%expect_test "Fun type inference with unknown type and Int" =
  get_infered (Fun ("x", Constant (Int 1)));
  [%expect {| (ArrowT ((TypeVariable 0), (BaseT Int))) |}]
;;

let%expect_test "Fun with several arguments type inference with unificated to Int" =
  get_infered (Fun ("x", Fun ("y", BinaryOp (Plus, Var "x", Var "y"))));
  [%expect {| (ArrowT ((BaseT Int), (ArrowT ((BaseT Int), (BaseT Int))))) |}]
;;

let%expect_test "App type inference" =
  get_infered (App (Fun ("x", Var "x"), Constant (Int 1)));
  [%expect {| (BaseT Int) |}]
;;

let%expect_test "Failed IfThenElse type inference" =
  get_infered (IfThenElse (Constant (Bool false), Constant (Int 1), Constant (Str "1")));
  [%expect
    {|
    Unification failed: type of the expression is (BaseT Int) but expected type was (
    BaseT String) |}]
;;

let%expect_test "List type inference" =
  get_infered
    (ADT
       ( cons_adt_name
       , [ Constant (Int 1)
         ; ADT (cons_adt_name, [ Constant (Int 1); ADT (nil_adt_name, []) ])
         ] ));
  [%expect {| (ListT (BaseT Int)) |}]
;;

let%expect_test "Failed List type inference" =
  get_infered
    (ADT
       ( cons_adt_name
       , [ Constant (Int 1)
         ; ADT (cons_adt_name, [ Constant (Bool true); ADT (nil_adt_name, []) ])
         ] ));
  [%expect
    {|
    Unification failed: type of the expression is (BaseT Bool) but expected type was (
    BaseT Int) |}]
;;

let%expect_test "Let type inference" =
  get_infered (Let (false, "x", None, Constant (Int 1)));
  [%expect {| (BaseT Int) |}]
;;
