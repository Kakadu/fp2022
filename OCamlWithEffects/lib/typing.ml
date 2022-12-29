(** Copyright 2021-2022, Danila Pechenev & Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_variable_number = int
type adt_type = string
type identifier = string

type ground_type =
  | Int
  | String
  | Char
  | Bool
  | Unit
[@@deriving eq, show { with_path = false }]

type typ =
  | TVar of type_variable_number (** 'a *)
  | TArr of typ * typ (** string -> int *)
  | TTuple of typ list (** int * int *)
  | TList of typ (** 'a list *)
  | TGround of ground_type (** int *)
  | TADT of adt_type * typ (** (int, string) Result *)
  | TEffect of typ (** (int -> char) effect *)

(* Ground types *)
let int_typ = TGround Int
let bool_typ = TGround Bool
let string_typ = TGround String
let unit_typ = TGround Unit
let char_typ = TGround Char
(* ------------ *)

(* Smart constructors for types *)
let tarrow left_type right_type = TArr (left_type, right_type)
let ttuple type_list = TTuple type_list
let tlist typ = TList typ
let tvar n = TVar n
let tadt name typ = TADT (name, typ)
let teffect typ = TEffect typ
(* ---------------------------- *)

let rec pp_type fmt typ =
  let open Format in
  let arrow_format = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | Int -> fprintf fmt "int"
     | String -> fprintf fmt "string"
     | Char -> fprintf fmt "char"
     | Bool -> fprintf fmt "bool"
     | Unit -> fprintf fmt "unit")
  | TTuple value_list ->
    fprintf
      fmt
      "%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmt " * ")
         (fun fmt typ -> pp_type fmt typ))
      value_list
  | TList typ -> fprintf fmt (arrow_format typ ^^ " list") pp_type typ
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow_format typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
  | TVar var -> fprintf fmt "%s" @@ "'" ^ Char.escaped (Char.chr (var + 97))
  | TADT (name, typ) ->
    pp_type fmt typ;
    fprintf fmt "%s" name
  | TEffect typ -> fprintf fmt (arrow_format typ ^^ " effect") pp_type typ
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `Occurs_check
  | `NoVariable of identifier
  | `NoConstructor of identifier
  | `UnificationFailed of typ * typ
  | `NotReachable
  | `NoHandlerProvided
  ]

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `Occurs_check -> fprintf fmt "Occurs check failed.\n"
  | `NoVariable identifier -> fprintf fmt "No such variable: %s" identifier
  | `NoConstructor identifier -> fprintf fmt "No such constructor: %s" identifier
  | `UnificationFailed (t1, t2) ->
    fprintf fmt "Unification failed: type of the expression is ";
    pp_type fmt t1;
    fprintf fmt " but expected type was ";
    pp_type fmt t2
  | `NotReachable -> fprintf fmt "Not reachable."
  | `NoHandlerProvided ->
    fprintf fmt "Effect appears in pattern-matching but handler was not provided."
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;
