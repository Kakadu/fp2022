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
  | TVar of type_variable_number
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TGround of ground_type
  | TADT of adt_type * typ

let int_typ = TGround Int
let bool_typ = TGround Bool
let string_typ = TGround String
let unit_typ = TGround Unit
let char_typ = TGround Char
let t_arrow l r = TArr (l, r)
let t_tuple typ_list = TTuple typ_list
let t_list typ = TList typ
let t_var n = TVar n
let t_adt name typ = TADT (name, typ)

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
      "%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmt " * ")
         (fun fmt ty -> pp_type fmt ty))
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
  | TADT (name, typ) ->
    pp_type fmt typ;
    fprintf fmt "%s" name
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
;;

let print_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;