open Ast
open Format

let pp_literal fmt = function
  | LInt x -> fprintf fmt "%d" x
  | LString x -> fprintf fmt "%s" x
  | LChar x -> fprintf fmt "%c" x
  | LBool x -> fprintf fmt "%b" x
  | LUnit -> fprintf fmt "()"
;;

let pp_binary_operator fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Eq -> fprintf fmt "="
  | NEq -> fprintf fmt "<>"
  | GT -> fprintf fmt ">"
  | GTE -> fprintf fmt "≥"
  | LT -> fprintf fmt "<"
  | LTE -> fprintf fmt "≤"
  | AND -> fprintf fmt "&&"
  | OR -> fprintf fmt "||"
;;

let pp_unary_operator fmt = function
  | Minus -> fprintf fmt "-"
  | Not -> fprintf fmt "not "
;;

let rec pp_expression fmt =
  let get_format expression =
    match expression with
    | ELiteral _ | EIdentifier _ -> format_of_string "%a"
    | _ -> format_of_string "(%a)"
  in
  let rec pp_list fmt delimiter = function
    | [ head ] -> pp_expression fmt head
    | head :: tail ->
      pp_expression fmt head;
      fprintf fmt "%c " delimiter;
      pp_list fmt delimiter tail
    | [] -> ()
  in
  let pp_id_list fmt list = Base.List.iter list ~f:(fun x -> fprintf fmt "%s;" x) in
  function
  | ELiteral literal -> pp_literal fmt literal
  | EIdentifier id -> fprintf fmt "%s" id
  | EBinaryOperation (binary_operator, left_operand, right_operand) ->
    let format =
      format_of_string
        (match left_operand, right_operand with
         | ELiteral _, ELiteral _
         | ELiteral _, EIdentifier _
         | EIdentifier _, ELiteral _
         | EIdentifier _, EIdentifier _ -> "%a %a %a"
         | _ -> "(%a) %a (%a)")
    in
    fprintf
      fmt
      format
      pp_expression
      left_operand
      pp_binary_operator
      binary_operator
      pp_expression
      right_operand
  | EUnaryOperation (unary_operator, operand) ->
    fprintf
      fmt
      ("%a" ^^ get_format operand)
      pp_unary_operator
      unary_operator
      pp_expression
      operand
  | EApplication (left_operand, right_operand) ->
    fprintf
      fmt
      (get_format left_operand ^^ get_format right_operand)
      pp_expression
      left_operand
      pp_expression
      right_operand
  | EFun (id_list, expression) ->
    fprintf fmt "fun %a -> %a" pp_id_list id_list pp_expression expression
  | EList list -> pp_list fmt ';' list
  | EConstructList (operand, list) ->
    fprintf
      fmt
      (get_format operand ^^ get_format list)
      pp_expression
      operand
      pp_expression
      list
  | ETuple list -> pp_list fmt ',' list
  | EDeclaration (name, id_list, expression) ->
    fprintf fmt "let %s %a = %a" name pp_id_list id_list pp_expression expression
  | ERecursiveDeclaration (name, id_list, expression) ->
    fprintf fmt "let rec %s %a = %a" name pp_id_list id_list pp_expression expression
  | _ -> fprintf fmt ""
;;
