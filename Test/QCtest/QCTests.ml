(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open QCheck.Gen
open Lambda_lib
open Lambda_lib.Utils

let varname =
  map (fun n -> String.make 1 (Char.chr n)) (int_range (Char.code 'a') (Char.code 'z'))
;;

let lam_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self -> function
         | 0 -> map var varname
         | n ->
           frequency
             [ 1, map2 abs varname (self (n / 2))
             ; 1, map2 app (self (n / 2)) (self (n / 2))
             ]))
;;

let __ () =
  List.iter
    (fprintf std_formatter "%a\n" Printast.pp_named)
    (QCheck.Gen.generate ~n:20 lam_gen)
;;

let arbitrary_lam =
  let open QCheck.Iter in
  let rec shrink_lam = function
    | Ast.Var i -> QCheck.Shrink.string i >|= var
    | Abs (c, b) -> of_list [ b ] <+> (shrink_lam b >|= fun b' -> abs c b')
    | App (a, b) ->
      of_list [ a; b ]
      <+> (shrink_lam a >|= fun a' -> app a' b)
      <+> (shrink_lam b >|= fun b' -> app a b')
  in
  QCheck.make lam_gen ~print:(asprintf "%a" Printast.pp_named) ~shrink:shrink_lam
;;

let rec pp ppf = function
  | Ast.Var c ->
    Format.fprintf ppf "%s" c (* | App (l, r) -> Format.fprintf ppf "(%a %a)" pp l pp r *)
  | App (l, r) -> Format.fprintf ppf "%a %a" pp l pp r (* Buggy implementation *)
  | Abs (x, b) -> Format.fprintf ppf "(\\%s . %a)" x pp b
;;

let print_parse_is_identity =
  QCheck.(
    Test.make arbitrary_lam (fun l ->
      Caml.Result.ok l
      = Angstrom.parse_string
          ~consume:Angstrom.Consume.Prefix
          Parser.(parse_lam.single parse_lam)
          (Format.asprintf "%a" pp l)))
;;

let run () = QCheck_base_runner.run_tests [ print_parse_is_identity ]
