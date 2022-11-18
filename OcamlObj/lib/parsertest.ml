(** Copyright 2021-2022, Kalashnikov Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

let test_parse ~code ~expected =
  match parse code with
  | Ok ok ->
    (match List.equal equal_declaration ok expected with
     | true -> true
     | false ->
       Format.printf "Expected: %a\nActual: %a\n" pp_prog expected pp_prog ok;
       false)
  | Error err ->
    Format.printf "Error: %s\n" err;
    false
;;

let%test _ =
  test_parse
    ~code:
      {|
      let s = object
  val v = 10
  method minus = v - 1
  method plus = v + 1
  
  
  method times = 
    let helper = fun a -> a*2 in
    helper
  
end;;


let test = s#minus;;

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "s"
          , EObj
              [ OVal (PVar "v", EConst (CInt 10))
              ; OMeth (PVar "minus", EBinop (Sub, EVar "v", EConst (CInt 1)))
              ; OMeth (PVar "plus", EBinop (Add, EVar "v", EConst (CInt 1)))
              ; OMeth
                  ( PVar "times"
                  , ELet
                      ( ( false
                        , PVar "helper"
                        , EFun (PVar "a", EBinop (Mul, EVar "a", EConst (CInt 2))) )
                      , EVar "helper" ) )
              ] )
      ; DLet (false, PVar "test", ECallM ("s", "minus"))
      ]
;;

let%test _ =
  test_parse
    ~code:
      {|

    let pair first second = 
  object
  val first = first
  val second = second
  method get_first = first
  method get_second = second
end;;

let mypair = pair 1 2;;

let pairsum = mypair#get_first + mypair#get_second
;;



    |}
    ~expected:
      [ DLet
          ( false
          , PVar "pair"
          , EFun
              ( PVar "first"
              , EFun
                  ( PVar "second"
                  , EObj
                      [ OVal (PVar "first", EVar "first")
                      ; OVal (PVar "second", EVar "second")
                      ; OMeth (PVar "get_first", EVar "first")
                      ; OMeth (PVar "get_second", EVar "second")
                      ] ) ) )
      ; DLet
          ( false
          , PVar "mypair"
          , EApp (EApp (EVar "pair", EConst (CInt 1)), EConst (CInt 2)) )
      ; DLet
          ( false
          , PVar "pairsum"
          , EBinop (Add, ECallM ("mypair", "get_first"), ECallM ("mypair", "get_second"))
          )
      ]
;;

let%test _ =
  test_parse
    ~code:{|

    let _ = (4 / 2 + 3 * 2 - 1) * (2 + 3) / 2;;

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "_"
          , EBinop
              ( Div
              , EBinop
                  ( Mul
                  , EBinop
                      ( Sub
                      , EBinop
                          ( Add
                          , EBinop (Div, EConst (CInt 4), EConst (CInt 2))
                          , EBinop (Mul, EConst (CInt 3), EConst (CInt 2)) )
                      , EConst (CInt 1) )
                  , EBinop (Add, EConst (CInt 2), EConst (CInt 3)) )
              , EConst (CInt 2) ) )
      ]
;;

let%test _ =
  test_parse
    ~code:
      {|

    let a = 
      let b = 
        let c = 
          let d x = x * 3
        in d 
      in c
      in fun x -> b (x + 2)

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "a"
          , ELet
              ( ( false
                , PVar "b"
                , ELet
                    ( ( false
                      , PVar "c"
                      , ELet
                          ( ( false
                            , PVar "d"
                            , EFun (PVar "x", EBinop (Mul, EVar "x", EConst (CInt 3))) )
                          , EVar "d" ) )
                    , EVar "c" ) )
              , EFun (PVar "x", EApp (EVar "b", EBinop (Add, EVar "x", EConst (CInt 2))))
              ) )
      ]
;;

let%test _ =
  test_parse
    ~code:
      {|

    let rec factorial n =
    if n <= 1 then
      1
    else
      factorial (n-1) * n;;

    |}
    ~expected:
      [ DLet
          ( true
          , PVar "factorial"
          , EFun
              ( PVar "n"
              , EIf
                  ( EBinop (Leq, EVar "n", EConst (CInt 1))
                  , EConst (CInt 1)
                  , EBinop
                      ( Mul
                      , EApp (EVar "factorial", EBinop (Sub, EVar "n", EConst (CInt 1)))
                      , EVar "n" ) ) ) )
      ]
;;

let%test _ =
  test_parse
    ~code:{|


      let max a b =
  if a > b then a else b;;


      |}
    ~expected:
      [ DLet
          ( false
          , PVar "max"
          , EFun
              ( PVar "a"
              , EFun (PVar "b", EIf (EBinop (Gre, EVar "a", EVar "b"), EVar "a", EVar "b"))
              ) )
      ]
;;

let%test _ =
  test_parse
    ~code:
      {|

    let keyw token =
      match token with
      | "let" -> 1
      | "match" -> 2
      | "with" -> 3
      | "rec" -> 4
      | "in" -> 5
      | "fun" -> 6
      | "if" -> 7
      | "then" -> 8
      | _ -> 0
      

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "keyw"
          , EFun
              ( PVar "token"
              , EMatch
                  ( EVar "token"
                  , [ PConst (CString "let"), EConst (CInt 1)
                    ; PConst (CString "match"), EConst (CInt 2)
                    ; PConst (CString "with"), EConst (CInt 3)
                    ; PConst (CString "rec"), EConst (CInt 4)
                    ; PConst (CString "in"), EConst (CInt 5)
                    ; PConst (CString "fun"), EConst (CInt 6)
                    ; PConst (CString "if"), EConst (CInt 7)
                    ; PConst (CString "then"), EConst (CInt 8)
                    ; PVar "_", EConst (CInt 0)
                    ] ) ) )
      ]
;;
