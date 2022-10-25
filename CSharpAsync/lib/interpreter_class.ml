open Ast

module type MONAD = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let error = Result.error
  let ( >> ) x f = x >>= fun _ -> f
end

module Interpreter_for_classes (M : MONADERROR) = struct
  open M
  open Tables

  let rec monadic_list_iter action list ret =
    match list with
    | [] -> return ret
    | x :: xs -> action x >> monadic_list_iter action xs ret

  let prog_init class_map =
    let field_map = KeyMap.empty in
    let method_map = KeyMap.empty in
    let body = StmtsBlock [Return (Some (Var "message"))] in
    let method_t : method_t =
      { method_mod= []
      ; method_type= String
      ; method_key= "ToString"
      ; args= []
      ; body } in
    let field_t : field_t =
      {field_type= String; field_key= "message"; is_const= false; sub_tree= None}
    in
    let dec_class =
      Class
        ( [Public]
        , "Exception"
        , None
        , [ ([Public], VarField (String, [("message", None)]))
          ; ( [Public]
            , Method
                ( []
                , String
                , "ToString"
                , []
                , StmtsBlock [Return (Some (Var "message"))] ) ) ] ) in
    let field_map = KeyMap.add "message" field_t field_map in
    let method_map = KeyMap.add "ToString" method_t method_map in
    let class_map =
      (* Delete excptions *)
      KeyMap.add "Exception"
        { class_key= "Exception"
        ; field_map
        ; method_map
        ; parent_key= None
        ; dec_class }
        class_map in
    return class_map

  let add_classes class_list_ast class_map =
    let add_class_in_map cl_map adding_class =
      match adding_class with
      | Class (_, class_key, parent, fields) -> (
          (* Function of adding a class element to the corresponding map *)
          let add_class_elem field_elem field_map method_map =
            match field_elem with
            | mod_list, VarField (field_type, arg_list) ->
                let rec helper_add_var list field_map method_map =
                  match list with
                  | [] -> return (field_map, method_map)
                  | (field_key, sub_tree) :: ps ->
                      let is_const = List.mem Const mod_list in
                      ( match KeyMap.find_opt field_key field_map with
                      | None ->
                          let field_map =
                            KeyMap.add field_key
                              {field_type; field_key; is_const; sub_tree}
                              field_map in
                          return (field_map, method_map)
                      | _ ->
                          error
                            (String.concat ""
                               [ "The field with this key: "; field_key
                               ; " already exists" ] ) )
                      >>= fun (field_m, method_m) ->
                      helper_add_var ps field_m method_m in
                helper_add_var arg_list field_map method_map
            | _, Method (method_mod, method_type, method_key, args, body) -> (
              match KeyMap.find_opt method_key method_map with
              | None ->
                  let method_map =
                    KeyMap.add method_key
                      {method_mod; method_type; method_key; args; body}
                      method_map in
                  return (field_map, method_map)
              | _ ->
                  error
                    (String.concat ""
                       [ "The method with this key: "; method_key
                       ; " already exists" ] ) ) in
          let rec iter_fields fields field_map method_map =
            match fields with
            | [] -> return (field_map, method_map)
            | x :: xs ->
                add_class_elem x field_map method_map
                >>= fun (field_m, method_m) -> iter_fields xs field_m method_m
          in
          iter_fields fields KeyMap.empty KeyMap.empty
          >>= fun (field_map, method_map) ->
          let parent_key = parent in
          match KeyMap.find_opt class_key cl_map with
          | None ->
              let class_t =
                { class_key
                ; field_map
                ; method_map
                ; parent_key
                ; dec_class= adding_class } in
              let cl_map = KeyMap.add class_key class_t cl_map in
              return cl_map
          | _ ->
              error
                (String.concat ""
                   ["The class with this key: "; class_key; " already exists"] )
          ) in
    let rec iter_classes class_list_ast class_map =
      match class_list_ast with
      | [] -> return class_map
      | x :: xs ->
          add_class_in_map class_map x
          >>= fun class_m -> iter_classes xs class_m in
    iter_classes class_list_ast class_map >>= fun class_m -> return class_m

  let interpret_classes class_list_ast class_map =
    match class_list_ast with
    | [] -> error "No classes found, incorrect syntax or empty file"
    | _ ->
        prog_init class_map
        >>= fun class_map_with_ex ->
        add_classes class_list_ast class_map_with_ex
end
