let string_to_list str =
  let rec helper acc list =
    match acc with
    | -1 -> list
    | _ -> helper (acc - 1) (str.[acc] :: list)
  in
  helper (String.length str - 1) []
;;

let%test _ = string_to_list "123" = [ '1'; '2'; '3' ]