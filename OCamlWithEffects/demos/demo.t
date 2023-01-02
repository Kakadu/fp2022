  $ ./demo.exe <<- EOF
  > let main = 1 + 2
  > EOF
  3
  $ ./demo.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 6
  > EOF
  720
  $ ./demo.exe <<-EOF
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let tuple_map f tuple = match tuple with
  >   | (x, y) -> (f x, f y)
  > 
  > let main = map (tuple_map (fun x -> x * 2)) [(1, 2); (5, 6)]
  > EOF
  [(2, 4); (10, 12)]
  $ ./demo.exe <<- EOF
  > let phi n = let rec helper last1 last2 n = if n > 0 then helper last2 (last1 + last2) (n - 1) else last2 in helper 1 1 (n - 2)
  > 
  > let main = phi 10
  > EOF
  55
  $ ./demo.exe <<- EOF
  > let product list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc * head)
  >     | _ -> acc
  >   in
  >   helper list 1
  > 
  > let main = product [1; 2; 7; 12; 10; 3; 21]
  > EOF
  105840
  $ ./demo.exe <<- EOF
  > let sum list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc + head)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = sum [1; 2; 7; 12; 10; 3; 21; 101; 78; 42; 38]
  > EOF
  315
  $ ./demo.exe <<- EOF
  > let length list =
  >   let rec helper list acc = match list with
  >     | _ :: tail -> helper tail (acc + 1)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = length [1; 23; 12; 657; 123; 346; 6; 234 ; 99; 34; 78; 28; 123; 0]
  > EOF
  14
  $ ./demo.exe <<- EOF
  > let head list =
  >   match list with
  >     | h :: _ -> Some h
  >     | _ -> None
  > 
  > let main = head [97; 81; 0; 54; 13]
  > EOF
  Some 97
  $ ./demo.exe <<- EOF
  > let head list =
  >   match list with
  >     | h :: _ -> Some h
  >     | _ -> None
  > 
  > let main = head []
  > EOF
  None 
  $ ./demo.exe <<- EOF
  > let tail list =
  >   match list with
  >     | _ :: t -> Some t
  >     | _ -> None
  > 
  > let main = tail [97; 81; 0; 54; 13]
  > EOF
  Some [81; 0; 54; 13]
  $ ./demo.exe <<- EOF
  > let tail list =
  >   match list with
  >     | _ :: t -> Some t
  >     | _ -> None
  > 
  > let main = tail []
  > EOF
  None 
  $ ./demo.exe <<- EOF
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63]
  > EOF
  [12; 3; 0; 1; 2]
  $ ./demo.exe <<- EOF
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = filter (fun v -> v > 10) [1;2;3]
  > EOF
  []
  $ ./demo.exe <<- EOF
  > let filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63]
  > EOF
  No such variable: filter
  $ ./demo.exe <<- EOF
  > let main = f 1 2
  > EOF
  No such variable: f
  $ ./demo.exe <<- EOF
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = count_solutions_of_sq_equation 2 9 4
  > EOF
  2
  $ ./demo.exe <<- EOF
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = count_solutions_of_sq_equation 2 9 4
  > EOF
  2
  $ ./demo.exe <<- EOF
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let sq = fun x -> x * x
  > 
  > let main = map sq [1;2;3;4;5;6;7;8;9;10]
  > EOF
  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
  $ ./demo.exe <<- EOF
  > let f x y z =
  >   match x, y, z with
  >     | true, true, true -> true
  >     | false, false, false -> true
  >     | _ -> false
  > 
  > let main = f (10 * 5 > 49) (58 / 2 = 27) (10 <> 20)
  > EOF
  false
  $ ./demo.exe <<- EOF
  > let main = "abc" + "def"
  > EOF
  Unification failed: type of the expression is string but expected type was int
  $ ./demo.exe <<- EOF
  > let pifagor_check = fun x y z -> x * x + y * y = z * z
  > 
  > let main = pifagor_check 3 4 5
  > EOF
  true
  $ ./demo.exe <<- EOF
  > let check_password password = 
  >   match password with
  >     | "qwerty123" -> Ok "success"
  >     | _ -> Error "FAIL"
  > 
  > let main = check_password "qwerty"
  > EOF
  Error "FAIL"
  $ ./demo.exe <<- EOF
  > let check_password password = 
  >   match password with
  >     | "qwerty123" -> Ok "success"
  >     | _ -> Error "FAIL"
  > 
  > let main = check_password "qwerty123"
  > EOF
  Ok "success"
  $ ./demo.exe <<- EOF
  > let check_password password = 
  >   match password with
  >     | "qwerty123" -> Ok "success"
  >     | _ -> Error "FAIL"
  > EOF
  <fun>
  $ ./demo.exe <<- EOF
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 2
  > 
  > let main = (if idx = 1 then fst else snd) (13, 225)
  > EOF
  225
  $ ./demo.exe <<- EOF
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 1
  > 
  > let main = (if idx = 1 then fst else snd) (13, 225)
  > EOF
  13
  $ ./demo.exe <<- EOF
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 1
  > 
  > let main = (if idx = 1 then fst else snd) (13, 45, 89)
  > EOF
  Unification failed: type of the expression is int * int * int but expected type was 'p * 'p
  $ ./demo.exe <<- EOF
  > let rec matrix_sum m1 m2 =
  >   let rec lines_sum l1 l2 =
  >     match l1, l2 with
  >       | h1 :: t1, h2 :: t2 -> (h1 + h2) :: lines_sum t1 t2
  >       | _, _ -> []
  >   in
  >   match m1, m2 with
  >     | h1 :: t1, h2 :: t2 -> lines_sum h1 h2 :: matrix_sum t1 t2
  >     | _, _ -> []
  > 
  > let matrix1 = [[1;  5;  7 ];
  >                [13; 32; 56];
  >                [45; 2;  17]]
  > 
  > let matrix2 = [[4;  29;  0];
  >                [79; 12; 66];
  >                [8;  88; 19]]
  > 
  > let main = matrix_sum matrix1 matrix2
  > EOF
  [[5; 34; 7]; [92; 44; 122]; [53; 90; 36]]
  $ ./demo.exe <<- EOF
  > let rec matrix_mult_number matrix number =
  >   let rec line_mult_number line =
  >     match line with
  >       | head :: tail -> (head * number) :: line_mult_number tail
  >       | _ -> []
  >   in
  >   match matrix with
  >     | head :: tail -> line_mult_number head :: matrix_mult_number tail number
  >     | _ -> []
  > 
  > let matrix = [[1;  5;  7 ];
  >               [13; 32; 56];
  >               [45; 2;  17]]
  > 
  > let main = matrix_mult_number matrix 5
  > EOF
  [[5; 25; 35]; [65; 160; 280]; [225; 10; 85]]
  $ ./demo.exe <<- EOF
  > let rec matrix_mult_number matrix number =
  >   let rec line_mult_number line =
  >     match line with
  >       | head :: tail -> (head * number) :: line_mult_number tail
  >       | _ -> []
  >   in
  >   match matrix with
  >     | head :: tail -> line_mult_number head :: matrix_mult_number tail number
  >     | _ -> []
  > 
  > let rec matrix_sum m1 m2 =
  >   let rec lines_sum l1 l2 =
  >     match l1, l2 with
  >       | h1 :: t1, h2 :: t2 -> (h1 + h2) :: lines_sum t1 t2
  >       | _, _ -> []
  >   in
  >   match m1, m2 with
  >     | h1 :: t1, h2 :: t2 -> lines_sum h1 h2 :: matrix_sum t1 t2
  >     | _, _ -> []
  > 
  > let matrix1 = [[1;  5;  7 ];
  >                [13; 32; 56];
  >                [45; 2;  17]]
  > 
  > let matrix2 = [[4;  29;  0];
  >                [79; 12; 66];
  >                [8;  88; 19]]
  > 
  > let main = matrix_sum (matrix_mult_number matrix1 2) (matrix_mult_number matrix2 7)
  > EOF
  [[30; 213; 14]; [579; 148; 574]; [146; 620; 167]]
  $ ./demo.exe <<- EOF
  > let int_list = [1; 2; 3]
  > 
  > let main = "0" :: int_list
  > EOF
  Unification failed: type of the expression is string but expected type was int
  $ ./demo.exe <<- EOF
  > effect Failure : string -> int effect
  > 
  > let binary_int_of_str n = match n with
  >   | "0" -> 0
  >   | "1" -> 1
  >   | s -> perform (Failure s)
  > 
  > let rec sum_up list = match list with
  >   | [] -> 0
  >   | s :: ss -> binary_int_of_str s + sum_up ss
  > 
  > let test_list = ["0"; "hope"; "1"; "it"; "0"; "works"; "1"]
  > 
  > let main = match sum_up test_list with
  >   | effect (Failure _) -> continue 0
  >   | res -> res
  > EOF
  2
  $ ./demo.exe <<-EOF
  > effect E: int -> int effect
  > 
  > let helper x = match perform (E x) with
  >    | effect (E s) -> continue (s*s)
  >    | l -> l
  > 
  > let main = match perform (E 5) with
  >    | effect (E s) -> continue (s*s)
  >    | l -> helper l
  > EOF
  625
  $ ./demo.exe <<-EOF
  > effect EmptyListException : int effect
  > 
  > let list_hd list = match list with
  >    | [] -> perform EmptyListException
  >    | hd :: _ -> hd
  > 
  > let safe_list_hd l = match list_hd l with
  >   | effect EmptyListException -> 0, false
  >   | res -> res, true
  > 
  > let main = safe_list_hd [12; 65; 94]
  > EOF
  (12, true)
  $ ./demo.exe <<-EOF
  > effect EmptyListException : int effect
  > 
  > let list_hd list = match list with
  >    | [] -> perform EmptyListException
  >    | hd :: _ -> hd
  > 
  > let safe_list_hd l = match list_hd l with
  >   | effect EmptyListException -> 0, false
  >   | res -> res, true
  > 
  > let main = safe_list_hd []
  > EOF
  (0, false)
  $ ./demo.exe <<-EOF
  > effect EmptyListException : int effect
  > 
  > let list_hd list = match list with
  >    | [] -> perform EmptyListException
  >    | hd :: _ -> hd
  > 
  > let safe_list_hd l = match list_hd l with
  >   | effect EmptyListException -> continue (0, false)
  >   | res -> res, true
  > 
  > let main = safe_list_hd []
  > EOF
  ((0, false), true)
  $ ./demo.exe <<-EOF
  > effect SmallDiscount : int -> int effect
  > 
  > effect BigDiscount : int -> int effect
  > 
  > let count_discount value = if value < 10000 then perform (SmallDiscount value) else perform (BigDiscount value)
  > 
  > let main = match count_discount 8500 with
  >   | effect (SmallDiscount v) -> continue (v - v / 10)
  >   | effect (BigDiscount v) -> continue (v - v / 5)
  >   | v -> v
  > EOF
  7650
  $ ./demo.exe <<-EOF
  > effect SmallDiscount : int -> int effect
  > 
  > effect BigDiscount : int -> int effect
  > 
  > let count_discount value = if value < 10000 then perform (SmallDiscount value) else perform (BigDiscount value)
  > 
  > let main = match count_discount 25000 with
  >   | effect (SmallDiscount v) -> continue (v - v / 10)
  >   | effect (BigDiscount v) -> continue (v - v / 5)
  >   | v -> v
  > EOF
  20000
  $ ./demo.exe <<-EOF
  > effect E: int -> int effect
  > 
  > let helper x = match perform (E x) with
  >    | effect (E s) -> continue "hello"
  >    | v -> v
  > 
  > let main = match perform (E 5) with
  >    | effect (E s) -> continue (s*s)
  >    | v -> helper v
  > EOF
  Unification failed: type of the expression is string but expected type was int
  $ ./demo.exe <<-EOF
  > let main = [1; 2; 3; "abc"]
  > EOF
  Unification failed: type of the expression is string but expected type was int
  $ ./demo.exe <<-EOF
  > let main = (1, 2, 3, "abc", fun x -> x * x)
  > EOF
  (1, 2, 3, "abc", <fun>)
  $ ./demo.exe <<-EOF
  > let f x = x * 100
  > let main = ["0", "1", f "2"]
  > EOF
  Unification failed: type of the expression is string but expected type was int
  $ ./demo.exe <<-EOF
  > let rec remove_last list = match list with
  > | [] -> []
  > | [head] -> []
  > | head :: tail -> head :: remove_last tail
  > 
  > let main = remove_last [1;2;3;4;5]
  > EOF
  [1; 2; 3; 4]
  $ ./demo.exe <<-EOF
  > let _ x = x
  > EOF
  : end_of_input
