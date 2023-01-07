Copyright 2021-2022, Michael Polyntsov and contributors
SPDX-License-Identifier: CC0-1.0

  $ ./demoQueryGen.exe <<-EOF
  > select * from t1
  Project t1.A (IntCol 0), t1.B (StringCol 1) ->
      Datasource (t1)

  $ ./demoQueryGen.exe <<-EOF
  > select *, * from t1, t2, t3
  Project t1.A (IntCol 0), t1.B (StringCol 1), t2.A (IntCol 2),
          t2.B (StringCol 3), t3.a (IntCol 4), t3.b (StringCol 5),
          t3.c (StringCol 6), t1.A (IntCol 0), t1.B (StringCol 1),
          t2.A (IntCol 2), t2.B (StringCol 3), t3.a (IntCol 4),
          t3.b (StringCol 5), t3.c (StringCol 6) ->
      CROSS JOIN
          CROSS JOIN
              Datasource (t1)
          AND
              Datasource (t2)
      AND
          Datasource (t3)

  $ ./demoQueryGen.exe <<-EOF
  > select t1.a + t2.a + t3.a = 1 + 1 AS something,
  > (t2.b = t3.b) = (t1.b = t3.c) AS pp
  > from t1 left join (t2 right join t3 on t2.b = t3.c) on t1.a = t2.a
  > where t1.a < 10 AND t2.b = 'something'
  Project something (Equal (Plus (Plus (IntCol 0, IntCol 2), IntCol 4),
                            Plus (ConstInt 1, ConstInt 1))),
          pp (Equal (Equal (StringCol 3, StringCol 5),
                     Equal (StringCol 1, StringCol 6))) ->
      LEFT JOIN
          Filter (Less (IntCol 0, ConstInt 10)) ->
              Datasource (t1)
      AND
          RIGHT JOIN
              Filter (Equal (StringCol 1, ConstString something)) ->
                  Datasource (t2)
          AND
              Datasource (t3)
          ON Equal (StringCol 1, StringCol 4)
      ON Equal (IntCol 0, IntCol 2)

  $ ./demoQueryGen.exe <<-EOF
  > select c
  > from t1, t2, t3
  > where t1.a < 10 AND t2.b = 'something' AND t1.b = 'this' AND t2.a = 1 
  Project c (StringCol 6) ->
      CROSS JOIN
          CROSS JOIN
              Filter (And (Less (IntCol 0, ConstInt 10),
                           Equal (StringCol 1, ConstString this))) ->
                  Datasource (t1)
          AND
              Filter (And (Equal (StringCol 1, ConstString something),
                           Equal (IntCol 0, ConstInt 1))) ->
                  Datasource (t2)
      AND
          Datasource (t3)
OR t3.c = 'e'
  $ ./demoQueryGen.exe <<-EOF
  > select t1.b = t2.b AND t3.c = 'long' AS something
  > from t2 right join t3 on t2.b = t3.c inner join t1 on t1.a = t2.a
  > where t1.a >= 10 AND t2.b != 'harry' AND t1.b != 'that' AND (2+2=4)
  Project something (And (Equal (StringCol 6, StringCol 1),
                          Equal (StringCol 4, ConstString long))) ->
      INNER JOIN
          RIGHT JOIN
              Filter (NotEqual (StringCol 1, ConstString harry)) ->
                  Datasource (t2)
          AND
              Datasource (t3)
          ON Equal (StringCol 1, StringCol 4)
      AND
          Filter (And (Equal (Plus (ConstInt 2, ConstInt 2), ConstInt 4),
                       And (GreaterOrEq (IntCol 0, ConstInt 10),
                            NotEqual (StringCol 1, ConstString that)))) ->
              Datasource (t1)
      ON Equal (IntCol 5, IntCol 0)
 

$ ./demoQueryGen.exe <<-EOF
> select t1.a + t2.a + t3.a = 1 + 1 AS something,
> (t2.b = t3.b) = (t1.b = t3.c) AS pp
> from t1, t2 right join t3 on t2.b = t3.c
> where t1.a < 10 AND t2.b = 'something' AND t1.b = 'this' AND (t2.a = 1 OR t3.c = 'e')
 
