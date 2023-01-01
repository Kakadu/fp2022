  $ ./demoParse.exe <<-EOF
  > 34 'string =)' {1, 2}
  > EOF
  [(Expr (Const (Number 34.))); (Expr (Const (String "string =)")));
    (Expr
       (TableInit
          [(JustExpr (Const (Number 1.))); (JustExpr (Const (Number 2.)))]))
    ]
  $ ./demoParse.exe <<-EOF
  > a[b], c = 3, 4, 5
  > EOF
  [(Set ([(Index ((Ident "a"), (Variable "b"))); (Ident "c")],
      [(Const (Number 3.)); (Const (Number 4.)); (Const (Number 5.))]))
    ]
  $ ./demoParse.exe <<-EOF
  > {} {b=3} {1} {1, 2} {1, b=2} {{}} {{}, 123} {[x] = x}
  > EOF
  [(Expr (TableInit []));
    (Expr (TableInit [(PairExpr ((Const (String "b")), (Const (Number 3.))))]));
    (Expr (TableInit [(JustExpr (Const (Number 1.)))]));
    (Expr
       (TableInit
          [(JustExpr (Const (Number 1.))); (JustExpr (Const (Number 2.)))]));
    (Expr
       (TableInit
          [(JustExpr (Const (Number 1.)));
            (PairExpr ((Const (String "b")), (Const (Number 2.))))]));
    (Expr (TableInit [(JustExpr (TableInit []))]));
    (Expr
       (TableInit [(JustExpr (TableInit [])); (JustExpr (Const (Number 123.)))]));
    (Expr (TableInit [(PairExpr ((Variable "x"), (Variable "x")))]))]
  $ ./demoParse.exe <<-EOF
  > a.b = 3 a.b.b.b = 3 a[b].c[d] = 3 a[4 + 1] = 3 + 2
  > EOF
  [(Set ([(Index ((Ident "a"), (Const (String "b"))))], [(Const (Number 3.))]));
    (Set (
       [(Index (
           (Index ((Index ((Ident "a"), (Const (String "b")))),
              (Const (String "b")))),
           (Const (String "b"))))
         ],
       [(Const (Number 3.))]));
    (Set (
       [(Index (
           (Index ((Index ((Ident "a"), (Variable "b"))), (Const (String "c"))
              )),
           (Variable "d")))
         ],
       [(Const (Number 3.))]));
    (Set (
       [(Index ((Ident "a"),
           (BinOp ((AOp Add), (Const (Number 4.)), (Const (Number 1.))))))
         ],
       [(BinOp ((AOp Add), (Const (Number 3.)), (Const (Number 2.))))]))
    ]
  $ ./demoParse.exe <<-EOF
  > if a then b elseif c then d else e end
  > if a then b end
  > if a then b else c end
  > for i=1,2,3 do end
  > for i=1,2 do end
  > while a do b end
  > repeat a until b
  > return
  > return 4
  > break
  > EOF
  [(If ((Variable "a"), [(Expr (Variable "b"))],
      [((Variable "c"), [(Expr (Variable "d"))])],
      (Some [(Expr (Variable "e"))])));
    (If ((Variable "a"), [(Expr (Variable "b"))], [], None));
    (If ((Variable "a"), [(Expr (Variable "b"))], [],
       (Some [(Expr (Variable "c"))])));
    (Fornum ("i", (Const (Number 1.)), (Const (Number 2.)),
       (Some (Const (Number 3.))), []));
    (Fornum ("i", (Const (Number 1.)), (Const (Number 2.)), None, []));
    (While ((Variable "a"), [(Expr (Variable "b"))]));
    (Repeat ([(Expr (Variable "a"))], (Variable "b"))); (Return None);
    (Return (Some (Const (Number 4.)))); Break]
  $ ./demoParse.exe <<-EOF
  > 1 "3" true b {}
  > a(1, {}, b)
  > a.b a[b] 
  > 1 + 2
  > 1 + 2 * 3
  > 1 * 2 + 3
  > 1 + 2 * 3 + 4 * 5 ^ 6
  > 1 + 2 * 3 - 4 - 5 - 6
  > 1 ^ 2 - 3 * 5 ^ 5 - 7
  > -5
  > -5 + 1
  > not false
  > not true or 4
  > not true and 4 or false
  > not true and not false or true
  > true and not false
  > EOF
  [(Expr (Const (Number 1.))); (Expr (Const (String "3")));
    (Expr (Const (Bool true))); (Expr (Variable "b")); (Expr (TableInit []));
    (StatementApply
       (Call ((Variable "a"),
          [(Const (Number 1.)); (TableInit []); (Variable "b")])));
    (Expr (TableGet ((Variable "a"), (Const (String "b")))));
    (Expr (TableGet ((Variable "a"), (Variable "b"))));
    (Expr (BinOp ((AOp Add), (Const (Number 1.)), (Const (Number 2.)))));
    (Expr
       (BinOp ((AOp Add), (Const (Number 1.)),
          (BinOp ((AOp Mul), (Const (Number 2.)), (Const (Number 3.)))))));
    (Expr
       (BinOp ((AOp Add),
          (BinOp ((AOp Mul), (Const (Number 1.)), (Const (Number 2.)))),
          (Const (Number 3.)))));
    (Expr
       (BinOp ((AOp Add),
          (BinOp ((AOp Add), (Const (Number 1.)),
             (BinOp ((AOp Mul), (Const (Number 2.)), (Const (Number 3.)))))),
          (BinOp ((AOp Mul), (Const (Number 4.)),
             (BinOp ((AOp Pow), (Const (Number 5.)), (Const (Number 6.))))))
          )));
    (Expr
       (BinOp ((AOp Sub),
          (BinOp ((AOp Sub),
             (BinOp ((AOp Sub),
                (BinOp ((AOp Add), (Const (Number 1.)),
                   (BinOp ((AOp Mul), (Const (Number 2.)), (Const (Number 3.))
                      ))
                   )),
                (Const (Number 4.)))),
             (Const (Number 5.)))),
          (Const (Number 6.)))));
    (Expr
       (BinOp ((AOp Add),
          (BinOp ((AOp Sub),
             (BinOp ((AOp Sub),
                (BinOp ((AOp Sub),
                   (BinOp ((AOp Sub),
                      (BinOp ((AOp Pow), (Const (Number 1.)),
                         (Const (Number 2.)))),
                      (BinOp ((AOp Mul), (Const (Number 3.)),
                         (BinOp ((AOp Pow), (Const (Number 5.)),
                            (Const (Number 5.))))
                         ))
                      )),
                   (Const (Number 7.)))),
                (Const (Number 5.)))),
             (Const (Number 5.)))),
          (Const (Number 1.)))));
    (Expr (UnOp (Not, (Const (Bool false)))));
    (Expr
       (BinOp ((LOp Or), (UnOp (Not, (Const (Bool true)))), (Const (Number 4.))
          )));
    (Expr
       (BinOp ((LOp Or),
          (BinOp ((LOp And), (UnOp (Not, (Const (Bool true)))),
             (Const (Number 4.)))),
          (Const (Bool false)))));
    (Expr
       (BinOp ((LOp Or),
          (BinOp ((LOp And), (UnOp (Not, (Const (Bool true)))),
             (UnOp (Not, (Const (Bool false)))))),
          (Const (Bool true)))));
    (Expr
       (BinOp ((LOp And), (Const (Bool true)),
          (UnOp (Not, (Const (Bool false)))))))
    ]
