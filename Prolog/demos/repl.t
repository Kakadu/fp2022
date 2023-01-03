  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > father_child(Father, Child).
  > N
  > N
  Father = tom,
  Child = sally ;
  Father = tom,
  Child = erica ;
  Father = mike,
  Child = tom.

  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > mother_child(X, Y).
  X = trude,
  Y = sally.

  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > father_child(mike, tom).
  true.

  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > mother_child(trude, sally).
  true.

  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > father_child(mike, tom).
  > mother_child(trude, sally).
  true.
  true.

  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > sibling(erica, tom).
  false.

  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > mother_child(trude, anne).
  false.

  $ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
  > parent_child(trude, Y).
  Y = sally.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(george,john).
  true.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(X,john).
  > N
  X = george .

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(john,john).
  false.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(X,john).
  > N
  > grand_father(john,john).
  X = george .
  false.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(X,john).
  > D
  > grand_father(john,john).
  X = george 
  false.


  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(john, sue).
  false.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > opp_sex(X,Y).
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  > N
  X = john,
  Y = sue ;
  X = john,
  Y = jane ;
  X = john,
  Y = june ;
  X = sam,
  Y = sue ;
  X = sam,
  Y = jane ;
  X = sam,
  Y = june ;
  X = george,
  Y = sue ;
  X = george,
  Y = jane ;
  X = george,
  Y = june ;
  X = sue,
  Y = john ;
  X = jane,
  Y = john ;
  X = june,
  Y = john ;
  X = sue,
  Y = sam ;
  X = jane,
  Y = sam ;
  X = june,
  Y = sam ;
  X = sue,
  Y = george ;
  X = jane,
  Y = george ;
  X = june,
  Y = george.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(sue, george).
  false.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(X,john).
  X = george 

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(X, Y).
  > N
  > N
  X = george,
  Y = john ;
  X = george,
  Y = jane .

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > grand_father(jane, Y).
  false.

  $ ../REPL.exe ../demos/examples/Test2.pl <<"EOF"
  > female(X).
  > N
  > N
  X = sue ;
  X = jane ;
  X = june.

  $ ../REPL.exe ../demos/examples/Test3.pl <<"EOF"
  > loves(X, mia).
  > N
  X = vincent ;
  X = marcellus.


  $ ../REPL.exe ../demos/examples/Test4.pl <<"EOF"
  > append([a], [b], X).
  X = [a, b].

  $ ../REPL.exe ../demos/examples/Test4.pl <<"EOF"
  > sublist([a, b, c, d, e], [c, d]).
  true.

  $ ../REPL.exe ../demos/examples/Test4.pl <<"EOF"
  > sublist([a, b, c, d, e], [c, g]).
  false.

  $ ../REPL.exe ../demos/examples/Test4.pl <<"EOF"
  > suffix([a,b,c],[b,c,d]).
  false.

  $ ../REPL.exe ../demos/examples/Test4.pl <<"EOF"
  > suffix([a,b,c],[b,c]).
  true.

  $ ../REPL.exe ../demos/examples/Test5.pl <<"EOF"
  > prove(good_pet(tweety)).
  > yes.
  > yes.
  > yes.
  has_feathers(tweety)
  tweets(tweety)
  small(tweety)
  true.

  $ ../REPL.exe ../demos/examples/Test5.pl <<"EOF"
  > prove(good_pet(tweety)).
  > no.
  > no.
  has_feathers(tweety)
  cuddly(tweety)
  false.

  $ ../REPL.exe ../demos/examples/Test5.pl <<"EOF"
  > prove(good_pet(tweety)).
  > no.
  > yes.
  has_feathers(tweety)
  cuddly(tweety)
  true.

  $ ../REPL.exe ../demos/examples/Test5.pl <<"EOF"
  > prove(good_pet(tweety)).
  > no.
  > yes.
  has_feathers(tweety)
  cuddly(tweety)
  true.

  $ ../REPL.exe ../demos/examples/Test5.pl <<"EOF"
  > prove(good_pet(tweety)).
  > yes.
  > no.
  > yes.
  has_feathers(tweety)
  tweets(tweety)
  cuddly(tweety)
  true.

  $ ../REPL.exe ../demos/examples/Test5.pl <<"EOF"
  > prove(good_pet(doggo)).
  > no.
  > yes.
  has_feathers(doggo)
  cuddly(doggo)
  false.

  $ ../REPL.exe ../demos/examples/Test5.pl <<"EOF"
  > prove(good_pet(doggo)).
  > yes.
  > yes.
  > yes.
  has_feathers(doggo)
  tweets(doggo)
  small(doggo)
  true.

  $ ../REPL.exe ../demos/examples/Test6.pl <<"EOF"
  > clause(cat, true).
  true.

  $ ../REPL.exe ../demos/examples/Test6.pl <<"EOF"
  > clause(dog, true).
  true.

  $ ../REPL.exe ../demos/examples/Test6.pl <<"EOF"
  > clause(insect(I), T).
  > N
  I = ant,
  T = true ;
  I = bee,
  T = true.

  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  > eliza([i, love, you], Response).
  Response = [why, do, you, love, me, "?"].

  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  > eliza([i, am, very, hungry], Response).
  Response = [why, are, you, very, hungry, "?"].

  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  > eliza([i, dunno, lol], Response).
  false.

  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  > eliza([i, dunno, lol], Response).
  false.

  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  > eliza([i, want, to, be, with, you], Response).
  Response = [why, do, you, want, to, be, with, me, "?"].

  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  > eliza([Nothing, to, say], Response).
  false.

  $ ../REPL.exe ../demos/examples/Test7.pl <<"EOF"
  > eliza([i, am], Response).
  false.

  $ ../REPL.exe ../demos/examples/unicamp/p01.pl <<"EOF"
  > my_last(X,[a,b,c,d]).
  > N
  X = d .

  $ ../REPL.exe ../demos/examples/unicamp/p01.pl <<"EOF"
  > my_last(X,[a,b,c,D]).
  > N
  X = D,
  D = X .

  $ ../REPL.exe ../demos/examples/unicamp/p01.pl <<"EOF"
  > my_last(X,[]).
  false.

  $ ../REPL.exe ../demos/examples/unicamp/p01.pl <<"EOF"
  > my_last(X,[1]).
  > N
  X = 1 .

  $ ../REPL.exe ../demos/examples/unicamp/p05.pl <<"EOF"
  > my_reverse([1,2,3], X).
  X = [3, 2, 1].

  $ ../REPL.exe ../demos/examples/unicamp/p05.pl <<"EOF"
  > my_reverse(["123",2,a], X).
  X = [a, 2, "123"].

  $ ../REPL.exe ../demos/examples/unicamp/p05.pl <<"EOF"
  > my_reverse(["123"], X).
  X = ["123"].

  $ ../REPL.exe ../demos/examples/unicamp/p05.pl <<"EOF"
  > my_reverse([], X).
  X = [].

######

$ ../REPL.exe ../demos/examples/unicamp/p05.pl <<"EOF"
> my_reverse([a,2,A], X).
X = [A, 2, a].

$ ../REPL.exe ../demos/examples/Test4.pl <<"EOF"
> sublist(Xs, Ys).
Xs = random,
Ys = [] 


$ ../REPL.exe ../demos/examples/Test.pl <<"EOF"
> wrong_pred(jane, anne, rise).
Error: No such predicate in DB for PI wrong_pred/3

$ ../REPL.exe ../demos/examples/Test6.pl <<"EOF"
> clause(legs(I, 6), Body).
false.

$ ../REPL.exe ../demos/examples/Test3.pl <<"EOF"
> jealous(X, Y).
> N
> N
> N
> N
> N
X = Y, Y = vincent ;
X = vincent,
Y = marcellus ;
X = marcellus,
Y = vincent ;
X = Y, Y = marcellus ;
X = Y, Y = pumpkin ;
X = Y, Y = honey_bunny.


