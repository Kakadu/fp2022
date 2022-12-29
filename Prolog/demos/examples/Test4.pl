append([],L,L).
append([H1|T],L2,[H1|L3]) :- append(T,L2,L3).

suffix(Xs, Ys) :-
    append(_, Ys, Xs).

prefix(Xs, Ys) :-
    append(Ys, _, Xs).

sublist(Xs, Ys) :-
    suffix(Xs, Zs),
    prefix(Zs, Ys).

nrev([], []).
nrev([H|T0], L) :-
	nrev(T0, T),
	append(T, [H], L).
