%%% Filename : ml_minimize.pl
%%% Date     : 3.2.2001
%%% Contents : Minimization of operators
%%% Author   : Stefan Katzenbeisser
%%% Interface: minimize/2
%%%
%%% 9.5.2021: caching added

sublist_(X, Subset, N) :-
	length(Subset, N),
	sublist(Subset, X).

minimize(X,Y) :-
    cached(minimize(X,Y)), !.
minimize(X,Y) :-
%	write('Minimizing: '), write(X), nl,
	repr_to_mynf(X,Xrep),
	formula_minimized(Xrep,Yrep),
	mynf_to_repr(Yrep, Y),
	assertz(cached(minimize(X,Y))).
%	write('Solution: '), write(Y), nl.

formula_minimized(Xs, Zs) :-
	saturate(Xs, ResClosureXs),
	sort(ResClosureXs, ResSorted),
	length(_,N),
%	format('trying N=~d.\n', [N]), flush_output,
	sublist_(ResSorted, Subset, N),
	saturate(Subset, SubsetClosure),
	sort(SubsetClosure, ResSorted),			% equal?
	!,
	Zs = Subset.
