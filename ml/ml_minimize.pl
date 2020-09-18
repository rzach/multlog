% -*- fundamental -*-

%%% Filename : ml_minimize.pl
%%% Date     : 3.2.2001
%%% Contents : Minimization of operators
%%% Author   : Stefan Katzenbeisser
%%% Interface: minimize/2

sublist_(X, Subset, N) :-
	length(Subset, N),
	sublist(Subset, X).

minimize(X,Y) :-
%	write('Minimizing: '), write(X), nl,
	repr_to_mynf(X,Xrep),
	formula_minimized(Xrep,Yrep),
	mynf_to_repr(Yrep, Y).
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
