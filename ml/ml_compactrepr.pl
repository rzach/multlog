%%% Filename : ml.pl
%%% Date     : 14.6.2001
%%% Contents : Conversion routines for efficient clause representations
%%% Author   : Stefan Kral and Stefan Katzenbeisser
%%% Interface: repr_to_mynf/2, mynf_to_repr/2 translate standard
%%%            representation of clauses to packed form (bistrings)
%%%            and vice versa. 

%%% Changes: 14/6/2001 (katz) fixed a bug in mynf_to_repr3_/3

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% repr_to_mynf/2 & mynf_to_repr/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 	switch from std. representation of clauses to 
%	more compact form (and vice-versa)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

repr_to_mynf(Rs, Ns) :-
	lgcTVs(TVs), 
	repr_to_mynf(Rs, TVs, Ns).

repr_to_mynf([], _TVs, []).
repr_to_mynf([R|Rs], TVs, [Z|Zs]) :-
	repr_to_mynf2(R, TVs, Z),
	repr_to_mynf(Rs, TVs, Zs).

repr_to_mynf2([], _TVs, []).
repr_to_mynf2([A|As], TVs, [X|Xs]) :-
	repr_to_mynf3(A, TVs, X),
	repr_to_mynf2(As, TVs, Xs).

repr_to_mynf3([], _TVs, 0).
repr_to_mynf3([A|As], TVs, V) :-
	ml_nth0(N, TVs, A),
	V0 is 1<<N,
	repr_to_mynf3_(As, TVs, V0, V).

repr_to_mynf3_([], _TVs, V, V).
repr_to_mynf3_([A|As], TVs, V0, V) :-
	ml_nth0(N, TVs, A),
	V1 is V0\/(1<<N),
	repr_to_mynf3_(As, TVs, V1, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mynf_to_repr(Ns, Rs) :-
	lgcTVs(TVs),
	mynf_to_repr(Ns, TVs, Rs).

mynf_to_repr([], _TVs, []).
mynf_to_repr([N|Ns], TVs, [Z|Zs]) :-
	mynf_to_repr2(N, TVs, Z),
	mynf_to_repr(Ns, TVs, Zs).

mynf_to_repr2([], _TVs, []).
mynf_to_repr2([A|As], TVs, [X|Xs]) :-
	mynf_to_repr3_(TVs, A, X),
	mynf_to_repr2(As, TVs, Xs).

mynf_to_repr3_([], _V, []).
mynf_to_repr3_([T|Ts], V, Ls0) :-
       (  V /\ 1 =:= 0
       -> Ls0 = Ls
       ;  Ls0 = [T|Ls]
       ),
       V0 is V>>1,
       mynf_to_repr3_(Ts, V0, Ls).
