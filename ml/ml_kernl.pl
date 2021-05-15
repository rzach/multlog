%%% Filename : ml_kernl.pl
%%% Date     : 20.8.1997
%%% Contents : MULTLOG kernel. The main computations. The reason why MULTLOG exists.
%%% Author   : Gernot Salzer
%%% Interface:
%%%            kernel.       Perform various computations.
%%%
%%% 2021/05/15 (GS): caching added

:- dynamic   krnTautology/1, krnOpIntro/3, krnQuIntro/3.

krnDatabase([krnTautology/1, krnOpIntro/3, krnQuIntro/3]).

kernel :-
   \+ errOccurred(_,_),
   clear(krnDatabase),
   chkTVs(TVs),
   make_krnTautology(TVs, Taut),
   assert(krnTautology(Taut)),
   (chkOp(N, S),
       ml_member(V, TVs),
%    println_terms(['*************** ', N, ': ', V, ' ***************']),
%    println_terms(['S:    ', S]),
       opmap2cnf(S, V, TVs, Cnf),
%    println_terms(['Cnf:  ', Cnf]),
	minimize(Cnf, SCnf),
%    println_terms(['SCnf: ', SCnf]),
	single_lits(SCnf, SSCnf),
%    println_terms(['SSCnf: ', SSCnf]),
       assertz(krnOpIntro(N, V, SSCnf)),
       fail
   ;   true
   ),
   (chkQu(N, S),
       ml_member(V, TVs),
%    println_terms(['*************** ', N, ': ', V, ' ***************']),
%    println_terms(['S:     ', S]),
       qumap2pcnf([[]:dum(my)|S], V, TVs, PCnf),
%    println_terms(['PCnf:  ', PCnf]),
       simple_saturation(PCnf, SPCnf),
%    println_terms(['SPCnf: ', SPCnf]),
       qupcnf2cnf(SPCnf, TVs, Cnf),
%    println_terms(['Cnf:   ', Cnf]),
       saturation(Cnf, SCnf),
%    println_terms(['SCnf:  ', SCnf]),
       assertz(krnQuIntro(N, V, SCnf)),
       fail
   ;   true
   ).

% opmap2cnf(Map, V, TVs, Cnf).    Computes the Cnf for truthvalue V wrt. operator mapping Map
opmap2cnf([], _, _, []).
opmap2cnf([_:V|Map], V, TVs, Cnf) :- !,
   opmap2cnf(Map, V, TVs, Cnf).
opmap2cnf([Vs:_|Map], V, TVs, [Disj|Cnf]) :-
   negate_lits(Vs, TVs, Disj),
   opmap2cnf(Map, V, TVs, Cnf).

negate_lits([], _TVs, []).
negate_lits([V|Vs], TVs, [D|Ds]) :-
	remove_member(V, TVs, D),
	negate_lits(Vs, TVs, Ds).

single_lits([], []).
single_lits([C|Cs], [D|Ds]) :-
	single_lits(C, 1, D, []),
	single_lits(Cs, Ds).

single_lits([], _, S, S).
single_lits([L|Ls], N0, S0, S) :-
	pair_lits(L, N0, S0, S1),
	N1 is N0+1,
	single_lits(Ls, N1, S1, S).

pair_lits([], _, S, S).
pair_lits([V|Vs], N, [N^V|S0], S) :-
	pair_lits(Vs, N, S0, S).
	
% qumap2pcnf(Map, V, TVs, Cnf). Constructs a propositional CNF for quantifier mapping Map and
%                          truth value V.
%                          If Map contains an entry Vs:V' and V' is different
%                          from V, then there is a clause in Cnf, which contains a literal
%                          -v for every v in Vs and a literal +v for every v in TVs-Vs
%                          (TVs is the set of all truth values).
qumap2pcnf([], _, _, []).
qumap2pcnf([_:V|Map], V, TVs, Cnf) :- !,
   qumap2pcnf(Map, V, TVs, Cnf).
qumap2pcnf([Vs:_|Map], V, TVs, [Disj|Cnf]) :-
   quset2pdisj(TVs, Vs, Disj),
   qumap2pcnf(Map, V, TVs, Cnf).

quset2pdisj([], _, []).
quset2pdisj([V|TVs], [V|Vs], [V^(-)|Disj]) :- !,
   quset2pdisj(TVs, Vs, Disj).
quset2pdisj([V|TVs], Vs, [V^(+)|Disj]) :-
   quset2pdisj(TVs, Vs, Disj).

% qupcnf2cnf(PCnf, TVs, Cnf).   Converts a propositional clause set (constructed by qumap2pcnf)
%                          to a non-propositional clause set.
qupcnf2cnf(PCnf, TVs, Cnf) :-
   qupcnf2cnf(PCnf, 1, TVs, Cnf).

qupcnf2cnf([], _, _, []).
qupcnf2cnf([PD|PCnf], N0, TVs, [D|Cnf]) :-
   qupdisj2disj(PD, N0, TVs, D0),
   sort(D0,D),
   N1 is N0 + 1,
   qupcnf2cnf(PCnf, N1, TVs, Cnf).

qupdisj2disj([], _, _, []).
qupdisj2disj([V^(+)|PD], N, TVs, [N^V|D]) :-
   qupdisj2disj(PD, N, TVs, D).
qupdisj2disj([V^(-)|PD], N, TVs, D0) :-
   negate_lit(TVs, V, _AlphaV, D0, D1),
   qupdisj2disj(PD, N, TVs, D1).

% simple_resolvent(C, D, R)         R is the simple resolvent of C and D
%    C and D must be ground and sorted by predicate and sign
%    A simple resolvent is a resolvent of clauses differing in exactly one literal
%    (= the resolved literal).
%    simple_resolvent succeeds at most once.
simple_resolvent([L|C], [L|D], [L|R]) :- !,
   simple_resolvent(C, D, R).
simple_resolvent([N^_|C], D, C) :-
   remove_member(N^_, D, C).
simple_resolvent(C, [N^_|D], D) :-
   remove_member(N^_, C, D).

simple_saturation(Cs0, Cs) :-
   cached(simple_saturation(Cs0, Cs)), !.
simple_saturation(Cs0, Cs) :-
   setof(R, C^Ds^D^(tail(Cs0,[C|Ds]), ml_member(D,Ds), simple_resolvent(C,D,R)), Rs0), !,
   simple_saturation(Rs0, Rs1),
   ground_subsumption(Cs0, Rs1, Cs1),
   ml_append(Cs1, Rs1, Cs),
   assertz(cached(simple_saturation(Cs0, Cs))).
simple_saturation(Cs, Cs).

ground_subsumption([], _, []).
ground_subsumption([C|Cs0], Ds, Cs) :-
   ml_member(D, Ds),
   sublist(D, C), !,                  % Subsumption test. C and D have to be ground and sorted.
   ground_subsumption(Cs0, Ds, Cs).
ground_subsumption([C|Cs0], Ds, [C|Cs]) :-
   ground_subsumption(Cs0, Ds, Cs).

resolvent(C0, D0, R) :-
   remove_member(N^V, C0, C1),
   remove_member(N^W, D0, D1),
   V \== W,
   ml_append(C1, D1, R0),
   sort(R0, R).

factor(C, F) :-
   factor(C),
   sort(C, F).

factor(C) :-
   tail(C, [L|D]),
   ml_member(L, D),
   factor1(D).

factor1(_C).
factor1(C) :-
  factor(C).

saturation(Cs0, Cs) :-
   cached(saturation(Cs0, Cs)), !.
saturation(Cs0, Cs) :-
   saturation([Cs0|Css], Css, Cs1, Cs1),
   remove_factors(Cs1),
   findall(C, (ml_member(C-Flag, Cs1), var(Flag)), Cs),
   assertz(cached(saturation(Cs0, Cs))).

saturation(Css, CssEnd, _, []) :-
   Css == CssEnd, !.
saturation([Cs|Css], CssEnd, Ds, DsEnd) :-
   saturation(Cs, Css, CssEnd, Ds, DsEnd).

saturation([], Css, CssEnd, Ds, DsEnd) :-
   saturation(Css, CssEnd, Ds, DsEnd).
saturation([C|Cs], Css, CssEnd, Ds, DsEnd) :-
   krnTautology(T),
   tail(C, T), !,
   saturation(Cs, Css, CssEnd, Ds, DsEnd).
saturation([C|Cs], Css, CssEnd, Ds, DsEnd) :-
   ol_member(D-Flag, Ds),
   var(Flag),
   subsumes(D, C), !,
   saturation(Cs, Css, CssEnd, Ds, DsEnd).
saturation([C|Cs], Css, [Rs|CssEnd], Ds, DsEnd) :-
   backward_subsumption(Ds, C),
%   copy_term(C, CC),
%   findall(R, (factor(CC,R); ol_member(D-Flag,Ds), var(Flag), resolvent(CC,D,R)), Rs),
   findall(R, (factor(C,R); ol_member(D-Flag,Ds), var(Flag), resolvent(C,D,R)), Rs),
   DsEnd = [C-_Mark|DsEnd1],
   saturation(Cs, Css, CssEnd, Ds, DsEnd1).

backward_subsumption(Ds, _C) :-
   var(Ds), !.
backward_subsumption([D-Flag|Ds], C) :-
   var(Flag),
   subsumes(C, D), !,
   Flag = deleted,
   backward_subsumption(Ds, C).
backward_subsumption([_|Ds], C) :-
   backward_subsumption(Ds, C).

make_krnTautology(TVs, Taut) :-
   make_krnTautology(TVs, _N, Taut).

make_krnTautology([], _, _Taut).
make_krnTautology([TV|TVs], N, [N^TV|Taut]) :-
   make_krnTautology(TVs, N, Taut).

subsumes(C, R) :-
   is_not_longer_than(C, R),
   \+ \+ (bind_vars(R),
          ml_subset(C,R)).

is_not_longer_than([], _R).
is_not_longer_than([_|C], [_|R]) :-
   is_not_longer_than(C, R).

bind_vars(R) :-
   bind_vars(R, 1).

bind_vars([], _).
bind_vars(['_'(N0)^_|R], N0) :- !,
   N1 is N0+1,
   bind_vars(R, N1).
bind_vars([_|R],N) :-
   bind_vars(R,N).

negate_lit([], _, _, NLs, NLs).
negate_lit([V|TVs], V, N, NLs0, NLs) :- !,
   negate_lit(TVs, V, N, NLs0, NLs).
negate_lit([TV|TVs], V, N, [N^TV|NLs0], NLs) :-
   negate_lit(TVs, V, N, NLs0, NLs).

remove_factors(Cs) :-
	rfs(Cs, []).

rfs([], _).
rfs([_-M|Cs], Ds) :-
	nonvar(M), !,
	rfs(Cs, Ds).
rfs([CM|Cs], Ds) :-
	CM = C-_,
	findall(F, factor(C,F), Fs),
	rfs1(Fs, Cs),
	rfs1(Fs, Ds),
	rfs(Cs, [CM|Ds]).

rfs1([], _).
rfs1([F|Fs], Cs) :-
	rfs2(Cs, F),
	rfs1(Fs, Cs).

rfs2([], _).
rfs2([_-M|Cs], F) :-
	nonvar(M), !,
	rfs2(Cs, F).
rfs2([C-deleted|Cs], F) :-
	subsumes(F, C), !,
	rfs2(Cs, F).
rfs2([_|Cs], F) :-
	rfs2(Cs, F).
