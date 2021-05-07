%%% Filename : ml_check.pl
%%% Date     : 6.5.2021
%%% Contents : Checks the syntax of all names in the specification.
%%%            Checks completeness and consistency.
%%% Author   : Gernot Salzer
%%% Interface: check.   Performs various checks on the specification loaded in the database.

:- set_prolog_flag(double_quotes, codes).
:- dynamic   chkTVs/1, chkOrd/2, chkSup/2, chkInf/2, chkOp/2, chkIsACI/1, chkQu/2.

chkDatabase([chkTVs/1, chkOrd/2, chkSup/2, chkInf/2, chkOp/2, chkIsACI/1, chkQu/2]).

check :-
	\+ errOccurred(openerror, _),
	\+ errOccurred(internal, _),
	!,
	clear(chkDatabase),
	check_LN,
	check_TVs,
	check_DTVs,
	check_orderings,
	check_operators,
	check_quantifiers.
check.

check_LN :-
	% Is there at least one "logic"?
	check(lgcLN(_), missingLN),
	% Is there at most  one "logic"?
	check(bagof(1,N^lgcLN(N),[1]), multLN),
	!.
check_LN.

check_TVs :-
	% Is there at least one "truth_values"?
	check(lgcTVs(_), missingTVs),
	% Is there at most  one "truth_values"?
	check(bagof(1,N^lgcTVs(N),[1]), multTVs),
	lgcTVs(TVs),
	% Does any truth value occur twice in TVs?
	check(ml_is_set(TVs), multTV),
	% Are there at least two truth values?
	check((ml_length(TVs,N), N>1), notEnoughTVs),
	% Are there less than 20 truth values?
	check((ml_length(TVs,N), N=<20), toomanyTVs),
	sort(TVs, TVs1), %%% DO NOT CHANGE ORDER OF TRUTH VALUES!
	assert(chkTVs(TVs1)),
	!.
check_TVs.

check_DTVs :-
	% Is there at least one "designated_truth_values"?
	check(lgcDTVs(_), missingDTVs),
	% Is there at most one "designated_truth_values"?
	check(bagof(1,N^lgcDTVs(N),[1]), multDTVs),
	lgcDTVs(DTVs),
	% Does any truth value occur twice in DTVs?
	check(ml_is_set(DTVs), multDTV),
	% Have all truth values been declared in "truth_values"?
	lgcTVs(TVs),
	diff(DTVs, TVs, Undecl),
	check(Undecl=[], undeclDTVs(Undecl)),
	!.
check_DTVs.

check_orderings :-
	bagof(1, S^lgcOrd1(N,S), Ss),
	   % Is the ordering declared twice?
	   check(Ss=[1], multOrd(N)),
	   lgcOrd1(N, S),
	   ordspec2rel(S,R,OTVs1),
	   sort(OTVs1, OTVs2),
	   chkTVs(TVs),
	   % Have all truth values been declared in "truth_values"?
	   diff(OTVs2, TVs, Undecl),
	   check(Undecl=[], undeclOTVs(N, Undecl)),
	   % Do all truth values occur in the specification?
	   diff(TVs, OTVs2, Unused),
	   check(Unused=[], unusedOTVs(N, Unused)),
	   transitive_closure(R, C),
	   % Is the transitive closure irreflexive?
	   check(\+ ml_member(X<X,C), nonIrreflOrd(N)),
	   assertz(chkOrd(N,C)),
	   ml_if(inf_op(C, Infop), assertz(chkInf(N, Infop)), true),
	   ml_if(sup_op(C, Supop), assertz(chkSup(N, Supop)), true),
	   % If the ordering induces neither an inf nor an sup operator, it's useless.
	   check((nonvar(Infop);nonvar(Supop)), uselessOrd(N)),
	fail.
check_orderings.

ordspec2rel(S, R, TVs) :-
	ordspec2rel(S, R, [], [], _, TVs, []).

ordspec2rel([], R, R, Vs, Vs, TVs, TVs).
ordspec2rel([El|S], R0, R, Vs0, Vs, TVs0, TVs) :-
	ordel2rel(El, R0, R1, Vs0, Vs1, TVs0, TVs1),
	ordspec2rel(S, R1, R, Vs1, Vs, TVs1, TVs).

ordel2rel(tv(V), R0, R, Vs, [V], [V|TVs], TVs) :-
	make_pairs(Vs, V, R0, R).
ordel2rel([], R, R, _, [], TVs, TVs).
ordel2rel([S|Ss], R0, R, Vs0, Vs, TVs0, TVs) :-
	ordspec2rel(S, R0, R1, Vs0, Vs1, TVs0, TVs1),
	ordel2rel(Ss, R1, R, Vs0, Vs2, TVs1, TVs),
	ml_append(Vs1, Vs2, Vs).

make_pairs([], _, R, R).
make_pairs([V1|Vs], V2, [V1<V2|R0], R) :-
	make_pairs(Vs, V2, R0, R).

transitive_closure(R, TR) :-
	transitive_closure(R, [], TR).

transitive_closure([], TR, TR).
transitive_closure([B<C|R], TR0, TR) :-
	ml_member(B<C, TR0), !,
	transitive_closure(R, TR0, TR).
transitive_closure([P|R0], TR0, TR) :-
	transitive_closure(TR0, P, R0, R1),
	transitive_closure(R1, [P|TR0], TR).

transitive_closure([], _, R, R).
transitive_closure([P1|TR], P2, R0, R) :-
	combine(P1, P2, R0, R1),
	combine(P2, P1, R1, R2),
	transitive_closure(TR, P2, R2, R).

combine(A<B, B<C, R, [A<C|R]) :- !.
combine(_, _, R, R).

inf_op(C, Op) :-
	chkTVs(TVs),
	inf_op(TVs, C, Op0),
	sort(Op0, Op).

inf_op([], _R, []).
inf_op([V|Vs], R, [[V,V]:V|Op0]) :-
	inf_op(Vs, V, R, Op0, Op1),
	inf_op(Vs, R, Op1).

inf_op([], _V, _R, Op, Op).
inf_op([V2|Vs], V1, R, [[V2,V1]:V, [V1,V2]:V|Op0], Op) :-
	inf(V1, V2, R, V),
	inf_op(Vs, V1, R, Op0, Op).

inf(V, V, _R, V).
inf(V1, V2, R, V) :-
	min(R, V1, V2, V), !.
inf(V1, V2, R, Inf) :-
	bagof(V, (ml_member(V<V1,R), ml_member(V<V2,R)), LBs),
	maxs(LBs, R, [Inf]).

sup_op(C, Op) :-
	chkTVs(TVs),
	sup_op(TVs, C, Op0),
	sort(Op0, Op).

sup_op([], _R, []).
sup_op([V|Vs], R, [[V,V]:V|Op0]) :-
	sup_op(Vs, V, R, Op0, Op1),
	sup_op(Vs, R, Op1).

sup_op([], _V, _R, Op, Op).
sup_op([V2|Vs], V1, R, [[V2,V1]:V, [V1,V2]:V|Op0], Op) :-
	sup(V1, V2, R, V),
	sup_op(Vs, V1, R, Op0, Op).

sup(V, V, _R, V).
sup(V1, V2, R, V) :-
	max(R, V1, V2, V), !.
sup(V1, V2, R, Sup) :-
	bagof(V, (ml_member(V1<V,R), ml_member(V2<V,R)), UBs),
	mins(UBs, R, [Sup]).

mins(Vs, R, Ms) :-
    mins(Vs, Vs, R, Ms).

mins([], _Vs, _R, []).
mins([V0|Vs0], Vs, R, Ms) :-
    ml_member(V, Vs),
    ml_member(V < V0, R), !,
	mins(Vs0, Vs, R, Ms).
mins([M|Vs0], Vs, R, [M|Ms]) :-
    mins(Vs0, Vs, R, Ms).

min([V1<V2|_R], V1, V2, V1) :- !.
min([V2<V1|_R], V1, V2, V2) :- !.
min([_|R], V1, V2, V) :-
	min(R, V1, V2, V).

maxs(Vs, R, Ms) :-
    maxs(Vs, Vs, R, Ms).

maxs([], _Vs, _R, []).
maxs([V0|Vs0], Vs, R, Ms) :-
    ml_member(V, Vs),
    ml_member(V0 < V, R), !,
	maxs(Vs0, Vs, R, Ms).
maxs([M|Vs0], Vs, R, [M|Ms]) :-
    maxs(Vs0, Vs, R, Ms).

max([V1<V2|_R], V1, V2, V2) :- !.
max([V2<V1|_R], V1, V2, V1) :- !.
max([_|R], V1, V2, V) :-
	max(R, V1, V2, V).

check_operators :-
	bagof(1, S^lgcOp(N,S), Ss),
	   % Is the operator declared twice?
	   check(Ss=[1], multOp(N)),
	   lgcOp(N, S),
	   S =.. [Kind, X],
	   check_opspec(Kind, X, N),
	fail.
check_operators.

check_opspec(map, M, Op) :-
	Op = _/A,
	chkTVs(TVs),
	bagof(Vs:V, (ml_length(Vs,A),ml_subset(Vs,TVs)), Fs),
	diff(M, Fs, MRest),
	findall(Vs, (ml_member(Vs:V,Fs), var(V)), FsRest),
	% Is the mapping complete?
	check(FsRest=[], missingOpEntries(Op, FsRest)),
	% Does it contain duplicate entries?
	check(MRest =[], multOpEntries(Op, MRest)),
	assertz(chkOp(Op, Fs)),
	ml_if(isACI(Fs), assertz(chkIsACI(Op)), true).
check_opspec(tab, T, Op) :-
	% Is it a binary operator?
	check(Op=_/2, noBinOp(Op)),
	chkTVs(TVs),
	% Does the table have the right number of entries?
	ml_length(TVs, N),
	ml_length(T,  TEls),
	check(TEls is N*(N+2), wrongNoTEls(Op)),
	tab2map(T, N, M),
	check_opspec(map, M, Op).
check_opspec(inf, Ord, Op) :-
	% Is the arity greater than one?
	Op = _/A,
	check(A>1, wrongArityInfOp(Op)),
	% Has the ordering been declared?
	check(lgcOrd(Ord, _), missingOpOrd(Op,Ord)),
	% Does it induce an inf operator?
	check(chkInf(Ord, _), noInfOpOrd(Op,Ord)),
	chkTVs(TVs),
	chkInf(Ord, M),
	ml_length(Vs, A),
	bagof(Vs:V, (ml_subset(Vs,TVs), evalCI(M,Vs,V)), Fs),
	assertz(chkOp(Op, Fs)),
	assertz(chkIsACI(Op)).
check_opspec(sup, Ord, Op) :-
	% Is the arity greater than one?
	Op = _/A,
	check(A>1, wrongAritySupOp(Op)),
	% Has the ordering been declared?
	check(lgcOrd(Ord, _), missingOpOrd(Op,Ord)),
	% Does it induce a sup operator?
	check(chkSup(Ord, _), noSupOpOrd(Op,Ord)),
	chkTVs(TVs),
	chkSup(Ord, M),
	ml_length(Vs, A),
	bagof(Vs:V, (ml_subset(Vs,TVs), evalCI(M,Vs,V)), Fs),
	assertz(chkOp(Op, Fs)),
	assertz(chkIsACI(Op)).

tab2map(T0, N, M) :-
	ml_length(HL, N),
	ml_append(HL, T1, T0),
	tab2map(T1, [], _, HL, M).

tab2map([], _Ys, _X, _HL, []).
tab2map([X|M0], [], _X, HL, M) :-
	tab2map(M0, HL, X, HL, M).
tab2map([V|M0], [Y|Ys], X, HL, [[X,Y]:V|M]) :-
	tab2map(M0, Ys, X, HL, M).

isACI(Op) :-
	Op = [Vs:_|_],
	ml_length(Vs,A),
	ml_length(Args,A),
	Args = [X|Ys],
	ml_subset(Ys,[Y]),
	chkTVs(TVs),
	bagof([X,Y]:V, TVs1^(tail(TVs,[X|TVs1]), ml_member(Y,TVs1), ml_member(Args:V,Op)), Aci),
	\+ (tail(TVs, [X|TVs1]),
	    tail([X|TVs1], [Y|TVs2]),
	    ml_member(Z, TVs2),
	    evalCI(Aci, [X,Y,Z], V1),
	    evalCI(Aci, [X,Z,Y], V2),
	    evalCI(Aci, [Y,Z,X], V3),
	    (V1\==V2; V2\==V3)
	   ),
	check_aci(Op, Aci).

check_aci([], _).
check_aci([Xs:V|Op], Aci) :-
	evalCI(Aci, Xs, V),
	check_aci(Op, Aci).

evalCI(CI, [X|Ys], V) :-
	evalCI1(Ys, X, CI, V).

evalCI1([], X, _CI, X).
evalCI1([X|Ys], X, CI, V) :- !,
	evalCI1(Ys, X, CI, V).
evalCI1([Y|Ys], X, CI, V) :-
	evalC(CI, X, Y, Z),
	evalCI1(Ys, Z, CI, V).

evalC([[X,Y]:V|_], X, Y, V) :- !.
evalC([[Y,X]:V|_], X, Y, V) :- !.
evalC([_|C], X, Y, V) :-
	evalC(C, X, Y, V).

check_quantifiers :-
	bagof(1, S^lgcQu(N,S), Ss),
	   % Is the quantifier declared twice?
	   check(Ss=[1], multQu(N)),
	   lgcQu(N, S),
	   S =.. [Kind, X],
	   check_quspec(Kind, X, N),
	fail.
check_quantifiers.

check_quspec(map, M, Qu) :-
	chkTVs(TVs),
	bagof(Vs:V, L^(sublist(L,TVs), L\==[], sort(L,Vs)), Fs),
	diff(M, Fs, MRest),
	findall(Vs, (ml_member(Vs:V,Fs), var(V)), FsRest),
	% Is the mapping complete?
	check(FsRest=[], missingQuEntries(Qu, FsRest)),
	 % Does it contain duplicate entries?
	check(MRest =[], multQuEntries(Qu, MRest)),
	assertz(chkQu(Qu, Fs)).
check_quspec(op, Op, Qu) :-
	% Has the operator been declared?
	check(lgcOp(Op, _), missingQuOp(Qu,Op)),
	% Is it ACI?
	check(chkIsACI(Op), noACIQuOp(Qu,Op)),
	chkTVs(TVs),
	chkOp(Op, M),
	bagof(Vs:V, L^(sublist(L,TVs), L\==[], sort(L,Vs), evalCI(M,Vs,V)), Fs),
	assertz(chkQu(Qu, Fs)).
check_quspec(inf, Ord, Qu) :-
	% Has the ordering been declared?
	check(lgcOrd(Ord, _), missingQuOrd(Qu,Ord)),
	% Does it induce an inf operator?
	check(chkInf(Ord, _), noInfQuOrd(Qu,Ord)),
	chkTVs(TVs),
	chkInf(Ord, M),
	bagof(Vs:V, L^(sublist(L,TVs), L\==[], sort(L,Vs), evalCI(M,Vs,V)), Fs),
	assertz(chkQu(Qu, Fs)).
check_quspec(sup, Ord, Qu) :-
	% Has the ordering been declared?
	check(lgcOrd(Ord, _), missingQuOrd(Qu,Ord)),
	% Does it induce a sup operator?
	check(chkSup(Ord, _), noSupQuOrd(Qu,Ord)),
	chkTVs(TVs),
	chkSup(Ord, M),
	bagof(Vs:V, L^(sublist(L,TVs), L\==[], sort(L,Vs), evalCI(M,Vs,V)), Fs),
	assertz(chkQu(Qu, Fs)).

check(Test, _Err) :-
	\+ \+ Test, !.
check(_Test, Err) :-
	phrase(chk_errmess(Err,Type), Mess),
	error(Type, [Mess]),
	!, fail.

chk_errmess(missingLN,	semantics) --> "Declaration 'logic' is missing.".
chk_errmess(multLN,	semantics) --> "Several declarations 'logic' found.".
chk_errmess(missingTVs,	semantics) --> "Declaration 'truth_values' is missing.".
chk_errmess(multTVs,	semantics) --> "Several declarations 'truth_values' found.".
chk_errmess(multTV,	semantics) --> "Multiple occurrences of truth value in ",
				       "'truth_values'.".
chk_errmess(notEnoughTVs,semantics)--> "Less than two truth values in 'truth_values'.".
chk_errmess(toomanyTVs,  semantics)--> "Too many truth values (maximum: 20).".
chk_errmess(missingDTVs,semantics) --> "Declaration 'designated_truth_values' is missing.".
chk_errmess(multDTVs,	semantics) --> "Several declarations 'designated_truth_values' found.".
chk_errmess(multDTV,	semantics) --> "Multiple occurrences of some truth value ",
				       "'designated_truth_values'.".
chk_errmess(undeclDTVs(Undecl), semantics) -->
	"The designated truth value", s(Undecl), " ",
        chk_alist(Undecl), " ", to_be(Undecl), " not declared in 'truth_values'.".
chk_errmess(multOrd(N),	semantics(N/ord)) -->
	"Multiple declarations.".
chk_errmess(undeclOTVs(N, Undecl), semantics(N/ord)) -->
	chk_alist(Undecl), " ", to_be(Undecl), " not declared in 'truth_values'.".
chk_errmess(unusedOTVs(N, Unused), semantics(N/ord)) -->
	chk_alist(Unused), " unused".
chk_errmess(nonIrreflOrd(N), semantics(N/ord)) -->
	"Should be irreflexive, but it isn't.".
chk_errmess(uselessOrd(N), semantics(N/ord)) -->
	"Neither complete for 'inf' nor for 'sup'.".
chk_errmess(multOp(Op), semantics(Op)) -->
	"Multiple declarations.".
chk_errmess(missingOpEntries(Op, FsRest), semantics(Op)) -->
	"No entr", ies(FsRest),	" for ", chk_entries(FsRest, "(", ")"), ".".
chk_errmess(multOpEntries(Op, MRest), semantics(Op)) -->
	"Superfluous entr", ies(MRest),  " ", chk_entries(MRest,  "(", ")"), ".".
chk_errmess(noBinOp(Op), semantics(Op)) -->
	" has to be binary to be definable by a table.".
chk_errmess(wrongNoTEls(Op), semantics(Op)) -->
	"Wrong number of table entries.".
chk_errmess(wrongArityInfOp(Op), semantics(Op)) -->
	"Needs arity greater than one to be definable by 'inf'.".
chk_errmess(wrongAritySupOp(Op), semantics(Op)) -->
	"Needs arity greater than one to be definable by 'sup'.".
chk_errmess(missingOpOrd(Op,Ord), semantics(Op)) -->
	"Ordering '", str(Ord), "' is undefined.".
chk_errmess(noInfOpOrd(Op,Ord), semantics(Op)) -->
	"Ordering '", str(Ord), "' is not complete for 'inf'.".
chk_errmess(noSupOpOrd(Op,Ord), semantics(Op)) -->
	"Ordering '", str(Ord), "' is not complete for 'sup'.".
chk_errmess(multQu(Qu), semantics(Qu/q)) -->
	"Multiple declarations.".
chk_errmess(missingQuEntries(Qu, FsRest), semantics(Qu/q)) -->
	"No entr", ies(FsRest), " for ", chk_entries(FsRest, "{", "}"), ".".
chk_errmess(multQuEntries(Qu, MRest), semantics(Qu/q)) -->
	"Superfluous entr", ies(MRest),  " ", chk_entries(MRest,  "{", "}"), ".".
chk_errmess(missingQuOp(Qu,Op/A), semantics(Qu/q)) -->
	"Operator '", str(Op), "/", str(A), "' is undefined.".
chk_errmess(missingQuOrd(Qu,Ord), semantics(Qu/q)) -->
	"Ordering '", str(Ord), "' is undefined.".
chk_errmess(noACIQuOp(Qu,Op/A), semantics(Qu/q)) -->
	"Operator '", str(Op), "/", str(A), "' is not ACI.".
chk_errmess(noInfQuOrd(Qu,Ord), semantics(Qu/q)) -->
	"Ordering '", str(Ord), "' is not complete for 'inf'.".
chk_errmess(noSupQuOrd(Qu,Ord), semantics(Qu/q)) -->
	"Ordering '", str(Ord), "' is not complete for 'sup'.".

chk_alist([A])     --> !, """", str(A), """".
chk_alist([A1,A2]) --> !, """", str(A1), """ and """, str(A2), """".
chk_alist([A|As])  --> """", str(A), """, ", chk_alist(As).

chk_entries([E], L, R)             -->
   chk_entry(E, L, R).
chk_entries([E1,E2], L, R)         -->
   chk_entry(E1, L, R), " and ", chk_entry(E2, L, R).
chk_entries([E1,E2,E3], L, R)      -->
   chk_entry(E1, L, R), ", ", chk_entry(E2, L, R), " and ", chk_entry(E3, L, R).
chk_entries([E1,E2,E3,_E4|_], L, R) -->
   chk_entry(E1, L, R), ", ", chk_entry(E2, L, R), ", ", chk_entry(E3, L, R), ", ... ".

chk_entry(T:V, L, R) --> !, L, chk_tlist(T), R, ":", str(V).
chk_entry(T, L, R)   --> L, chk_tlist(T), R.

chk_tlist([V])    --> !, str(V).
chk_tlist([V|Vs]) --> str(V), ",", chk_tlist(Vs).

to_be([_])     --> "is".
to_be([_,_|_]) --> "are".

s([_])     --> "".
s([_,_|_]) --> "s".

ies([_])     --> "y".
ies([_,_|_]) --> "ies".


