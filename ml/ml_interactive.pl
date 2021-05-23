%% ml_interactive.pl -- Run MUltlog interactively
multlog.

user:file_search_path(multlog, Dir) :-
    source_file(multlog, File),
    file_directory_name(File, Dir).

:- set_prolog_flag(double_quotes, codes).

% Load required files
:- [multlog(ml_conf)].
:- [multlog(ml_util)].
:- [multlog(ml_error)].
:- [multlog(ml_lgc)].
:- [multlog(ml_check)].
:- [multlog(ml_kernl)].
:- [multlog(ml_tex)].
:- [multlog(propres_compact)].
:- [multlog(ml_minimize)].
:- [multlog(ml_compactrepr)].

:- (dynamic logName/2, logTVs/2, logDTVs/2, logNDTVs/2, logOp/3).

% Defining logics
% ===============

% loadLogic(Filename, Lg) -- load definition of logic Lg from file FileName
% from FileName

loadLogic(FN, Lg) :-
    lgc_in(FN),
    check,
    deleteLogic(Lg),
    lgcLN(LN),
    assertz(logName(Lg, LN)),
    lgcTVs(TVs),
    assertz(logTVs(Lg, TVs)),
    lgcDTVs(DTVs),
    assertz(logDTVs(Lg, DTVs)),
    subtract(TVs, DTVs, NDTVs),
    assertz(logNDTVs(Lg, NDTVs)),
    (   lgcOp(N, _),
        chkOp(N, S),
        assertz(logOp(Lg, N, S)),
        fail
    ;   true
    ),
    format("Logic ~s loaded as '~a'.", [LN, Lg]), nl, !.

deleteLogic(Lg) :-
(logName(Lg, _) ->
    (retract(logName(Lg, _)),
    retract(logTVs(Lg, _)),
    retract(logDTVs(Lg, _)),
    retract(logNDTVs(Lg, _)),
    retract(logOp(Lg, _, _))) ; true), !.

% addOp(Lg, Op/Ar, Map) -- add Op/Ar to logic L with mapping Map.
% if Ar/Op already defined, it is replaced.

addOp(Lg, Op/Ar, Map) :-
    retract(logOp(Lg, Op/Ar, _)),
    assert(logOp(Lg, Op/Ar, Map)).

% delOp(Lg, Op/Ar) -- delete Op/Ar from logic Lg

delOp(Lg, Op/Ar) :- 
    retract(logOp(Lg,Op/Ar,_)).

% logOps(Lg, Ops) -- Ops is the list of operators of Lg

logOps(Lg, Ops) :-
    setof(Op, A^logOp(Lg, Op, A), Ops).

% logOps(Lg, Maps) -- Maps is the list of operator/map pairs of Lg

logMaps(Lg, Maps) :-
    setof(Op:Map, logOp(Lg, Op, Map), Maps).

% Semantics of logics
% ===================

% valueOp(Maps, Op, Arg, Val) -- in logic Lg, the value of Op applied to
% Arg is V.

valueOp(Lg, Op/Ar, Arg, V) :-
    logOp(Lg, Op/Ar, Map), !,
    member(Arg:V, Map).
valueOp(Lg, Op, Arg, V) :-
    logOp(Lg, Op/_, Map), !,
    member(Arg:V, Map).

% hasValue(Lg, F, V) -- in logic Lg, formula F has value V.

hasValue(Lg, F, F) :- % F can be a truth value of Lg
    logTVs(Lg,TVs),
    member(F,TVs).
hasValue(_, F, _) :-
    var(F), !, fail.
hasValue(Lg, F, V) :-  % F can be a constant mapped to V
    logOp(Lg,F/0, [[]:V]).
hasValue(Lg, F, V) :-
    F =.. [Op|Args],
    atom(Op),
    logOp(Lg, Op/A, Map),
    length(Args, A),
    maplist(hasValue(Lg), Args, Vals),
    member(Vals:V, Map).

% hasValueIn(Lg, Vs, F) :- For any values in truth values of logic Lg,
% F takes a value in Vs.

hasValueIn(Lg, F, Vs) :-
    hasValue(Lg, F, V),
    member(V, Vs).

haveValuesIn(_Lg, [], _Vs).
haveValuesIn(Lg, [F|Fs], Vs) :-
    hasValueIn(Lg, F, Vs),
    haveValuesIn(Lg, Fs, Vs).

% isDesignated(Lg, F) -- in logic Lg, F takes a
% designated value

isDesignated(Lg, F) :-
    logDTVs(Lg, DTVs),
    hasValueIn(Lg, F, DTVs).

areDesignated(_Lg, []).
areDesignated(Lg, [F|Fs]) :-
    isDesignated(Lg, F),
    areDesignated(Lg,Fs).

% isUnDesignated(Lg, F) -- same, except will find all solutions
% where F has a non-designated value.

isUnDesignated(Lg, F) :-
    logNDTVs(Lg, NDTVs),
    hasValueIn(Lg, F, NDTVs).

% isTaut(Lg, F) -- F is a tautology of Lg.

isTaut(Lg, F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(F, Vars), !,
    forall(listOfTVs(Vars, TVs),
           ( hasValue(Lg, F, V),
             member(V, DTVs)
           )).

findTaut(Lg, F) :-
    findFmla(Lg, F),
    isTaut(Lg, F).


% isConseq(Lg, Fs, F) -- in logic Lg, F is a consequence of the Fs

isConseq(Lg, Fs, F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(Fs, Vars),
    forall(listOfTVs(Vars, TVs),
        ( haveValuesIn(Lg, Fs, DTVs) ->
          hasValueIn(Lg, F, DTVs) ; true
        )).

% areEquiv(Lg1, F1, Lg2, F2) -- F1 and F2 agree on all assignments of
% values from Lg1 to the variables of F1 and F2. Assumes that truth
% values of Lg1 are also truth values of Lg2.

areEquiv(Lg1, F1, Lg2, F2) :-
    forall(hasValue(Lg1, F1, V),
        hasValue(Lg2, F2, V)
           ).

% findEquiv(Lg1, F1, Lg2, -F2) -- find a formula F2 equivalent to F1.

findEquiv(Lg1, F1, Lg2, F2) :-
    term_variables(F1, VarsF1),
    findFmla(Lg2, F2, _, V), 
    maplist(revmember(VarsF1), V),
    areEquiv(Lg1, F1, Lg2, F2).

% listOfTVs(L, TVs) -- L is a subset of TVs (works as generator of
% lists of truth values).

listOfTVs([], _) :- !.
listOfTVs([L|Ls], TVs) :-
    member(L, TVs),
    listOfTVs(Ls, TVs).

% Finding formulas
% ================

% findFmla(Lg, -F) --- F is a formula of logic L. This will backtrack
% to find all formulas of Lg. It works by finding all most general
% formulas, then partitioning the set of variables, and identifying
% all variables in a set in the partition.

findFmla(Lg, F) :-
    findFmla(Lg, F, _, V),
    partition(V, Vs),
    unifyPart(Vs).

% findFmla(Lg, F, N, Vs) -- F is a most general formula of length N in
% logic Lg, with variables Vs.

findFmla(Lg, F, N, Vs) :-
    setof(Op, Map^logOp(Lg, Op, Map), Ops), !,
    length(S, N),
    findFmla(F, Ops, S, [], Vs, []).

% findFmla(F, Ops, S, S0, Vs, Vs0) F is a formula skeleton built from
% Ops of size S-S0, with variables Vs-Vs0. Ops is a list of
% Operator/Arity pairs.

findFmla(F, _Ops, [_|S], S, [F|Vs], Vs) :-
    var(F).
findFmla(F, Ops, [_|S], S0, Vs, Vs0) :-
    member(Op/Ar, Ops),
    functor(F, Op, Ar),
    F =.. [_|Fs],
    findFmla_l(Fs, Ops, S, S0, Vs, Vs0).

% isMGFmla_l(Lg, Ops, Fs, S, S0, Vs, Vs0) Fs is a list of formulas built
% from Ops with total size S-S0 and variables Vs-Vs0

findFmla_l([], _, S, S, Vs, Vs).
findFmla_l([F|Fs], Ops, S, S0, Vs, Vs0) :-
    findFmla(F, Ops, S, S1, Vs, Vs1),
    findFmla_l(Fs, Ops, S1, S0, Vs1, Vs0).

% partition(S, Ps) -- partition S into set of sets P.

partition([A], [[A]]).
partition([A|As], [[A] | Ps]) :-
    partition(As, Ps).
partition([A|As], Ps) :-
    partition(As, Qs),
    addElement(A, Qs, Ps).

addElement(A, [P | Ps], [[A|P]|Ps]).
addElement(A, [P | Ps], [P|Qs]) :-
    addElement(A, Ps, Qs).

% unifyPart(P) :- unify elements of each set in a partition.

unifyPart([]).
unifyPart([P|Ps]) :-
    unifySet(P),
    unifyPart(Ps).

% unifySet(A,S) :- unify all members of S.

unifySet([A|As]) :- 
    unifySet(A, As).

unifySet(_, []).
unifySet(A, [A|As]) :- 
    unifySet(A,As).

prettyFmla(F) :-
    term_variables(F, Vars),
    length(Vars, N),
    varList(N,Vars),
    write_term(F, [nl]), nl.

varList(N,Vars) :-
    numlist(1,N,Nums),
    maplist(varNo, Nums, Vars).

varNo(N,v-N).

revmember(X,Y) :- member(Y,X).
superset(X, Y) :- subset(Y, X).
intersects(X,Y) :- member(Z,X), member(Z,Y).

% Finding congruences of a matrix
% ===================

% isCong(Lg, Part) :- Part (a partition of truth values
% of Lg) is a congruence.

isCong(Lg, Part, DPart) :-
    logDTVs(Lg, DTVs),
    logNDTVs(Lg, NDTVs),
    partition(DTVs, DPart),
    partition(NDTVs, NDPart),
    union(NDPart, DPart, Part),
    forall(logOp(Lg, _Op/Ar, Map),
        congMap(Ar, Map, Part)).


% congMap(Ar, Map, Part) :- Part is a congruence wrt the Ar-place
% mapping Map, i.e., if [x1] -> y1 and [x2] -> y2 with x_1 ~ x_2 then
% y1 ~ y2 (where x ~ y iff [x, y] \subset X \in Part.

congMap(0, _, _) :- !.
congMap(Ar, Map, Part) :-
    length(ArgCs, Ar),
    forall(maplist(revmember(Part), ArgCs),
        checkCongPart(Map, ArgCs, Part)).

% checkCong(Map, ArgCs, ValC) :- ArgCs is a list of congruence
% classes; this checks if all values are in ValC (ie if all
% values are congruent).

% find the congruence class of the value of Map for the first sequence
% of arguments....

checkCongPart(Map, ArgCs, Part) :-
    maplist(revmember, ArgCs, Args),
    member(Args:V, Map),
    member(ClassV, Part),
    member(V, ClassV),
    !,
    checkCong(Map, ArgCs, ClassV).

% ... then test if the value of Map for all sequences of arguments
% from the sequence of classes of arguments is in Class

checkCong(Map, ArgCs, ClassV) :-
    forall(maplist(revmember, ArgCs, Args),
        (   member(Args:V, Map),
            member(V, ClassV))).

% subClass(Part, A) :- A is a subset of a set in P.

subClass([C|_], A) :-
    subset(A,C), !.
subClass([_|Cs], A) :-
    subClass(Cs, A).

equivClass([], _, _) :-
    fail.
equivClass([C|_], A, C) :-
    member(A, C), !.
equivClass([_|Cs], A, C) :-
    equivClass(Cs, A, C).

% display the partition P, D

writeCong(P, D) :-
    length(P, N1),
    length(D, N2),
    format("Found a congruence:~n~d congruence classes, ~d designated~nCongruence   classes:~n~w~nDesignated classes:~n~w~n", [N1, N2, P, D]).

% Products of logics
% ==================

logProduct(Lg1, Lg2, Lg12) :-
    logTVs(Lg1, TVs1),
    logTVs(Lg2, TVs2),
    setProduct(TVs1, TVs2, TVs),
    assertz(logTVs(Lg12, TVs)),
    logDTVs(Lg1, DTVs1),
    logDTVs(Lg2, DTVs2),
    setProduct(DTVs1, DTVs2, DTVs),
    assertz(logDTVs(Lg12, DTVs)),
    subtract(TVs, DTVs, NDTVs),
    assertz(logNDTVs(Lg12, NDTVs)),
    forall(logOp(Lg1, Op/Ar, Map1),
    (   logOp(Lg2, Op/Ar, Map2),
        mapProduct(Map1, Map2, Map),
        assertz(logOp(Lg12, Op/Ar, Map)))).

setProduct(A1,A2,B) :-
    setof((P1, P2), (member(P1,A1), member(P2,A2)), B).

mapProduct(M1, M2, B) :-
    setof(Args:V,
        prodMap(M1, M2, Args, V),
        B).

prodMap(M1, M2, Args, (V1,V2)) :-
member(A1:V1, M1),
member(A2:V2, M2),
pairUp(A1,A2,Args).

pairUp([],[],[]).
pairUp([A|As],[B|Bs], [(A,B)|Cs]) :-
pairUp(As,Bs,Cs).
