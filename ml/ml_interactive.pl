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

% defining logics

% loadLogic(+Filename,+Lg) -- load definition of logic Lg from file FileName
% from FileName
loadLogic(FN, Lg) :-
    lgc_in(FN),
    check,
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

isTaut(+Lg, +F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(F, Vars), !,
    forall(listOfTVs(Vars, TVs),
           ( hasValue(Lg, F, V),
             member(V, DTVs)
           )).

% isConseq(+Lg, +Fs, +F) -- in logic Lg, F is a consequence of the Fs

isConseq(Lg, Fs, F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(Fs, Vars),
    forall(listOfTVs(Vars, TVs),
        ( haveValuesIn(Lg, Fs, DTVs) ->
          hasValueIn(Lg, F, DTVs) ; true
        )).

% areEquiv(+Lg1, +F1, +Lg2, +F2) -- F1 and F2 agree on all assignments of
% values from Lg1 to the variables of F1 and F2. Assumes that truth
% values of Lg1 are also truth values of Lg2.

areEquiv(Lg1, F1, Lg2, F2) :-
    forall(hasValue(Lg1, F1, V),
        hasValue(Lg2, F2, V)
           ).

% findEquiv(+Lg1, +F1, +Lg2, -F2) -- find a formula F2 equivalent to F1.

findEquiv(Lg1, F1, Lg2, F2) :-
    term_variables(F1, VarsF1),
    isFmla(Lg2, F2, _, V), 
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

% isFmla(+Lg, -F) --- F is a formula of logic L. This will backtrack
% to find all formulas of Lg. It works by finding all most general
% formulas, then partitioning the set of variables, and identifying
% all variables in a set in the partition.

isFmla(Lg, F) :-
    isFmla(Lg, F, _, V),
    partition(V, Vs),
    unifyPart(Vs).

% isFmla(Lg, F, N, Vs) -- F is a most general formula of length N in
% logic Lg, with variables Vs.

isFmla(Lg, F, N, Vs) :-
    setof(Op, Map^logOp(Lg, Op, Map), Ops), !,
    length(S, N),
    isFmla(F, Ops, S, [], Vs, []).

% isFmla(F, Ops, S, S0, Vs, Vs0) F is a formula skeleton built from
% Ops of size S-S0, with variables Vs-Vs0. Ops is a list of
% Operator/Arity pairs.

isFmla(F, _Ops, [_|S], S, [F|Vs], Vs) :-
    var(F).
isFmla(F, Ops, [_|S], S0, Vs, Vs0) :-
    member(Op/Ar, Ops),
    functor(F, Op, Ar),
    F =.. [_|Fs],
    isFmla_l(Fs, Ops, S, S0, Vs, Vs0).

% isMGFmla_l(Lg, Ops, Fs, S, S0, Vs, Vs0) Fs is a list of formulas built
% from Ops with total size S-S0 and variables Vs-Vs0

isFmla_l([], _, S, S, Vs, Vs).
isFmla_l([F|Fs], Ops, S, S0, Vs, Vs0) :-
    isFmla(F, Ops, S, S1, Vs, Vs1),
    isFmla_l(Fs, Ops, S1, S0, Vs1, Vs0).

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