%% ml_interactive.pl -- Run MUltlog interactively

:- ['@mlpath@/ml'].

:- dynamic logName/2, logTVs/2, logDTVs/2, logNDTVs/2, logOp/3.

% loadLogic(+Filename,+Lg) -- load definition of logic Lg from file FileName
% from FileName

loadLogic(FN,Lg) :-
    lgc_in(FN),
    check,
    lgcLN(LN),
    assertz(logName(Lg, LN)),
    lgcTVs(TVs),
    assertz(logTVs(Lg, TVs)),
    lgcDTVs(DTVs),
    assertz(logDTVs(Lg, DTVs)),
    subtract(TVs,DTVs,NDTVs),
    assertz(logNDTVs(Lg, NDTVs)),
    (lgcOp(N, _),
	   chkOp(N, S),
	   assertz(logOp(Lg, N, S)),
	   fail
	;  true
	),
    format("Logic ~s loaded as '~a'.", [LN, Lg]),
    !.

% valueOp(Lg, Op, Arg, Val) -- in logic Lg, the value of Op applied to
% Arg is V.

valueOp(Lg, Op/Ar, Args, V) :-
    logOp(Lg, Op/Ar, Map),
    member(Args:V,Map).
valueOp(Lg, Op, Args, V) :-
    logOp(Lg, Op/_, Map), !,
    member(Args:V, Map).

% isFmla(+Lg, +Var, ?F, ?N) - F is a legal formula in logic Lg of
% length N. Var is the list of variables in F.

isFmla(_, Vars, v-N, 1) :-
    member(v-N, Vars).
isFmla(Lg, _, Op, 1) :-
    logOp(Lg, Op/0, _).
isFmla(Lg, Vars, F, Len) :-
    logOp(Lg, Op/Ar, _),
    Ar > 0,
    functor(F, Op, Ar),
    length(Args, Ar),
    length(Lens, Ar),
    F =.. [Op|Args],
    succ(Sum,Len),
    int_partition(Lens,Sum),
    maplist(isFmla(Lg, Vars), Args, Lens).

int_partition([],0).
int_partition([N|Ns],S) :-
    between(1,S,N),
    plus(N,R,S),
    int_partition(Ns,R). 

% hasValue(Lg, Ass, F, V) -- formula F has value V under assignment
% Ass in logic Lg. Assignments are lists of variable:value pairs.

hasValue(Lg, _, V, V) :-
    logTVs(Lg, TVs),
    member(V, TVs).
hasValue(_, Ass, v-N, V) :-
    member(v-N:V, Ass).
hasValue(Lg, _, F, V) :-
    logOp(Lg, F/0, [[]:V]).
hasValue(Lg, Ass, F, V) :-
    logOp(Lg, Op/Ar, Map),
    length(Args,Ar),
    F =.. [Op|Args],
    maplist(hasValue(Lg,Ass), Args, Vs),
    member(Vs:V, Map).

% collectVars(+F, -Vars) -- Vars is the list of all variables in Fmla

collectVars(v-N, [v-N]) :- !.
collectVars(F, Vars) :-
    F =.. [_|Args],
    maplist(collectVars, Args, VVars),
    ord_union(VVars, Vars).

% assignment(TVs, Vars, Ass) -- Ass is an assignment of variables in
% Vars to truth values in TVs

assignment(_, [], []).
assignment(TVs, [V|Vars],[V:TV|Ass]) :-
    member(TV, TVs),
    assignment(TVs,Vars,Ass).

% valueIn(InTVs, OutTVs, Ass, F) -- Ass assigns truth values from
% InTVs to the variables in F, and F evaluates to a value in OutTVs.

valueIn(InTVs, OutTVs, Ass, F) :-
    collectVars(F, Vars), !,
    assignment(InTVs, Vars, Ass), 
    hasValue(Lg, Ass, F, V),
    member(V, OutTVs).

% isDesignated(Lg, Ass, F) -- The value of F in logic Lg on assignment
% Ass is designated.

isDesignated(Lg, Ass, F) :-
    logDTVs(Lg, DTVs),
    logTVs(Lg, TVs),
    valueIn(TVs,DTVs,Ass,F).

% isUndesignated(Lg, Ass, F) -- The value of F in logic Lg on assignment
% Ass is not designated.

isUndesignated(Lg, Ass, F) :-
    logDTVs(Lg, DTVs),
    logTVs(Lg, TVs),
    valueIn(TVs,DTVs,Ass,F).

% isTaut(Lg, F) -- F is a tautology of Lg.

isTaut(Lg, F) :-
    logDTVs(Lg, DTVs),
    logTVs(Lg, TVs),
    collectVars(F, Vars), !,
    forall(assignment(TVs, Vars, Ass),
        (   hasValue(Lg, Ass, F, V),
            member(V, DTVs))).

% taut(Lg, F) -- same, except it backtracks through all formulas of F

taut(Lg, F) :-
    logDTVs(Lg, DTVs),
    logTVs(Lg, TVs), !,
    length(Vars, N),
    varList(N, Vars),
    isFmla(Lg,Vars,F,N),
    forall(assignment(TVs, Vars, Ass),
        (   hasValue(Lg, Ass, F, V),
            member(V, DTVs))).

isFmla(Lg,F) :-
    length(Vars, N),
    varList(N, Vars),
    isFmla(Lg,Vars,F,N).

varList(N,Vars) :-
    numlist(1,N,Nums),
    maplist(varNo, Nums, Vars).
    varNo(N,v-N).
