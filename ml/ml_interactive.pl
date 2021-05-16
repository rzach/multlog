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

valueOp(Lg, Op/Ar, Arg, V) :-
    logOp(Lg, Op/Ar, Map),
    member(Arg:V,Map).
valueOp(Lg, Op, Arg, V) :-
    logOp(Lg, Op/_, Map), !,
    member(Arg:V, Map).

% valueFmlaT(Lg, F, Val) -- in logic Lg, formula F has value V.

valueFmla(Lg, F, V) :-
    logTVs(Lg, TVs), !,
    member(V, TVs),
    fmlaList(F, L),
    valueList(Lg, L, V, TVs).

% valueList(Lg, List, Val, TVs) -- in logic Lg, the expression corresponding
% to List has value Val (TVs is the list of truth values of logic Lg).

valueList(_, TV, TV, TVs) :-
    member(TV, TVs).
valueList(Lg, [Op|Args], Val, TVs) :-
    valueLists(Lg, Args, Vals, TVs),
    valueOp(Lg, Op, Vals, Val).

valueLists(_, [], [], _).
valueLists(Lg, [Arg|Args], [Val|Vals], TVs) :-
    valueList(Lg, Arg, Val, TVs),
    valueLists(Lg, Args, Vals, TVs).

% fmlaList(F, L) -- recursively convert a formula F to a nested list L.

fmlaList(F, Atom) :-
    F =.. [Atom], !.
fmlaList(F, [Op|Lists]) :-
    F =.. [Op|Args],
    fmlaLists(Args,Lists).

fmlaLists([],[]) :- !.
fmlaLists([A|Args], [L|Lists]) :-
    fmlaList(A,L),
    fmlaLists(Args,Lists).


% isDesignated(Lg, F) -- The value of F in logic Lg is
% designated. If F contains variables, every value combination for
% these variables for which F is designated is a solution.

isDesignated(Lg, F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(F, Vars), !,
    listOfTVs(Vars,TVs),
    valueFmla(Lg, F, V),
    member(V, DTVs).

% isUnDesignated(Lg, F) -- same, except will find all solutions
% where F has a non-designated value.

isUnDesignated(Lg, F) :-
    logTVs(Lg, TVs),
    logNDTVs(Lg, NDTVs),
    term_variables(F, Vars), !,
    listOfTVs(Vars,TVs),
    valueFmla(Lg, F, V),
    member(V, NDTVs).

% isTaut(Lg, F) -- F is a tautology of Lg.

isTaut(Lg, F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(F, Vars), !,
    forall(listOfTVs(Vars,TVs),
        (   valueFmla(Lg, F, V),
            member(V, DTVs))).

% listOfTVs(L, TVs) -- L is a subset of TVs (works as generator of
% lists of truth values).

listOfTVs([], _) :- !.
listOfTVs([L|Ls], TVs) :-
    member(L, TVs),
    listOfTVs(Ls,TVs).

% Attempts at making a generator for formulas

isFormula(Lg, V) :-
    var(V).
isFormula(Lg, F) :-
    logOp(Lg,Op/N,_),
    functor(F, Op, N),
    F =.. [Op|Args],
    areFormulas(Lg, Args).

areFormulas(_, []).
areFormulas(Lg, [F|Fs]) :-
    isFormula(Lg, F),
    areFormulas(Lg, Fs).


% fmlaListDepth

fmlaListDepth(_, [V], 1) :-
    var(V).
fmlaListDepth(Lg, [Op|Args], D) :-
    logOp(Lg, Op/Ar, _),
    length(Args, Ar),
    fmlaListsDepth(Lg, Args, Ds), D is Ds+1.

fmlaListsDepth(_, [], 0) :- !.
fmlaListsDepth(Lg, [L|Ls], M) :-
    fmlaListDepth(Lg, L, D),
    fmlaListsDepth(Lg, Ls, N), M is D+N.

fmlaPolish(Lg, F, [F]) :-
    var(F).
fmlaPolish(Lg, Op, [Op/0]) :-
    logOp(Lg, Op/0, _).
fmlaPolish(Lg, F, [Op/N|PArgs]) :-
    logOp(Lg, Op/N, _), !,
    fmlasPolish(Lg, Args, PArgs),
    length(Args, N),
    F =.. [Op|Args].

fmlasPolish(_, [], []).
fmlasPolish(Lg,[F],List) :-
    fmlaPolish(Lg,F,List).
fmlasPolish(Lg,[F|Fs],List) :-
    fmlaPolish(Lg,F,L1),
    fmlasPolish(Lg,Fs,L2),
    append(L1,L2,List).
