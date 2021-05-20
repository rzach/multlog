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

% valueOp(Lg, Op, Arg, Val) -- in logic Lg, the value of Op applied to
% Arg is V.
valueOp(Lg, Op/Ar, Arg, V) :-
    logOp(Lg, Op/Ar, Map), !,
    member(Arg:V, Map).
valueOp(Lg, Op, Arg, V) :-
    logOp(Lg, Op/_, Map), !,
    member(Arg:V, Map).

% hasValue(Lg, FS, V) -- in logic Lg, formula skeleton FS has value V.
hasValue(Lg, FS, V) :-
    logOp(Lg, FS/0, [[]:V]).
hasValue(Lg, V, V) :-
    logTVs(Lg, TVs),
    member(V, TVs).
hasValue(Lg, FS, V) :-
    logOp(Lg, Op/A, Map),
    length(Args, A),
    FS =.. [Op|Args],
    maplist(hasValue(Lg), Args, Vals),
    member(Vals:V, Map).

% isDesignated(Lg, F) -- The value of F in logic Lg is
% designated. If F contains variables, every value combination for
% these variables for which F is designated is a solution.
isDesignated(Lg, F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(F, Vars), !,
    listOfTVs(Vars, TVs),
    hasValue(Lg, F, V),
    member(V, DTVs).

% isUnDesignated(Lg, F) -- same, except will find all solutions
% where F has a non-designated value.
isUnDesignated(Lg, F) :-
    logTVs(Lg, TVs),
    logNDTVs(Lg, NDTVs),
    term_variables(F, Vars), !,
    listOfTVs(Vars, TVs),
    hasValue(Lg, F, V),
    member(V, NDTVs).

% isTaut(Lg, F) -- F is a tautology of Lg.
isTaut(Lg, F) :-
    logTVs(Lg, TVs),
    logDTVs(Lg, DTVs),
    term_variables(F, Vars), !,
    forall(listOfTVs(Vars, TVs),
           ( hasValue(Lg, F, V),
             member(V, DTVs)
           )).

% listOfTVs(L, TVs) -- L is a subset of TVs (works as generator of
% lists of truth values).
listOfTVs([], _) :- !.
listOfTVs([L|Ls], TVs) :-
    member(L, TVs),
    listOfTVs(Ls, TVs).
