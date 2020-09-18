%%% Filename : ml_ilc.pl
%%% Date     : 13.1.1997
%%% Contents : Module for outputing a logic in the syntax for ILC, the Tcl/Tk interface.
%%% Author   : Gernot Salzer
%%% Interface: ilc_out(Name).  Stores logic in file 'Name'.

:- dynamic   ilcFN/1, ilcTVno/2, ilcTVs/1.

ilcDatabase([ilcFN/1, ilcTVno/2, ilcTVs/1]).

ilc_out(FN) :-
	clear(ilcDatabase),
	assert(ilcFN(FN)),
	tell(FN),
	ilc_out_logic,
	ilc_out_errors,
	told,
	!.
ilc_out(FN) :-
	phrase(ilc_errmess(cannot_write_file(FN)), Message),
	error(openerror, [Message]).

ilc_out_logic :-
	ilc_check(bagof([I1,I2], ml_subset([I1,I2],[0,1]), [[0,0],[0,1],[1,0],[1,1]]),
	   broken_subset),
	ilc_check(bagof(L, sublist(L,[0,1,2]), [[0,1,2],[0,1],[0,2],[0],[1,2],[1],[2],[]]),
	   broken_sublist),
	lgcTVs(TVs),
	make_set(TVs,TTVs),
	assert(ilcTVs(TTVs)),
	ml_if(lgcLN(LN)
	  , (print_phrase(ilcLN(LN)),
	     print_phrase(ilcSep))
	  , true
	),
	print_phrase(ilcTVs(TTVs)),
	ml_if(lgcDTVs(DTVs)
	  , (print_phrase(ilcSep),
	     print_phrase(ilcDTVs(DTVs)))
	  , true
	),
	(lgcOrd(N, S),
	   print_phrase(ilcSep),
	   print_phrase(ilcOrd(N, S)),
	   fail
	;  true
	),
	(lgcOp(N, S),
	   print_phrase(ilcSep),
	   print_phrase(ilcOp(N, S)),
	   fail
	;  true
	),
	(lgcQu(N, S),
	   print_phrase(ilcSep),
	   print_phrase(ilcQu(N, S)),
	   fail
	;  true
	),
	!.
ilc_out_logic.

ilc_out_errors :-
	(errOccurred(Type, Mess),
	   print_phrase(ilcSep),
	   print_phrase(ilcError(Type, Mess)),
	   println_strings(Mess),
	   fail
	;  true
	).

make_ilcTVno(TV, I) :-
	name(I, IS),
	assertz(ilcTVno(TV, IS)).

ilcTV2no(TV, No) :-
	nonvar(TV),
	ilcTVno(TV, No),
	!.
ilcTV2no(_TV, "-1").

ilcSep --> "%", eol.

ilcLN(N) --> "logic ", eol, N, eol.

ilcTVs(TVs)          --> "truth_values {", ilcTVs(TVs, 0), "}", eol.
ilcTVs([TV], I)      --> !, ilcTV(TV, I).
ilcTVs([TV|TVs], I0) --> ilcTV(TV, I0), {I1 is I0+1}, ",", ilcTVs(TVs, I1).
ilcTV(TV, I)         --> str(TV), {make_ilcTVno(TV, I)}.

ilcDTVs(TVs)       --> "designated_truth_values {", ilcDTVs1(TVs), "}", eol.
ilcDTVs1([TV])     --> !, ilcTVno(TV).
ilcDTVs1([TV|TVs]) --> ilcTVno(TV), " ", ilcDTVs1(TVs).
ilcTVno(TV)        --> {ilcTV2no(TV,TVno)}, TVno.

ilcOrd(N, S)  --> "ordering ", str(N), eol, S, eol.

ilcOp(Id/A, S)       --> "operator ", str(Id), "/", str(A), " ", ilcOpspec(S), eol.
ilcOpspec(map(M)) --> "mapping", eol, ilcOpmap(M).
ilcOpspec(tab(M)) --> "table",   eol, ilcOptab(M).
ilcOpspec(sup(N)) --> "bop sup ", str(N).
ilcOpspec(inf(N)) --> "bop inf ", str(N).
ilcOpmap(M)       --> {opmap2ilc(M, TVs)}, ilcOpTVs(TVs).
ilcOptab(M)       --> {optab2ilc(M, TVs)}, ilcOpTVs(TVs).
ilcOpTVs([TV])    --> !, ilcTVno(TV).
ilcOpTVs([TV|TVs])--> ilcTVno(TV), " ", ilcOpTVs(TVs).

ilcQu(N, S)       --> "quantifier ", str(N), " ", ilcQuspec(S), eol.
ilcQuspec(map(M)) --> "mapping", eol, ilcQumap(M).
ilcQuspec(op(Id/A))  --> "op ", str(Id), "/", str(A).
ilcQuspec(sup(N)) --> "bop sup ", str(N).
ilcQuspec(inf(N)) --> "bop inf ", str(N).
ilcQumap(M)       --> {qumap2ilc(M, TVs)}, ilcQuTVs(TVs).
ilcQuTVs([TV])    --> !, ilcTVno(TV).
ilcQuTVs([TV|TVs])--> ilcTVno(TV), " ", ilcQuTVs(TVs).

ilcError(Type, Mess) -->
	"error ",
	ilcErrorType(Type), " ",
	{ml_length(Mess, NoLines)}, str(NoLines),
	ilcErrorInfo(Type), eol.

ilcErrorType(openerror)    --> "openerror".
ilcErrorType(internal)     --> "internal".
ilcErrorType(syntax)       --> "syntax".
ilcErrorType(ordsyntax(_)) --> "ordsyntax".
ilcErrorType(semantics)    --> "semantics".
ilcErrorType(semantics(_)) --> "semantics".

ilcErrorInfo(openerror)    --> "".
ilcErrorInfo(internal)     --> "".
ilcErrorInfo(syntax)       --> "".
ilcErrorInfo(ordsyntax(N)) --> ilcErrorOrdOpQu(N).
ilcErrorInfo(semantics)    --> "".
ilcErrorInfo(semantics(N)) --> ilcErrorOrdOpQu(N).

ilcErrorOrdOpQu(Name/Kind) -->
	" ", str(Name), "/", ilcErrorOrdOpQuKind(Kind).

ilcErrorOrdOpQuKind(ord)   --> "ord".
ilcErrorOrdOpQuKind(q)     --> "q".
ilcErrorOrdOpQuKind(I)     --> {integer(I)}, str(I).

opmap2ilc(M, Results) :-
	ilcTVs(TVs),
	M = [X:_|_],
	ml_length(X, N),
	ml_length(T, N),
	bagof(TV, T^(ml_subset(T,TVs), ilcmember(T:TV,M)), Results).

optab2ilc(T, Results) :-
	ilcTVs(TVs),
	ml_length(TVs, N),
	tab2map(T, N, M),
	opmap2ilc(M, Results).

qumap2ilc(M, Results) :-
	ilcTVs(TVs),
	bagof(TV,
	      L1^L2^(sublist(L1,TVs), L1\==[], sort(L1,L2), ilcmember(L2:TV,M)),
	      Results).

ilcmember(E, L) :-
	ml_member(E, L), !.
ilcmember(_E, _L).

ilc_check(Test, _Err) :-
	\+ \+ Test, !.
ilc_check(_Test, Err) :-
	phrase(ilc_errmess(Err,Type), Mess),
	error(Type, [Mess]),
	!, fail.

ilc_errmess(broken_subset , internal) --> "Internal error: subset/2 broken.".
ilc_errmess(broken_sublist, internal) --> "Internal error: sublist/2 broken.".
ilc_errmess(cannot_write_file(FN)) -->
	"Can't open ILC file '", str(FN), "' for writing.".

