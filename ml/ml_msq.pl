%%% Filename : ml.pl
%%% Contents : Root module of MUltlog
%%% Author   : Gernot Salzer
%%% Interface:
%%%            lgc2msq(In,Out,Cnf). Translates LGC-file "In" to msq-file "Out"
%%%                                 using configuration in "Cnf"
%%%
%%% 2021/05/14, GS: output of stripped cfg file added



:- set_prolog_flag(double_quotes, codes).
:- dynamic	cached/1, msqFN/1, msqCFN/1, msqTVno/2, msqOpno/2, msqQuno/2,
						msqCounter/1, msqName/2, msqPrefix/1, msqInfix/1, msqExtra/2.

lgc2msq(In, Out, CFN) :-
	init_errors,
	println,
	who_am_I,
	print_terms(['Converting ', In, ' to MUltseq rules file ', Out]),
	ml_if(var(CFN)
	  ,  println
	  ,  println_terms([' using configuration in ', CFN])
	),
	lgc_in(In),
	atom_concat(In, '.stripped', InStripped),
	lgc_stripped(In, InStripped),
	atom_concat(In, '.cache', Cache),
%	cache_in(Cache),
	check,
	kernel,
%	cache_out(Cache),
	msq_out(Out, CFN).

msqDatabase([msqFN/1, msqCFN/1, msqTVno/2, msqOpno/2, msqQuno/2, msqCounter/1,
             texName/2, texPrefix/1, texInfix/1, texExtra/2]).

msq_out(FN, CFN) :-
	\+ errOccurred(_,_),
	clear(msqDatabase),
	ml_if(nonvar(CFN), read_texCfg(CFN), true),
	asserta(msqFN(FN)),
	tell(FN),
	lgcLN(LN),
	format("% ~w - MSeq specification of logic '~s'\n", [FN, LN]),
	lgcTVs(TVs),
	format("truth_values(~w).\n", [TVs]),
	lgcDTVs(DTVs),
	format("designated_truth_values(~w).\n", [DTVs]),
	(member(TV,TVs),
		texName(TV,TexTV),
		format("tex_tv(~w,~s).\n", [TV,TexTV]),
		fail
	; true),
	(lgcOp(Op/Ar,_),
		texName(Op,TexN),
		% write operator format for Op
		(texPrefix(Op) ->
			format("tex_op(~w(A), [\"~w\", A]).\n", [Op, TexN])
		;
		texInfix(Op) ->
			format("tex_op(~w(A, B), [\"(\", A, \"~w\", B, \")\"]).\n", [Op, TexN])
		;
			formatOpTex(Op/Ar)),
		% write rules for Op
		(krnOpIntro(Op/Ar, T, Cnf),
			format("rule(", []),
			formatOp(Op/Ar),
			format("^~w, [", [T]),
			formatCnf(Cnf),
			format("], ~w_~w).\n", [Op, T]),
			fail
		; true),
		fail
	; true),
	told,
	!.

formatOp(Op/Ar) :-
	format("~w(",[Op]),
	findall(X,between(1,Ar,X),L),
	formatVarList(L, ','),
	format(")", []).

formatOpTex(Op/Ar) :-
	format("tex_op(", []),
	formatOp(Op/Ar),
	texName(Op, TexN),
	findall(X,between(1,Ar,X),L),
	format(", [\"~s(\", ",[TexN]),
	formatVarList(L, ', \",\",'),
	format(", \")\"]).\n",[]).


formatVarList([], _).
formatVarList([X|Xs], Sep) :-
	format("A~w", [X]),
	(Xs = [_|_] -> format(" ~w ", [Sep]); true),
	formatVarList(Xs, Sep).

formatClause([]). 
formatClause([X^T|Ls]) :-
	format("A~w^~w", [X, T]),
	(Ls = [_|_] -> format(", ", []); true),
	formatClause(Ls).

formatCnf([]).
formatCnf([C|Cs]) :-
	format("[",[]),
	formatClause(C),
	format("]",[]),
	(Cs = [_|_] -> format(", ", []); true),
	formatCnf(Cs).
