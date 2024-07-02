%%% Filename : ml_msq.pl
%%% Contents : Conversion of Multlog to Multseq files
%%% Author   : Richard Zach
%%% Interface:
%%%            lgc2msq(In,Out,Cnf). Translates Multlog .lgc file "In" to %%%               Multseq file "Out" using configuration in "Cnf"

:- set_prolog_flag(double_quotes, string).
:- dynamic cached/1.

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
	cache_in(Cache),
	check,
	kernel,
	cache_out(Cache),
	msq_out(Out, CFN),
	!.

msq_out(FN, CFN) :-
	\+ errOccurred(_,_),
	ml_if(nonvar(CFN), read_texCfg(CFN), true),
	tell(FN),
	lgcLN(LN),
	format("% ~w - MSeq specification of logic '~s'\n", [FN, LN]),
	lgcTVs(TVs),
	format("truth_values(~w).\n", [TVs]),
	lgcDTVs(DTVs),
	format("designated_truth_values(~w).\n", [DTVs]),
	(member(TV,TVs),
		getTexName(TV,TexTV),
		format("tex_tv(~w,[~W]).\n", [TV,TexTV,[quoted(true)]]),
		fail
	; true),
	(lgcOp(Op/Ar,_),
		getTexName(Op,TexN),
		format("\noperator(~w, ~w, ~W).\n", [Op, Ar, TexN, [quoted(true)]]),
		% write rules for Op
		(krnOpIntro(Op/Ar, T, Cnf),
			format("rule(", []),
			formatOp(Op/Ar),
			format("^~w, [", [T]),
			formatCnf(Cnf),
			format("], ~w_~w).\n", [Op, T]),
			getTexName(T, TexT),
			format("tex_rn(~w_~w, [\"{\", ", [Op, T]),
			write_term(TexN, [quoted(true)]),
			format(", \"}_{\", ", []),
			write_term(TexT, [quoted(true)]),
			format(", \"}\"]).\n", []),
			fail
		; true),
		% write LaTeX codes for TVs and Ops
		getTexName(Op,TexN),
		% write operator format for Op
		(texPrefix(Op) ->
			format("op(500,fx,~w).\n", [Op]),
			format("tex_op(~w(A), [~W, \" \", A]).\n",
				[Op, TexN, [quoted(true)]])
		;
		texInfix(Op) ->
			format("op(700,xfx,~w).\n", [Op]),
			format("tex_op(~w(A, B), [\"(\", A, ~W, \" \", B, \")\"]).\n",
				[Op, TexN, [quoted(true)]])
		;
			formatOpTex(Op/Ar)),
		fail
	; true),
	told,
	!.

formatOp(Op/Ar) :-
	(Ar = 0 ->
		format("~w", [Op])
	;
		format("~w(",[Op]),
		findall(X,between(1,Ar,X),L),
		formatVarList(L, ','),
		format(")", [])).

formatOpTex(Op/Ar) :-
	format("tex_op(", []),
	formatOp(Op/Ar),
	getTexName(Op, TexN),
	(Ar = 0 ->
		format(", [~W]).\n", [TexN, [quoted(true)]])
	;
		findall(X,between(1,Ar,X),L),
		format(", [~W, \"(\", ",[TexN, [quoted(true)]]),
		formatVarList(L, ', \",\",'),
		format(", \")\"]).\n",[])), !.

% always use string "\\neg" even if config file has bare \\neg
getTexName(X, TexN) :-
	(texName(X, \\TN) ->
		atom_string(TN, TNS),
		string_concat("\\", TNS, TexN)
	; texName(X, TexN)), !.

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

