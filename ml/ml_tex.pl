%%% Filename : ml_tex.pl
%%% Date     : 13.1.1997
%%% Contents : Module for writing LaTeX commands to a style file,
%%%            which is read by "ml.tex".
%%% Author   : Gernot Salzer
%%% Interface:
%%%            tex_out(FN,CFN).  Writes logic specific LaTeX commands to file 'FN';
%%%                              CFN is the name of a file containing a table for translating
%%%                              ord/op/qu-names to TeX expressions, and maybe other
%%%                              configuration information. If SFN is a variable, it is
%%%                              ignored.

:- set_prolog_flag(double_quotes, codes).
:- dynamic   texFN/1, texCFN/1, texTVno/2, texOpno/2, texQuno/2, texCounter/1,
             texName/2, texPrefix/1, texInfix/1.

texDatabase([texFN/1, texCFN/1, texTVno/2, texOpno/2, texQuno/2, texCounter/1,
             texName/2, texPrefix/1, texInfix/1]).

tex_out(FN, CFN) :-
	\+ errOccurred(_,_),
	clear(texDatabase),
	ml_if(nonvar(CFN), read_texCfg(CFN), true),
	asserta(texFN(FN)),
	tell(FN),
	lgcLN(LN),
	print_phrase(texLN(LN)),
	lgcTVs(TVs),
	init_counter(0),
	(ml_member(TV, TVs),
	   step_counter(I),
	   assertz(texTVno(TV,I)),
	   print_phrase(texTV(TV)),
	   fail
	;  true
	),
	clear_counter(I1),
	print_phrase(texTVCnt(I1)),
	lgcDTVs(DTVs),
	(ml_member(TV, DTVs),
	   print_phrase(texDTV(TV)),
	   fail
	;  true
	),
	init_counter(0),
	(lgcOp(N, _),
	   chkOp(N, S),
	   step_counter(I),
	   assertz(texOpno(N,I)),
	   print_phrase(texOp(N, S, TVs)),
	   fail
	;  true
	),
	clear_counter(I2),
	print_phrase(texOpCnt(I2)),
	init_counter(0),
	(lgcQu(N, _),
	   chkQu(N, S),
	   step_counter(I),
	   assertz(texQuno(N,I)),
	   print_phrase(texQu(N, S, TVs)),
	   fail
	;  true
	),
	clear_counter(I3),
	print_phrase(texQuCnt(I3)),
	(krnOpIntro(Op, TV, Cnf),
	   print_phrase(texOpRule(Op, TV, Cnf, TVs)),
	   print_phrase(texNDOpRule(Op, TV, Cnf, TVs, DTVs)),
	   print_phrase(texClOpRule(Op, TV, Cnf)),
	   print_phrase(texTabOpRule(Op, TV, Cnf)),
	   fail
	;  true
	),
	(krnQuIntro(Qu, TV, Cnf),
	   print_phrase(texQuRule(Qu, TV, Cnf, TVs)),
	   print_phrase(texNDQuRule(Op, TV, Cnf, TVs, DTVs)),
	   print_phrase(texClQuRule(Qu, TV, Cnf)),
	   print_phrase(texTabQuRule(Qu, TV, Cnf)),
	   fail
	;  true
	),
	told,
	!.

init_counter(N) :-
   asserta(texCounter(N)).

step_counter(N) :-
   retract(texCounter(N0)),
   N is N0 + 1,
   asserta(texCounter(N)).

clear_counter(N) :-
   retract(texCounter(N)).

read_texCfg(CFN) :-
   see(CFN), !,
   asserta(texCFN(CFN)),
   repeat,
      read(T),
      process_texCfg(T),
      T == end_of_file,
   !,
   seen.
read_texCfg(CFN) :-
	phrase(tex_errmess(cannot_read_file(CFN)), Message),
	error(openerror, [Message]).

process_texCfg(end_of_file) :- !.
process_texCfg(texName(L, T)) :- !,
   assertz(texName(L, T)).
process_texCfg(texInfix(L)) :- !,
   assertz(texInfix(L)).
process_texCfg(texPrefix(L)) :- !,
   assertz(texPrefix(L)).
process_texCfg(_X) :-
	phrase(tex_errmess(unknownTeXcfg), Message),
	error(syntax, [Message]).

tex_errmess(cannot_read_file(FN)) -->
	"Can't open LGC file '", str(FN), "' for reading.".
tex_errmess(unknownTeXcfg) -->
	"Unknown declaration in TeX configuration file.".


:- op(10,fx,\\).

\\N        --> bs, str(N).
\\(N,I)    --> \\N, roman(I).
\\(N,I,J)  --> {integer(J)}, !, bs, str(N), roman(I), "X", roman(J).
\\(N,I,O)  --> bs, str(N), roman(I), str(O).
bs        --> [92].
bsbs      --> [92,92].

def(X,Y)  --> \\def, X, "{", Y, "}", eol.

texTVCnt(C) --> def(\\'NoTVs', str(C)).
texOpCnt(C) --> def(\\'NoOps', str(C)).
texQuCnt(C) --> def(\\'NoQus', str(C)).
texLN(N)    --> def(\\'NameOfLogic', N).
texTV(TV)   --> def(textv(TV), texName(TV)),
                {texTVno(TV, I)}, \\newif, \\(ifTV, I, 'IsDTV'), eol.

texDTV(TV)  --> {texTVno(TV, I)}, \\('TV', I, 'IsDTVtrue' ), eol.

texOp(Op, S, TVs) -->
	{Op=N/A},
	def(texopname(Op),  texName(N)),
	def(texoparity(Op), str(A)),
	def(texopfmla(Op), texOpExprStrict(A, N)),
	\\def, texoptab(Op), "{%", eol,
	texOpSpec(A, Op, S, TVs), "%", eol,
	"}", eol.

texOpSpec(0, Op, [[]:F], _)   -->
	!,
	"   ", \\widetilde, "{", texopname(Op), "} ", \\equiv, textv(F).
texOpSpec(2, Op, S, TVs)      -->
	!,
	{opmap2textab(S, TVs, Lines), ml_length(TVs,L)},
	"   ", \\begin, "{array}{c|*{", str(L), "}{c}}", eol,
	"   ", \\widetilde, "{", texopname(Op), "} & ", 
	texTVs(TVs, " & "), " ", bsbs, \\hline, eol,
	texOpTabLines(Lines), eol,
	"   ", \\end, "{array}".
texOpSpec(_, Op, S, TVs)      -->
	{opmap2tex(S, TVs, Lines)},
	"   ", \\begin, "{array}{c|l}", eol,
	"   ",\\widetilde,"{", texopname(Op), "} & ", bsbs, \\hline, eol,
	texOpMapLines(Lines), eol,
	"   ", \\end, "{array}".

texOpTabLines([L])     -->
	!,
	"   ", texTVs(L, " & ").
texOpTabLines([L|Ls])  -->
	"   ", texTVs(L, " & "), " ", bsbs, eol,
	texOpTabLines(Ls).

texOpMapLines([L])    -->
	!,
	"   ", texOpMapLine(L).
texOpMapLines([L|Ls]) -->
	"   ", texOpMapLine(L), " ", bsbs, eol,
	texOpMapLines(Ls).

texOpMapLine(T:F) -->
	texTVtup(T), " & ", textv(F).

texTVtup([TV]) -->
	!,
	textv(TV).
texTVtup(TVs)  -->
	"(", texTVs(TVs, ","), ")".

texQu(Qu, S, TVs) -->
	def(texquname(Qu), texName(Qu)),
	\\def, texqutab(Qu), "{%", eol,
	texQuSpec(Qu, S, TVs), "%", eol,
	"}", eol.

texQuSpec(Qu, S, TVs) -->
	{qumap2tex(S, TVs, Lines)},
	"   ", \\begin, "{array}{c|l}", eol,
	\\widetilde,"{", texquname(Qu), "} & ", bsbs, \\hline, eol,
	texQuMapLines(Lines), eol,
	"   ", \\end, "{array}".

texQuMapLines([L])    -->
	!,
	"   ", texQuMapLine(L).
texQuMapLines([L|Ls]) -->
	"   ", texQuMapLine(L), " ", bsbs, eol,
	texQuMapLines(Ls).

texQuMapLine(S:F) -->
	\\'{', texTVs(S, ","), \\'}', " & ", textv(F).

texOpRule(Op, TV, Cnf, TVs) -->
	{group_prems(Cnf, TVs, Ps)},
	\\def, texopconcl(Op, TV), eol,
	"   {", texOpConcl(Op, TV), "}", eol,
	\\def, texopprems(Op, TV), eol,
	"   {", texOpPrems(Ps), eol,
	"   }", eol.

texOpConcl(N/A, TV) -->
	\\'Gamma', ", [", textv(TV), \\colon, " ", texOpExpr(A, N), "]".

% texOpExpr(Arity,N) - write atomic formula with connective N: eg: A \land B

texOpExpr(0, N) -->
	!,
	texName(N).
texOpExpr(1, N) -->
	{texPrefix(N)},!,
	\\mathord, texName(N), texOpAtom(1).
texOpExpr(2, N) -->
	{texInfix(N)}, !,
	texOpAtom(1), \\mathbin, texName(N), texOpAtom(2).
texOpExpr(A, N) -->
	texName(N), "(", texOpExprArgs(1, A), ")".

% texOpExprStrict(Arity,N) - write atomic formula with connective N, 
% but with parens if needed: eg: (A \land B)

texOpExprStrict(2, N) -->
	{texInfix(N)},
	!,
	"(", texOpExpr(2, N), ")".
texOpExprStrict(A, N) -->
	texOpExpr(A, N).

% texOpExprArgs(N,Arity) - write list of N schematic arguments.

texOpExprArgs(A, A) -->
	texOpAtom(A).
texOpExprArgs(I, A) -->
	{I < A, I1 is I+1},
	texOpAtom(I), ",", 
	texOpExprArgs(I1, A).

texOpPrems([])     -->
	"".
texOpPrems([P])    -->
	!,
	texOpPrem(P).
texOpPrems([P|Ps]) -->
	texOpPrem(P), eol,
	"   &",
	texOpPrems(Ps).

texOpPrem(P) -->
	\\'Gamma', texOpPrem1(P).

texOpPrem1([])        -->
	"".
texOpPrem1([As^Ss|P]) -->
	", [", texTVs(Ss, ","), \\colon, " ", texOpAtoms(As), "]", texOpPrem1(P).

texOpAtoms([A])    -->
	!,
	texOpAtom(A).
texOpAtoms([A|As]) -->
	texOpAtom(A), ",",
	texOpAtoms(As).

texOpAtom(A) -->
	{A =< 26},     % This only works for operators with an arity <= 26
	{L is 0'A+A-1},
	[L].

texQuRule(Qu, TV, Cnf0, TVs) -->
	{name_qucnf(Cnf0, c(0,0), _Ts, Cnf1, C)},
	{group_prems(Cnf1, TVs, Ps)},
	\\def, texquconcl(Qu, TV),  eol,
	"   {", texQuConcl(Qu, TV), "}", eol,
	\\def, texquprems(Qu, TV),  eol,
	"   {", texQuPrems(Ps, C), eol,
	"   }", eol.

texQuConcl(Qu, TV) -->
	\\'Gamma', ", [", textv(TV), \\colon, "(", texName(Qu), \\(','), "x)A(x)]".

texQuPrems([], _)     -->
	"".
texQuPrems([P], C)    -->
	!,
	texQuPrem(P, C).
texQuPrems([P|Ps], C) -->
	texQuPrem(P, C), eol,
	"   &",
	texQuPrems(Ps, C).

texQuPrem(P, C) -->
	\\'Gamma', texQuPrem1(P, C).

texQuPrem1([], _)        -->
	"".
texQuPrem1([As^Ss|P], C) -->
	", [", texTVs(Ss, ","), \\colon, " ", texQuAtoms(As, C), "]",
	texQuPrem1(P, C).

texQuAtoms([A], C)    -->
	!,
	texQuAtom(A, C).
texQuAtoms([A|As], C) -->
	texQuAtom(A, C), ",",
	texQuAtoms(As, C).

texQuAtom(A, C) -->
	"A(", texQuAtomArg(A, C), ")".

texQuAtomArg(a(_I), c(1,_)) -->
	!, \\alpha.
texQuAtomArg(a(I), _)       -->
	\\alpha, "_{", str(I), "}".
texQuAtomArg(t(_I), c(_,1)) -->
	!,
	\\tau.
texQuAtomArg(t(I), _)       -->
	\\tau,   "_{", str(I), "}".


texNDOpRule(Op, TV, Cnf, TVs, DTVs) -->
	{split_cnf(Cnf, DTVs, NCnf, PCnf),
	 group_prems(NCnf, TVs, NPs),
	 group_prems(PCnf, TVs, PPs)
	},
	texNDOpRule1(Op, TV, NPs, PPs, TVs, DTVs).

texNDOpRule1(Op, TV, NPs, PPs, _TVs, DTVs) -->
	{ml_member(TV,DTVs)}, !,
	{ml_length(NPs, N)},
	\\def, texndopconcl(Op, TV), eol,
	"   {", texGammaPlusKomma(1, N),
	texNDPOpConcl(Op, [TV]), "}", eol,
	\\def, texndopprems(Op, TV), eol,
	"   {", texNDPOpPrems(NPs,PPs,1), eol,
	"   }", eol.
texNDOpRule1(Op, TV, NPs, PPs, TVs, DTVs) -->
	{ml_length(NPs, N)},
	\\def, texndopconcl(Op, TV), eol,
	"   {", texGammaPlus(0, N),"}", eol,
	\\def, texndopprems(Op, TV), eol,
	"   {", texNDPOpPrem0(Op, TV, TVs, DTVs), eol,
        "   &", texNDPOpPrems(NPs,PPs,1), eol,
	"   }", eol.

texNDPOpConcl(N/A, TVs) -->
	"[", texTVs(TVs, ","), \\colon, " ", texOpExpr(A, N), "]".

texNDPOpPrems([],[],_I)            -->
	"".
texNDPOpPrems([NP],[PP],I)         -->
	!,
	texNDPOpPrem(NP,PP,I).
texNDPOpPrems([NP|NPs],[PP|PPs],I) -->
	texNDPOpPrem(NP,PP,I), eol,
        "   &", {I1 is I+1},
	texNDPOpPrems(NPs,PPs,I1).
texNDPOpPrem0(Op, TV, TVs, DTVs) -->
	\\deduce, "{", \\'Gamma', "^+_0, ", 
	texNDPOpConcl(Op,DTVs), "}{", \\'Gamma', "^-_0",
	{findall(V, (ml_member(V,TVs),\+ml_member(V,DTVs),\+V=TV), Vs)},
	texNDPOpPrem01(Vs, Op),
	"}".

texNDPOpPrem01([], _Op) -->
	!.
texNDPOpPrem01(Vs, Op)  -->
	", ", \\lceil, texNDPOpConcl(Op,Vs), \\rceil.

texNDPOpPrem(NP,PP,I) -->
	\\deduce, "{", \\'Gamma', "^+_{", str(I), "}", texOpPrem1(PP), "}",
	         "{", \\'Gamma', "^-_{", str(I), "}", texNDOpPrem1(NP), "}".

texNDOpPrem1([])        -->
	"".
texNDOpPrem1([As^Ss|P]) -->
	", ", \\lceil, "[", texTVs(Ss, ","), \\colon, " ", 
	texOpAtoms(As), "]", \\rceil,
	texNDOpPrem1(P).

texNDQuRule(Qu, TV, Cnf0, TVs, DTVs) -->
	{name_qucnf(Cnf0, c(0,0), _Ts, Cnf1, C),
	 split_cnf(Cnf1, DTVs, NCnf, PCnf),
	 group_prems(NCnf, TVs, NPs),
	 group_prems(PCnf, TVs, PPs)
	},
	texNDQuRule1(Qu, TV, NPs, PPs, TVs, DTVs, C).

texNDQuRule1(Qu, TV, NPs, PPs, _TVs, DTVs, C) -->
	{ml_member(TV,DTVs)}, !,
	{ml_length(NPs, N)},
	\\def, texndquconcl(Qu, TV), eol,
	"   {", texGammaPlusKomma(1, N),
	texNDPQuConcl(Qu, [TV]), "}", eol,
	\\def, texndquprems(Qu, TV), eol,
	"   {", texNDPQuPrems(NPs,PPs,1, C), eol,
	"   }", eol.
texNDQuRule1(Qu, TV, NPs, PPs, TVs, DTVs, C) -->
	{ml_length(NPs, N)},
	\\def, texndquconcl(Qu, TV), eol,
	"   {", texGammaPlus(0, N),"}", eol,
	\\def, texndquprems(Qu, TV), eol,
	"   {", texNDPQuPrem0(Qu, TV, TVs, DTVs), eol,
        "   &", texNDPQuPrems(NPs,PPs,1, C), eol,
	"   }", eol.

texGammaPlusKomma(N,Nx) --> {Nx < N}, !.
texGammaPlusKomma(N,Nx) -->
	texGammaPlus(N,Nx), ",".

texGammaPlus(N,Nx) -->
	{Nx < N}, !.
texGammaPlus(N,N) -->
	!,
	texGammaPlus(N).
texGammaPlus(N,N1) -->
	{N1 is N+1}, !,
	texGammaPlus(N), ",", texGammaPlus(N1).
texGammaPlus(N,N2) -->
	{N2 is N+2}, !,
	{N1 is N+1}, 
	texGammaPlus(N), ",", texGammaPlus(N1), ",", texGammaPlus(N2).
texGammaPlus(N,Nx) -->
	texGammaPlus(N), ",", \\ldots, ",", texGammaPlus(Nx).

texGammaPlus(N) -->
	\\'Gamma', "^+_{", str(N), "}".

texNDPQuConcl(Qu, TVs) -->
	"[", texTVs(TVs, ","), \\colon, "(", texName(Qu), \\(','), "x)A(x)]".

texNDPQuPrems([],[],_I,_C)           -->
	"".
texNDPQuPrems([NP],[PP],I,C)         -->
	!,
	texNDPQuPrem(NP,PP,I,C).
texNDPQuPrems([NP|NPs],[PP|PPs],I,C) -->
	texNDPQuPrem(NP,PP,I,C), eol,
        "   &", {I1 is I+1},
	texNDPQuPrems(NPs,PPs,I1,C).

texNDPQuPrem0(Qu, TV, TVs, DTVs) -->
	\\deduce, "{", \\'Gamma', "^+_0, ",
	texNDPQuConcl(Qu,DTVs), "}{", \\'Gamma', "^-_0",
	{findall(V, (ml_member(V,TVs),\+ ml_member(V,DTVs),\+ V=TV), Vs)},
	texNDPQuPrem01(Vs, Qu),
	"}".

texNDPQuPrem01([], _Qu) -->
	!.
texNDPQuPrem01(Vs, Qu)  -->
	", ", \\lceil,
	texNDPQuConcl(Qu,Vs),
	\\rceil.

texNDPQuPrem(NP,PP,I,C) -->
	\\deduce, "{", \\'Gamma', "^+_{", str(I), "}", texQuPrem1(PP,C), "}",
        "{", \\'Gamma', "^-_{", str(I), "}", texNDQuPrem1(NP,C), "}".

texNDQuPrem1([],_C)        -->
	"".
texNDQuPrem1([As^Ss|P],C) -->
	", ", \\lceil, "[", texTVs(Ss, ","), \\colon, " ",
	texQuAtoms(As,C), "]", \\rceil,
	texNDQuPrem1(P,C).

texClOpRule(Op, TV, Cnf) -->
	\\def, texclopconcl(Op, TV), eol,
	"   {", texClOpConcl(Cnf), "}", eol,
	\\def, texclopprem(Op, TV), eol,
	"   {", texClOpPrem(Op, TV), eol,
	"   }", eol.

texClOpConcl([]) -->
	!.
texClOpConcl(Cs) -->
	\\cup, \\'{', texClOpConcl1(Cs), \\'}'.

texClOpConcl1([C]) -->
	!,
	texClOpClause(C).
texClOpConcl1([C|Cs]) -->
	texClOpClause(C), ",", \\(';'),
	texClOpConcl1(Cs).

texClOpClause([]) -->
	!, \\'Clause'.
texClOpClause(C) -->
	\\'Clause', \\cup, \\'{', texClOpLits(C), \\'}'.

texClOpLits([L]) -->
	!,
	texClOpLit(L).
texClOpLits([L|Ls]) -->
	texClOpLit(L), ",",
	texClOpLits(Ls).

texClOpLit(N^TV) -->
	texOpAtom(N), "^", textv(TV).

texClOpPrem(N/A, TV) -->
	\\cup, \\'{', \\'Clause', \\cup, \\'{', texClOpExpr(A, N), "^", textv(TV), \\'}', \\'}'.

texClOpExpr(0, N) -->
	!,
	texName(N).
texClOpExpr(1, N) -->
	{texPrefix(N)},!,
	"(", \\mathord, texName(N), texOpAtom(1), ")".
texClOpExpr(2, N) -->
	{texInfix(N)}, !,
	"(", texOpAtom(1), \\mathbin, texName(N), texOpAtom(2), ")".
texClOpExpr(A, N) -->
	texName(N), "(", texOpExprArgs(1, A), ")".

texClQuRule(Op, TV, Cnf0) -->
	{name_qucnf(Cnf0, c(0,0), _Ts, Cnf1, Cnts)},
	\\def, texclquconcl(Op, TV), eol,
	"   {", texClQuConcl(Cnf1, Cnts), "}", eol,
	\\def, texclquprem(Op, TV), eol,
	"   {", texClQuPrem(Op, TV), eol,
	"   }", eol.

texClQuConcl([], _Cnts) -->
	!.
texClQuConcl(Cs, Cnts) -->
	\\cup, \\'{', texClQuConcl1(Cs, Cnts), \\'}'.

texClQuConcl1([C], Cnts) -->
	!,
	texClQuClause(C, Cnts).
texClQuConcl1([C|Cs], Cnts) -->
	texClQuClause(C, Cnts), ",", \\(';'),
	texClQuConcl1(Cs, Cnts).

texClQuClause([], _Cnts) -->
	!, \\'Clause'.
texClQuClause(C, Cnts) -->
	\\'Clause', \\cup, \\'{', texClQuLits(C, Cnts), \\'}'.

texClQuLits([L], Cnts) -->
	!,
	texClQuLit(L, Cnts).
texClQuLits([L|Ls], Cnts) -->
	texClQuLit(L, Cnts), ",",
	texClQuLits(Ls, Cnts).

texClQuLit(N^TV, Cnts) -->
	"A(", texClQuAtomArg(N, Cnts), ")^", textv(TV).

texClQuAtomArg(a(_I), c(1,_)) -->
	!, "b".
texClQuAtomArg(a(I), _)       -->
	"b_{", str(I), "}".
texClQuAtomArg(t(_I), c(_,1)) -->
	!,
	"f(", \\vec, "{a})".
texClQuAtomArg(t(I), _)       -->
	"f_{", str(I), "}(", \\vec, "{a})".

texClQuPrem(Qu, TV) -->
	\\cup, \\'{', \\'Clause', \\cup, \\'{', "((", texName(Qu), \\(','), "x)A(x))^",
        textv(TV), \\'}', \\'}'.

% Tableaux


texTabOpRule(Op, TV, Cnf) -->
	\\def, textabopconcl(Op, TV), eol,
	"   {", texTabOpConcl(Cnf), "}", eol,
	\\def, textabopprem(Op, TV), eol,
	"   {", texTabOpPrem(Op, TV), eol,
	"   }", eol.

texTabOpConcl([]) -->
	!,
	\\otimes.
texTabOpConcl([C]) -->
	!,
	texTabOpBranch(C).
texTabOpConcl([C|Cs]) -->
	texTabOpBranch(C), \\qquad,
	texTabOpConcl(Cs).

texTabOpBranch([]) -->
	!.
texTabOpBranch(C) -->
	\\begin, "{array}[t]{@{}r@{}l@{}}", 
	texTabOpLits(C), 
	\\end, "{array}".

texTabOpLits([L]) -->
	!,
	texTabOpLit(L).
texTabOpLits([L|Ls]) -->
	texTabOpLit(L), "\\\\ ",
	texTabOpLits(Ls).

texTabOpLit(N^TV) -->
	textv(TV), "&", \\colon, " ", texOpAtom(N).

texTabOpPrem(N/A, TV) -->
	textv(TV), \\colon, " ", texTabOpExpr(A, N).

texTabOpExpr(0, N) -->
	!,
	texName(N).
texTabOpExpr(1, N) -->
	{texPrefix(N)},!,
	\\mathord, texName(N), texOpAtom(1).
texTabOpExpr(2, N) -->
	{texInfix(N)}, !,
	texOpAtom(1), \\mathbin, texName(N), texOpAtom(2).
texTabOpExpr(A, N) -->
	texName(N), "(", texOpExprArgs(1, A), ")".

texTabQuRule(Op, TV, Cnf0) -->
	{name_qucnf(Cnf0, c(0,0), _Ts, Cnf1, Cnts)},
	\\def, textabquconcl(Op, TV), eol,
	"   {", texTabQuConcl(Cnf1, Cnts), "}", eol,
	\\def, textabquprem(Op, TV), eol,
	"   {", texTabQuPrem(Op, TV), eol,
	"   }", eol.

texTabQuConcl([C], Cnts) -->
	!,
	texTabQuBranch(C, Cnts).
texTabQuConcl([C|Cs], Cnts) -->
	texTabQuBranch(C, Cnts), \\qquad,
	texTabQuConcl(Cs, Cnts).

texTabQuBranch([], _Cnts) -->
	!.
texTabQuBranch(C, Cnts) -->
	\\begin, "{array}[t]{@{}r@{}l@{}}", 
	texTabQuLits(C, Cnts),
	\\end, "{array}".

texTabQuLits([L], Cnts) -->
	!,
	texTabQuLit(L, Cnts).
texTabQuLits([L|Ls], Cnts) -->
	texTabQuLit(L, Cnts), "\\\\",
	texTabQuLits(Ls, Cnts).

texTabQuLit(N^TV, Cnts) -->
	textv(TV), "&", \\colon, " A(", texQuAtomArg(N, Cnts), ")".

texTabQuPrem(Qu, TV) -->
	textv(TV), \\colon,  "(", texName(Qu), "x)A(x)".

texTVs([], _Sep)      -->
	"".
texTVs([TV], _Sep)    -->
	!,
	textv(TV).
texTVs([TV|TVs], Sep) -->
	textv(TV), Sep,
	texTVs(TVs, Sep).

textv(TV)        --> {texTVno(TV, I)}, \\('TV', I).
texopname(Op)    --> {texOpno(Op, I)}, \\('Opname', I).
texoparity(Op)   --> {texOpno(Op, I)}, \\('Oparity', I).
texopfmla(Op)    --> {texOpno(Op, I)}, \\('Opfmla', I).
texoptab(Op)     --> {texOpno(Op, I)}, \\('Optab', I).
texquname(Qu)    --> {texQuno(Qu, I)}, \\('Quname', I).
texqutab(Qu)     --> {texQuno(Qu, I)}, \\('Qutab', I).
texopconcl(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('Opconcl', I, J).
texopprems(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('Opprems', I, J).
texndopconcl(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('NDOpconcl', I, J).
texndopprems(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('NDOpprems', I, J).
texclopconcl(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('ClOpconcl', I, J).
texclopprem(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('ClOpprem', I, J).
textabopconcl(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('TabOpconcl', I, J).
textabopprem(Op,TV)--> {texOpno(Op, I), texTVno(TV, J)}, \\('TabOpprem', I, J).
texquconcl(Qu,TV)--> {texQuno(Qu, I), texTVno(TV, J)}, \\('Quconcl', I, J).
texquprems(Qu,TV)--> {texQuno(Qu, I), texTVno(TV, J)}, \\('Quprems', I, J).
texndquconcl(Qu,TV)--> {texQuno(Qu, I), texTVno(TV, J)}, \\('NDQuconcl', I, J).
texndquprems(Qu,TV)--> {texQuno(Qu, I), texTVno(TV, J)}, \\('NDQuprems', I, J).
texclquconcl(Op,TV)--> {texQuno(Op, I), texTVno(TV, J)}, \\('ClQuconcl', I, J).
texclquprem(Op,TV)--> {texQuno(Op, I), texTVno(TV, J)}, \\('ClQuprem', I, J).
textabquconcl(Op,TV)--> {texQuno(Op, I), texTVno(TV, J)}, \\('TabQuconcl', I, J).
textabquprem(Op,TV)--> {texQuno(Op, I), texTVno(TV, J)}, \\('TabQuprem', I, J).


texName(N) --> {texName(N,TN)}, !, "{", TN, "}".
texName(N) --> "{", \\it, " ", str(N), "}".

roman(0)  --> "".
roman(I0) --> {roman(N,R), I0 >= N, !, I1 is I0-N}, R, roman(I1).

roman(1000,  "m").
roman( 900, "cm").
roman( 500,  "d").
roman( 400, "cd").
roman( 100,  "c").
roman(  90, "xc").
roman(  50,  "l").
roman(  40, "xl").
roman(  10,  "x").
roman(   9, "ix").
roman(   5,  "v").
roman(   4, "iv").
roman(   1,  "i").

opmap2textab(M, TVs, Tab) :-
   tablines(TVs, TVs, M, Tab).

tablines([], _, _, []).
tablines([X|Xs], Ys, M, [[X|L]|Ls]) :-
   tabline(Ys, X, M, L),
   tablines(Xs, Ys, M, Ls).

tabline([], _, _, []).
tabline([Y|Ys], X, M, [F|Fs]) :-
   ml_member([X,Y]:F, M),
   tabline(Ys, X, M, Fs).

opmap2tex(M, TVs, TM) :-
   M = [X:_|_],
   ml_length(X, N),
   ml_length(T, N),
   bagof(T:F, (ml_subset(T,TVs), ml_member(T:F,M)), TM).

qumap2tex(M, TVs, TM) :-
   bagof(S:F, SS^(sublist(S,TVs), S\==[], sort(S,SS), ml_member(SS:F,M)), TM).

group_prems([], _TVs, []).
group_prems([P0|Ps0], TVs, [P|Ps]) :-
   group_by_sign(TVs, P0, P1),
   group_by_atom(P1, P),
   group_prems(Ps0, TVs, Ps).

name_qucnf([], C, _Ts, [], C).
name_qucnf([D|Ds], C0, Ts, [E|Es], C) :-
	name_quatoms(D, C0, Ts, E, C1),
	name_qucnf(Ds, C1, Ts, Es, C).

name_quatoms([], C, _, [], C).
name_quatoms([A0^S|P0], C0, Ts, [A^S|P], C) :-
   name_quatom(A0, C0, Ts, A, C1),
   name_quatoms(P0, C1, Ts, P, C).

name_quatom(a(I1), c(I,J), _, a(I1), c(I1,J)) :-
   I1 is I+1, !.
name_quatom(a(I), C, _, a(I), C) :- !.
name_quatom(N, C, Ts, t(J1), C1) :-
   ml_member(N=J1, Ts), !,
   (var(J1) ->
     (C=c(I,J),
      J1 is J+1,
      C1=c(I,J1)
     )
   ;
     C1=C
   )
   .

group_by_sign([], [], []).
group_by_sign([S|Ss], P0, [As^S|P]) :-
   group_by_sign(S, P0, As, P1),
   As \== [], !,
   group_by_sign(Ss, P1, P).
group_by_sign([_|Ss], P0, P) :-
   group_by_sign(Ss, P0, P).

group_by_sign(S, P0, [A|As], P) :-
   remove_member(A^S, P0, P1), !,
   group_by_sign(S, P1, As, P).
group_by_sign(_, P, [], P).

group_by_atom([], []).
group_by_atom([A^S|P0], [A^[S|Ss]|P]) :-
   group_by_atom(A, P0, Ss, P1),
   group_by_atom(P1, P).

group_by_atom(A, P0, [S|Ss], P) :-
   remove_member(A^S, P0, P1), !,
   group_by_atom(A, P1, Ss, P).
group_by_atom(_, P, [], P).

split_cnf([], _DTVs, [], []).
split_cnf([C|Cs], DTVs, [NC|NCs], [PC|PCs]) :-
	split_clause(C, DTVs, NC, PC),
	split_cnf(Cs, DTVs, NCs, PCs).

split_clause([], _DTVs, [], []).
split_clause([L|Ls], DTVs, NLs, [L|PLs]) :-
	L = _^TV,
	ml_member(TV, DTVs), !,
	split_clause(Ls, DTVs, NLs, PLs).
split_clause([L|Ls], DTVs, [L|NLs], PLs) :-
	split_clause(Ls, DTVs, NLs, PLs).
