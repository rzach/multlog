%%% ml_lgc.pl, scanner and parser for lgc files
%%% Interface: lgc_in(FN).       Translates the declarations on LGC-file to
%%%                              their internal representation.
%%%                              Performs basic syntax checks.
%%%  9. 1.1997, Gernot Salzer: initial version
%%%  9. 5.2021, Gernot Salzer: generation of lgc file stripped of comments

:- set_prolog_flag(double_quotes, codes).
:- dynamic   lgcFN/1, lgcLN/1, lgcTVs/1, lgcDTVs/1, lgcOrd/2, lgcOrd1/2, lgcOp/2, lgcQu/2.

lgcDatabase([lgcFN/1, lgcLN/1, lgcTVs/1, lgcDTVs/1, lgcOrd/2, lgcOrd1/2, lgcOp/2, lgcQu/2]).

lgc_in(FN) :-
	clear(lgcDatabase),
	see(FN), !,
	asserta(lgcFN(FN)),
	repeat,
	   get_chars(Cs, Errs_get),
	   process_get_errors(Errs_get, Cs),
	   ml_if(  (Errs_get=[]; Errs_get=[eof])
	     ,  (phrase(tokens(Ts), Cs)
		,ml_if(  phrase(decls(Decls), Ts)
		   ,  process_declarations(Decls)
		   ,  find_and_process_parse_errors(edecls, Ts, syntax)
		   )
		)
	     ,  true
	     ),
	ml_member(eof, Errs_get),
	!,
	seen.

lgc_in(FN) :-
	phrase(lgc_errmess(cannot_read_file(FN)), Message),
	error(openerror, [Message]).

lgc_stripped(FN, Stripped) :-
	see(FN),
	tell(Stripped),
	!,
	repeat,
	   get_chars(Cs0, Errs_get),
	   (append(_, Cs1, Cs0),Cs1=[C|_],C > 32 ->
	      atom_codes(CsA, Cs1),
	      write(CsA), nl
	   ;  true
       ),
	   ml_member(eof, Errs_get),
	   !,
	seen,
	told.
lgc_stripped(FN, _Stripped) :-
	phrase(lgc_errmess(cannot_read_file(FN)), Message),
	error(openerror, [Message]).

process_declarations([]).
process_declarations([D|Ds]) :-
	process_declaration(D),
	process_declarations(Ds).

process_declaration(D) :-
	assertz(D),
	ml_if(  D = lgcOrd(N,S)
	  ,  (phrase(tokens(Ts), S),
	      ml_if(  phrase(ordspec(OS), Ts)
	        ,  assertz(lgcOrd1(N,OS))
	        ,  find_and_process_parse_errors(eordspec, Ts, ordsyntax(N/ord))
	        )
	     )
	  ,  true
	  ).

/**************************** Error handling *****************************/

process_get_errors([], _Cs).
process_get_errors([E|Es], Cs) :-
	process_get_error(E, Cs),
	process_get_errors(Es, Cs).

process_get_error(eof, _Cs).
process_get_error(Err, _Cs) :-
	phrase(lgc_errmess(Err), Message0),
	capitalize(Message0, Message1),
	error(syntax, [Message1]).
process_get_error(Err-Cs1, Cs0) :-
	phrase(lgc_errmess(Err), Message0),
	capitalize(Message0, Message1),
	errcontext(Cs1, Cs0, Context, Pos),
	errposmark(Pos, Position),
	error(syntax, [Message1, Context, Position]).

find_and_process_parse_errors(Start, Ts, Errtype) :-
	phrase(Start, [Ts-Errs], [[]-[]]),
	!,
	process_parse_errors(Errs, Ts, [dummy], Errtype).

process_parse_errors([], _Ts, _Pos0, _Errtype).
process_parse_errors([E|Es], Ts, Pos0, Errtype) :-
	process_parse_error(E, Ts, Pos0, Pos1, Errtype),
	process_parse_errors(Es, Ts, Pos1, Errtype).

process_parse_error(_Err-Ts1, _Ts0, Ts1, Ts1, _Errtype) :-
	!.
process_parse_error(skip-Ts1, _Ts0, _, Ts1, _Errtype) :-
	!.
process_parse_error(Err-Ts1, Ts0, _, Ts1, Errtype) :-
	tokens2string(Ts1, Cs1),
	tokens2string(Ts0, Cs0),
	phrase(lgc_errmess(Err), Message0),
	capitalize(Message0, Message1),
	errcontext(Cs1, Cs0, Context, Pos),
	errposmark(Pos, Position),
	error(Errtype, [Message1, Context, Position]).

lgc_errmess(cannot_read_file(FN)) -->
	"can't open LGC file '", str(FN), "' for reading.".
lgc_errmess(eof_in_comment) -->
	"EOF encountered while scanning comment.".
lgc_errmess(eof_in_string) -->
	"EOF encountered while scanning string.".
lgc_errmess(illegal_char_in_string) -->
	"illegal character in string.".
lgc_errmess(string_too_long(Kind)) -->
	pseudotoken(Kind),
	" exceeds length limit of ",
	{max_length(Kind, ML)}, str(ML), " characters.".
lgc_errmess(exp(Ts)) -->
	pseudotokens(Ts),
	" expected.".

pseudotokens([T]) -->
	pseudotoken(T).
pseudotokens([T1, T2]) -->
	pseudotoken(T1),
	" or ",
	pseudotoken(T2).
pseudotokens(Ts) -->
	{Ts = [_, _, _ | _]},
	pseudotokens1(Ts).

pseudotokens1([T]) -->
	"or ",
	pseudotoken(T).
pseudotokens1([T|Ts]) -->
	{Ts = [_ | _]},
	pseudotoken(T),
	", ",
	pseudotokens1(Ts).

pseudotoken(opname)	--> "name of operator".
pseudotoken(quname)	--> "name of quantifier".
pseudotoken(ordname)	--> "name of ordering".
pseudotoken(truthvalue)	--> "truth value".
pseudotoken(logname)	--> "name of logic".
pseudotoken(ordspec)	--> "specification of ordering".
pseudotoken(decl)	--> "start of declaration".
pseudotoken(ordel)	--> "truth value or '{'".
pseudotoken(opspec)	-->
	"operator specification (mapping, table, sup, or inf)".
pseudotoken(opmap)	-->
	"truth value or '('".
pseudotoken(quspec)	-->
	"quantifier specification (mapping or induced_by)".
pseudotoken(indspec)	-->
	"specification of induced quantifier (sup, inf, or operator)".
pseudotoken(name(K,_))	--> {var(K)}, "name".
pseudotoken(name(K,S))	--> {K==id,  var(S)}, "identifier".
pseudotoken(name(K,S))	--> {K==id,  nonvar(S)}, "'", S, "'".
pseudotoken(name(K,S))	--> {K==num, var(S)}, "number".
pseudotoken(name(K,S))	--> {K==num, nonvar(S)}, "'", S, "'".
pseudotoken(name(K,S))	--> {K==sym, var(S)}, "symbol".
pseudotoken(name(K,S))	--> {K==sym, nonvar(S)}, "'", S, "'".
pseudotoken(string(S))	--> {var(S)}, "string".
pseudotoken(string(S))	--> {nonvar(S)}, "'", S, "'".
pseudotoken(end)	--> "end".
pseudotoken(T)		--> {functor(T,_,0)}, "'", token(T), "'".

capitalize([Lc|S], [Uc|S]) :-
	0'a =< Lc, Lc =< 0'z, !,
	Uc is Lc - 0'a + 0'A.
capitalize(S, S).

tokens2string(Ts, Cs) :-
	phrase(tokens2string(Ts), Cs).

tokens2string([name(id,I), name(num,N)|Ts]) -->
	!,
	token(name(id,I)),
	" ",
	tokens2string([name(num,N)|Ts]).
tokens2string([name(K,S1), name(K,S2)|Ts]) -->
	!,
	token(name(K,S1)),
	" ",
	tokens2string([name(K,S2)|Ts]).
tokens2string([string(S1), string(S2)|Ts]) -->
	!,
	token(string(S1)),
	" ",
	tokens2string([string(S2)|Ts]).
tokens2string([end]) -->
	!,
	[].
tokens2string([T|Ts]) -->
	token(T),
	tokens2string(Ts).
tokens2string([]) -->
	[].

errcontext(After0, All0, All, Pos) :-
   ml_append(Before0, After0, All0),
   max_length(errcontext_before, Mbefore),
   ml_length(Before0, Lbefore0),
   ml_if(Lbefore0 =< Mbefore,
        (Pos     = Lbefore0,
         Before2 = Before0
        )
     ,  (Pos     = Mbefore,
         Lbefore1 is Mbefore-4,
         ml_length(Before1, Lbefore1),
         ml_append(_, Before1, Before0),
         ml_append("... ", Before1, Before2)
        )
   ),
   max_length(errcontext_after, Mafter),
   ml_length(After0, Lafter0),
   ml_if(Lafter0 =< Mafter,
         After2  = After0
     ,  (Lafter1 is Mafter-4,
         ml_length(After1, Lafter1),
         ml_append(After1, _, After0),
         ml_append(After1, " ...", After2)
        )
   ),
   ml_append(Before2, After2, All).

errposmark(0, "^ HERE").
errposmark(N, [0' |S]) :-
   N > 0,
   N1 is N-1,
   errposmark(N1, S).



/******************************** Read chars from file ********************************/

get_chars(Cs, Errs) :-
	repeat,
	get0(C),
%	(32 < C, C =\= 128; C = -1),
	!,
	get_chars(C, Cs, Errs).

get_chars(-1, [], [eof]) :- !.             /* end of file                  */
get_chars(0'., [0'.], []) :- !.            /* end of declaration           */
get_chars(0'", [0'"|Cs], Errs) :- !,       /* start of quoted string       */
	get0(C),
	in_string(C, Cs, Errs).
get_chars(0'/, Cs, Errs) :- !,             /* Possible start of comment    */
	get0(C),
	'/*comment?'(C, Cs, Errs).
get_chars(0'%, [0' /**/|Cs], Errs) :- !,   /* %-comment                    */
	repeat,
	get0(C),
	(C = 10; C = 13; C = -1), /* eol or eof */
	!,
	'end%comment'(C, Cs, Errs).
get_chars(C0, [C0|Cs], Errs) :-            /* significant character        */
%	(32 < C0, C0 =\= 128), !,
	get0(C1),
	get_chars(C1, Cs, Errs).
%get_chars(_C, [0' /**/|Cs], Errs) :-       /* white spaces                 */
%	get_chars(Cs, Errs).

'/*comment?'(0'*, [0' /**/|Cs], Errs) :- !,
	get0(C),
	'in/*comment'(C, Cs, Errs).
'/*comment?'(C, [0'/|Cs], Errs) :-
	get_chars(C, Cs, Errs).

'in/*comment'(-1, Cs, [eof_in_comment|Errs]) :- !,
	get_chars(-1, Cs, Errs).
'in/*comment'(0'*, Cs, Errs) :- !,
	get0(C),
	'end/*comment?'(C, Cs, Errs).
'in/*comment'(_C0, Cs, Errs) :-
	repeat,
	get0(C1),
	(C1 = 0'*; C1 = -1),
	!,
	'in/*comment'(C1, Cs, Errs).

'end/*comment?'(-1, Cs, [eof_in_comment|Errs]) :- !,
	get_chars(-1, Cs, Errs).
'end/*comment?'(0'/, Cs, Errs) :- !,
	get_chars(Cs, Errs).
'end/*comment?'(C, Cs, Errs) :-
	'in/*comment'(C, Cs, Errs).

'end%comment'(-1, Cs, Errs) :- !,
	get_chars(-1, Cs, Errs).
'end%comment'(_C, Cs, Errs) :-
	get_chars(Cs, Errs).

in_string(-1, Cs, [eof_in_string|Errs]) :- !,
	get_chars(-1, Cs, Errs).
in_string(C0, [C0|Cs], [illegal_char_in_string-[C0|Cs]|Errs]) :-
	(C0 < 32; 127 < C0), !,
	get0(C1),
	in_string(C1, Cs, Errs).
in_string(0'", [0'"|Cs], Errs) :- !,
	get0(C),
	'end_string?'(C, Cs, Errs).
in_string(C0, [C0|Cs], Errs) :-
	get0(C1),
	in_string(C1, Cs, Errs).

'end_string?'(0'", [0'"|Cs], Errs) :- !,
	get0(C),
	in_string(C, Cs, Errs).
'end_string?'(C, Cs, Errs) :-
	get_chars(C, Cs, Errs).



/********************************* Tokenizer **********************************/

tokens(Ts) -->
	spaces,
	tokens1(Ts),
	!.

tokens1([T|Ts]) -->
	token(T),
	spaces,
	tokens1(Ts).
tokens1([end]) -->
	[].

spaces -->
    space(_),
	spaces.
spaces -->
	[].

token(lpar)   --> "(".
token(rpar)   --> ")".
token(lbrace) --> "{".
token(rbrace) --> "}".
token(lbrack) --> "[".
token(rbrack) --> "]".
token(period) --> ".".
token(colon)  --> ":".
token(comma)  --> ",".
token(slash)  --> "/".
token(name(num,Cs)) -->
	digits(Cs),
	{Cs = [_|_]}.
token(name(sym,Cs)) -->
	symbols(Cs),
	{Cs = [_|_]}.
token(name(id,[C|Cs])) -->
	lower(C),
	diglets(Cs).
token(string(Cs)) -->
	string(Cs).
token(undef(C)) -->
	[C].

digits([C|Cs]) -->
	digit(C),
	digits(Cs).
digits([]) -->
	[].

symbols([C|Cs]) -->
	symbol(C),
	symbols(Cs).
symbols([]) -->
	[].

diglets([C|Cs]) -->
	(digit(C); lower(C); upper(C)),
	diglets(Cs).
diglets([]) -->
	[].

digit(C)  --> [C], {0'0 =< C, C =< 0'9}.
symbol(C) --> [C], {ml_member(C, "-+*^<>=~?@#$&")}.
lower(C)  --> [C], {0'a =< C, C =< 0'z}.
upper(C)  --> [C], {0'A =< C, C =< 0'Z; C == 0'_}.
space(C)  --> [C], {C =< 0' /**/}.

string(Cs) -->
	"""",
	string1(Cs),
	"""".

string1([0'"|Cs]) -->
	"""""",
	string1(Cs).
string1([C|Cs]) -->
	[C],
	{C =\= 0'"},
	string1(Cs).
string1([]) -->
	[].



/*************** Parser for syntactically correct specs ******************/

decls([])          --> [end].
decls([D|Ds])      --> decl(D), [period], decls(Ds).

decl(lgcLN(N))     --> [name(id,"logic")], pstring(logname, N).
decl(lgcTVs(TVs))  --> [name(id,"truth_values"), lbrace], tvs(TVs), [rbrace].
decl(lgcDTVs(TVs)) --> [name(id,"designated_truth_values"), lbrace], tvs(TVs),
		       [rbrace].
decl(lgcOrd(N,S))  --> [name(id,"ordering")  , lpar], pname(ordname, N),
		       [comma], pstring(ordspec, S), [rpar].
decl(lgcOp(N/A1,S))--> [name(id,"operator")  , lpar], pname(opname, N),
		       [slash, name(num,A0), comma], opspec(S), [rpar],
		       {name(A1,A0)}.
decl(lgcQu(N,S))   --> [name(id,"quantifier"), lpar], pname(quname, N),
		       [comma], quspec(S), [rpar].

ordspec(Es)        --> ordspec1(Es), [end].

ordspec1([E|Es])   --> ordel(E), ordspec2(Es).

ordspec2([])	   --> [].
ordspec2(Es)       --> [name(sym,"<")], ordspec1(Es).

ordel(tv(TV))	   --> pname(truthvalue, TV).
ordel(Ss)	   --> [lbrace], ordspecs(Ss), [rbrace].

ordspecs([S|Ss])   --> ordspec1(S), ordspecs1(Ss).

ordspecs1([])	   --> [].
ordspecs1(Ss)      --> [comma], ordspecs(Ss).

opspec(map(M))     --> [name(id,"mapping"), lbrace], opmap(M), [rbrace].
opspec(tab(T))     --> [name(id,"table")  , lbrack], tvs(T)  , [rbrack].
opspec(sup(N))     --> [name(id,"sup"), lpar], pname(ordname,N), [rpar].
opspec(inf(N))     --> [name(id,"inf"), lpar], pname(ordname,N), [rpar].

opmap([[]:TV])	   --> pname(truthvalue, TV).
opmap([Ass|M])	   --> opass(Ass), opmap1(M).

opmap1([])	   --> [].
opmap1([Ass|M])	   --> [comma], opass(Ass), opmap1(M).

opass(TVs:TV)	   --> [lpar], tvs(TVs), [rpar, colon],
		       pname(truthvalue, TV).

quspec(map(M))	   --> [name(id,"mapping"), lbrace], qumap(M), [rbrace].
quspec(S)	   --> [name(id,"induced_by")], quspec1(S).

quspec1(sup(N))	   --> [name(id,"sup"), lpar], pname(ordname, N), [rpar].
quspec1(inf(N))	   --> [name(id,"inf"), lpar], pname(ordname, N), [rpar].
quspec1(op(N/A1))  --> pname(opname, N), [slash, name(num,A0)],
		       {name(A1,A0)}.

qumap([Ass|M])	   --> quass(Ass), qumap1(M).

qumap1([])	   --> [].
qumap1(M)	   --> [comma], qumap(M).

quass(TVs:TV)	   --> [lbrace], tvs(TVs1), {sort(TVs1, TVs)},
		       [rbrace, colon], pname(truthvalue, TV).

tvs([TV|TVs])	   --> pname(truthvalue, TV), tvs1(TVs).

tvs1([])	   --> [].
tvs1(TVs)	   --> [comma], tvs(TVs).

pname(Kind, N) -->
	[name(_, S)],
	{max_length(Kind, ML), ml_length(S, SL), SL =< ML},
	{name(N, S)}.

pstring(Kind, S) -->
	[string(S)],
	{max_length(Kind, ML), ml_length(S, SL), SL =< ML}.



/****************** Parser for erroneous specs **********************/

edecls -->
	etoken(end).
edecls -->
	edecl([period,end]),
	exp(period, [name(id,"logic"),name(id,"truth_values"),name(id,"designated_truth_values"),
             name(id,"ordering"), name(id,"operator"), name(id,"quantifier"), end] ),
	edecls.

edecl(F) -->
        echoice([name(id,"logic"),name(id,"truth_values"),name(id,"designated_truth_values"),
                 name(id,"ordering"), name(id,"operator"), name(id,"quantifier")], F, [decl]),
	edecl1(F).

edecl1(F) -->
	etoken(name(id,"logic")),
	estring(logname, F).
edecl1(F) -->
	( etoken(name(id,"truth_values"))
        ; etoken(name(id,"designated_truth_values"))
        ),
	exp(lbrace, [name(_,_), rbrace | F]),
	etvs(rbrace, F),
	exp(rbrace, F).
edecl1(F) -->
	etoken(name(id,"ordering")),
	exp(lpar, [name(_,_), comma, string(_), rpar | F]),
	ename(ordname, [comma, string(_), rpar | F]),
	exp(comma, [string(_), rpar | F]),
	estring(ordspec, [rpar | F]),
	exp(rpar, F).
edecl1(F) -->
	etoken(name(id,"operator")),
	exp(lpar, [name(_,_), slash, comma, rpar | F]),
	ename(opname, [slash, name(num,_), comma, name(id,"mapping"), name(id,"table"),
                            name(id,"sup"), name(id,"inf"), rpar | F]),
	exp(slash, [name(num,_), comma, name(id,"mapping"), name(id,"table"),
                            name(id,"sup"), name(id,"inf"), rpar | F]),
	exp(name(num,_), [comma, name(id,"mapping"), name(id,"table"),
                            name(id,"sup"), name(id,"inf"), rpar | F]),
	exp(comma, [name(id,"mapping"), name(id,"table"),
                            name(id,"sup"), name(id,"inf"), rpar | F]),
	eopspec([rpar | F]),
	exp(rpar, F).
edecl1(F) -->
	etoken(name(id,"quantifier")),
	exp(lpar, [name(_,_), comma, rpar | F]),
	ename(quname, [comma, name(id,"mapping"), name(id,"induced_by"), rpar | F]),
	exp(comma, [name(id,"mapping"), name(id,"induced_by"), rpar | F]),
	equspec([rpar | F]),
	exp(rpar, F).
edecl1(_) -->
	[].

eordspec -->
	eordspec([end], []),
	etoken(end).

eordspec(R, F) -->
	{ml_append(R, F, RF)},
	eordel([name(sym,"<")|RF]),
	echoice([name(sym,"<")|R], [ name(_,_), lbrace |F]),
	eordspec1(R, F).

eordspec1(R, F) -->
	( etoken(name(sym,"<"))
	; enext(name(_,_))
	; enext(lbrace)
	),
	eordspec(R, F).
eordspec1(_, _) -->
	[].

eordel(F) -->
	echoice([name(_,_), lbrace], F, [ordel]),
	eordel1(F).

eordel1(F) -->
	enext(name(_,_)),
	ename(truthvalue, F).
eordel1(F) -->
	etoken(lbrace),
	eordspecs([rbrace | F]),
	exp(rbrace, F).
eordel1(_) -->
	[].
	
eordspecs(F) -->
	eordspec([comma, rbrace], F),
	echoice([comma, rbrace], [name(_,_), lbrace | F]),
	eordspecs1(F).

eordspecs1(F) -->
	( etoken(comma)
	; enext(name(_,_))
	; enext(lbrace)
	),
	eordspecs(F).
eordspecs1(_) -->
	[].

eopspec(F) -->
        echoice([name(id,"mapping"), name(id,"table"),
	         name(id,"sup"), name(id,"inf")], F, [opspec]),
	eopspec1(F).

eopspec1(F) -->
	etoken(name(id,"mapping")),
	exp(lbrace, [name(_,_), lpar, rbrace | F]),
	eopmap([rbrace | F]),
	exp(rbrace, F).
eopspec1(F) -->
	etoken(name(id,"table")),
	exp(lbrack, [name(_,_), rbrack | F]),
	etvs(rbrack, F),
	exp(rbrack, F).
eopspec1(F) -->
	( etoken(name(id,"sup"))
	; etoken(name(id,"inf"))
	),
	exp(lpar, [name(_,_), rpar | F]),
	ename(ordname, [rpar | F]),
	exp(rpar, F).
eopspec1(_) -->
	[].

eopmap(F) -->
	echoice([name(_,_), lpar], F, [opmap]),
	eopmap1(F).

eopmap1(F) -->
	enext(name(_,_)),
	ename(truthvalue, F).
eopmap1(F) -->
	enext(lpar),
	eopasgn([comma, lpar | F]),
	eopasgns(F).
eopmap1(_) -->
	[].

eopasgns(F) -->
	echoice([comma, rbrace], [lpar | F]),
	eopasgns1(F).

eopasgns1(F) -->
	( etoken(comma)
	; enext(lpar)
	),
	eopasgn([comma, lpar | F]),
	eopasgns(F).
eopasgns1(_) -->
	[].

eopasgn(F) -->
	exp(lpar, [name(_,_), rpar, colon | F]),
	etvs(rpar, [colon, name(_,_) | F]),
	exp(rpar, [colon, name(_,_) | F]),
	exp(colon, [name(_,_) | F]),
	ename(truthvalue, F).

equspec(F) -->
        echoice([name(id,"mapping"), name(id,"induced_by")], F, [quspec]),
	equspec1(F).

equspec1(F) -->
	etoken(name(id,"mapping")),
	exp(lbrace, [lbrace, rbrace |F]),
	equmap([rbrace |F]),
	exp(rbrace, F).
equspec1(F) -->
	etoken(name(id,"induced_by")),
	echoice([name(id,"sup"), name(id,"inf"), name(_,_)], F, [indspec]),
	equspec2(F).
equspec1(_) -->
	[].

equspec2(F) -->
	( etoken(name(id,"sup"))
	; etoken(name(id,"inf"))
	),
	echoice([lpar, slash], F),
	equspec3(F).
equspec2(F) -->
	enext(name(_,_)),
	ename(opname, []),
	exp(slash, [name(num,_) |F]),
	exp(name(num,_), F).
equspec2(_) -->
	[].

equspec3(F) -->
	etoken(lpar),
	ename(ordname, [rpar |F]),
	exp(rpar, F).
equspec3(F) -->
	etoken(slash),
	exp(name(num,_), F).
equspec3(_) -->
	[].

equmap(F) -->
	equasgn([comma, lbrace |F]),
	equasgns(F).

equasgns(F) -->
	echoice([comma, rbrace], [lbrace | F]),
	equasgns1(F).

equasgns1(F) -->
	( etoken(comma)
	; enext(lbrace)
	),
	equasgn([comma, lpar | F]),
	equasgns(F).
equasgns1(_) -->
	[].

equasgn(F) -->
	exp(lbrace, [name(_,_), rbrace, colon | F]),
	etvs(rbrace, [colon, name(_,_) | F]),
	exp(rbrace, [colon, name(_,_) | F]),
	exp(colon, [name(_,_) | F]),
	ename(truthvalue, F).

etvs(R, F) -->
	ename(truthvalue, [comma, R |F]),
	echoice([comma, R], [name(_,_) |F]),
	etvs1(R, F).

etvs1(R, F) -->
	( etoken(comma)
	; enext(name(_,_))
	),
	etvs(R, F).
etvs1(_, _) -->
	[].

ename(Kind, F) -->
	echoice([name(_,_)], F, [Kind]),
	ename1(Kind).

ename1(Kind) -->
	etoken(name(_,S)),
	check_stringlength(Kind, S).
ename1(_) -->
	[].

estring(Kind, F) -->
	echoice([string(_)], F, [Kind]),
	estring1(Kind).

estring1(Kind) -->
	etoken(string(S)),
	check_stringlength(Kind, S).
estring1(_) -->
	[].

check_stringlength(_Kind, S) -->
	{var(S)}.
check_stringlength(Kind, S) -->
	{nonvar(S),
	 max_length(Kind, ML),
	 ml_length(S, SL),
	 SL =< ML
	}.
check_stringlength(Kind, S) -->
	{nonvar(S),
	 max_length(Kind, ML),
	 ml_length(S, SL),
	 SL > ML
	},
	error_signal(string_too_long(Kind)).

exp(T, F) -->
	echoice([T], F),
	exp1(T).

exp1(T) -->
	etoken(T).
exp1(_) -->
	[].

echoice(Ts, F) -->
	echoice(Ts, F, Ts).

echoice(Ts, F, PTs) -->
	enext(T),
	{\+ ml_member(T, Ts)},
	!,
	error_signal(exp(PTs)),
	{ml_append(Ts, F, TsF)},
	eskip(TsF).
echoice(_, _, _) -->
	[].

eskip(Ts) -->
	enext(T),
	{\+ ml_member(T, Ts)},
	!,
	etoken(_),
	error_signal(skip),
	eskip(Ts).
eskip(_) -->
	[].

enext(T), [[T|Ts]-Es] -->
	[[T|Ts]-Es].

etoken(T), [Ts-Es] -->
	[[T|Ts]-Es].

error_signal(E), [Ts-Es] -->
	[Ts-[E-Ts|Es]].
