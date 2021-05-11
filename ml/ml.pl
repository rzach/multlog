%%% Filename : ml.pl
%%% Contents : Root module of MUltlog
%%% Author   : Gernot Salzer
%%% Interface:
%%%            lgc2tex(In,Out,Cnf). Translates LGC-file "In" to TeX-file "Out"
%%%                                 using configuration in "Cnf"

:- ['@mlpath@/ml_conf'].
:- ['@mlpath@/ml_util'].
:- ['@mlpath@/ml_error'].
:- ['@mlpath@/ml_lgc'].
:- ['@mlpath@/ml_check'].
:- ['@mlpath@/ml_kernl'].
:- ['@mlpath@/ml_tex'].
:- ['@mlpath@/propres_compact'].
:- ['@mlpath@/ml_minimize'].
:- ['@mlpath@/ml_compactrepr'].

:- dynamic cached/1.

lgc2tex(In, Out, CFN) :-
	init_errors,
	println,
	who_am_I,
	print_terms(['Converting ', In, ' to ', Out]),
	ml_if(var(CFN)
	  ,  println
	  ,  println_terms([' using configuration in ', CFN])
	),
	lgc_in(In),
	atom_concat(In, '.stripped', Stripped),
	lgc_stripped(In, Stripped),
	atom_concat(In, '.cache', Cache),
	cache_in(Cache),
	check,
	kernel,
	cache_out(Cache),
	tex_out(Out, CFN).

cache_in(Cache) :-
	(exists_file(Cache) ->
	   [Cache]
	;  true
	).

cache_out(Cache) :-
	tell(Cache),
	(cached(T),
	   write_canonical(cached(T)),
	   write('.'), nl,
	   fail
	;  true
	),
	told.
