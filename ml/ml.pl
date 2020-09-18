%%% Filename : ml.pl
%%% Date     : 13.1.1997
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
	check,
	kernel,
	tex_out(Out, CFN).
