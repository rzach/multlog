%%% Filename : ilc.pl
%%% Date     : 13.1.1997
%%% Contents : Conversion LGC --> ILC
%%% Author   : Gernot Salzer
%%% Interface:
%%%            lgc2ilc(In,Out).    Translates LGC-file "In" to ILC-file "Out"

:- ['@mlpath@/ml_conf'].
:- ['@mlpath@/ml_util'].
:- ['@mlpath@/ml_error'].
:- ['@mlpath@/ml_lgc'].
:- ['@mlpath@/ml_check'].
:- ['@mlpath@/ml_ilc'].

lgc2ilc(In, Out) :-
	init_errors,
	println,
	who_am_I,
	println_terms(['Converting ', In, ' to ', Out]),
	lgc_in(In),
	check,
	ilc_out(Out).
