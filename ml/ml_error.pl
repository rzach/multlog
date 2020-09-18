%%% Filename : ml_error.pl
%%% Date     : 16.8.1997
%%% Contents : Error handling.
%%% Author   : Gernot Salzer
%%% Interface:
%%%            init_errors.         Reset errors.
%%%            error(T,M).          Asserts the fact that an error of type T has
%%%                                 occurred and outputs message M.
%%%            errOccurred(T,M).    Error M (=list of strings) of type T has occurred.

:- dynamic   errOccurred/2.

errDatabase([errOccurred/2]).

init_errors :-
	clear(errDatabase).

error(Type, [M0|Ms0]) :-
	phrase(error_type(Type), TypeMess),
	ml_append(TypeMess, M0, M1),
	Ms1 = [M1|Ms0],
	assertz(errOccurred(Type, Ms1)),
	telling(File),
	tell(user),
	println_strings(Ms1),
	told,
	tell(File).

error_type(Type) -->
	"*** ",
	error_type1(Type),
	": ".

error_type1(openerror) -->
	"Open error".
error_type1(internal) -->
	"Internal error".
error_type1(syntax) -->
	"Syntax error".
error_type1(ordsyntax(N/ord)) -->
	"Syntax error in ordering '", str(N), "'".
error_type1(semantics) -->
	"Semantics error".
error_type1(semantics(Ord/ord)) --> !,
	"Error in ordering '", str(Ord), "'".
error_type1(semantics(Qu/q)) --> !,
	"Error in quantifier '", str(Qu), "'".
error_type1(semantics(N/A)) --> !,
	"Error in operator '", str(N), "/", str(A), "'".

