%%% Filename : ml_util.pl
%%% Date     : 13.1.1997
%%% Contents : Useful predicates and utilities.
%%% Author   : Gernot Salzer
%%% Interface:
%%%            str(N)                --> {name(N,NS)}, NS.
%%%            tail(L, M).           M is a tail of list L.
%%%            remove_member(E, L, LminusE).   LminusE is obtained from L by removing some
%%%                                  element unifiable with E; fails if there is no such element.
%%%            ol_member(E, L).      Same as member/2, but recognizes the end of open lists.
%%%            ol_add(L, E).         Adds E to the end of open list L.
%%%            ol_close(L).          Close open list L, i.e., append [].
%%%            ml_subset(S,L).       S is a subset of L.
%%%            sublist(S,L).         S is a sublist of L, i.e., S is obtained
%%%                                  from L by deleting some elements and keeping
%%%                                  the order of the remaining elements.
%%%            subslist(S,L).        Same as sublist/2, except that S and L have to be sorted.
%%%            diff(As, Bs, Cs).     Cs contains all elements of As that are not a member of Bs.
%%%            ml_is_set(S).         Succeeds if no two elements in S unify.
%%%            make_set(L, S).       S is obtained from L by merging right.
%%%            skip(N, L0, L).       L is obtained from L0 by skipping the first N elements.
%%%            clear(S).             Retracts all clauses specified by S.
%%%            list(S).              Lists all clauses specified by S.
%%%            print_terms(Ts).      Output a list of terms, Ts.
%%%            println_terms(Ts).    = print_terms(Ts) + print_nl.
%%%            print_term(T).        Print term T; atoms remain unquoted.
%%%            println.              Print end of line.
%%%            print_phrase(V).      Output DCG phrase starting from nonterminal V.
%%%            println_strings(Ss).  Print strings in Ss separated by nl.
%%%            truncate(S0, N, S).   S consists of the first N elements of S0,
%%%                                  if length(S0)>N; fails otherwise.
%%%            ml_member(E, L).      E is occurs in list L; the usual member/2
%%%            ml_append(Xs, Ys, Zs).  Zs is Ys appended to Xs; the usual append/3
%%%            ml_length(L,N).       L is a list of length N; the usual length/2
%%%            ml_if(Cond, Then, Else).
%%%            ml_nth0(N,List,Element) Element is the N'th element in List (N must
%%%                                  be a variable.
%%%            who_am_I

str(A) --> {name(A,S)}, S.

% tail(L, M).           M is a tail of list L.
tail(L, L).
tail([_|L], M) :-
   tail(L, M).

% remove_member(E, L, LminusE).   LminusE is obtained from L by removing some
%                       element unifiable with E; fails if there is no such element.
remove_member(E, [E|L], L).
remove_member(E, [E1|L], [E1|L1]) :-
   remove_member(E, L, L1).

% ol_member(E, L).      Same as member/2, but recognizes the end of open lists.
ol_member(_, L) :-
   var(L), !,
   fail.
ol_member(E, [E|_]).
ol_member(E, [_|L]) :-
   ol_member(E, L).

% ol_add(L, E).         Adds E to the end of open list L.
ol_add(L, E) :-
	var(L), !,
	L=[E|_].
ol_add([_|L], E) :-
	ol_add(L,E).

% ol_close(L).          Close open list L, i.e., append [].
ol_close([]) :-
	!.
ol_close([_|L]) :-
	ol_close(L).

% ml_subset(S,L).       Each element in S unifies with some element in L.
ml_subset([], _L).
ml_subset([E|S], L) :-
   ml_member(E, L),
   ml_subset(S, L).

% sublist(S,L).         S is a sublist of L, i.e., it can be obtained
%                       from L by deleting elements while keeping the
%                       order of the remaining elements.
sublist([], []).
sublist([E|S], [E|Es]) :-
   sublist(S, Es).
sublist(S, [_E|Es]) :-
   sublist(S, Es).

% subslist(S,L).        Same as sublist/2, except that S and L have to be sorted.
%                       Note that the elements are compared by = (unifiability)
%                       and not == (identical).
subslist([], []).
subslist([E|S], [E|Es]) :- !,
   subslist(S, Es).
subslist(S, [_E|Es]) :-
   subslist(S, Es).

% diff(As, Bs, Cs).     Cs contains all elements of As that are not a member of Bs.
diff([], _, []).
diff([A|As], Bs, Cs) :-
   ml_member(A, Bs), !,
   diff(As, Bs, Cs).
diff([A|As], Bs, [A|Cs]) :-
   diff(As, Bs, Cs).

% ml_is_set(S).         Succeeds if no two elements in S unify.
ml_is_set(S) :-
   var(S), !,
   fail.
ml_is_set([]).
ml_is_set([E|S]) :-
   ml_member(E,S), !,
   fail.
ml_is_set([_|S]) :-
   ml_is_set(S).

% make_set(L, S).       S is obtained from L by merging right, with elements being unified.
%                       Example: make_set([1,2,3,X,2], [3,1,2]).
make_set([], []).
make_set([E|L], S) :-
   ml_member(E, L), !,
   make_set(L, S).
make_set([E|L], [E|S]) :-
   make_set(L, S).

% skip(N, L0, L).       L is the list obtained from L0 by skipping the first N elements.
skip(0, L, L).
skip(N0, [_|L0], L) :-
   N0 > 0,
   N1 is N0 - 1,
   skip(N1, L0, L).

% clear(S).             Retracts all clauses specified by S.
clear([]).
clear([S|Ss]) :-
   clear(S),
   clear(Ss).
clear(P/A) :-
   functor(H,P,A),
   retractall(H).
clear(P) :-
   atom(P),
   P \== [],
   G =..[P,S],
   G,
   clear(S).

% list(S).              Lists all clauses specified by S.
list([]).
list([S|Ss]) :-
   list(S),
   list(Ss).
list(P/A) :-
   functor(H,P,A),
   (clause(H, B),
      print_terms([H]),
      ml_if(B==true, println_terms(['.']), println_terms([' :- ', B, '.'])),
      fail
   ;  true
   ).
list(P) :-
   atom(P),
   P \== [],
   G =..[P,S],
   G,
   list(S).

% print_term(T).        Print term T.
print_term(T) :-
   write(T).  %<<<<<<<<<<<<<<<<<<<<< write/1 HAS TO BE BUILTIN !!!!!!!!!!!!!!!

% print_terms(Ts).      Output a list of terms, Ts.
print_terms([]).
print_terms([T|Ts]) :-
   print_term(T),
   print_terms(Ts).

% println_terms(Ts).    = print_terms(Ts) + print_nl.
println_terms(Ts) :-
   print_terms(Ts),
   println.

% println.              Print end of line.
println :-
   print_phrase(eol).

% print_phrase(V).      Output DCG phrase starting from nonterminal V.
print_phrase(V) :-
   phrase(V, S),
   name(A, S),
   write(A).  %<<<<<<<<<<<<<<<<<<<<< write/1 HAS TO BE BUILTIN !!!!!!!!!!!!!!!

% println_strings(Ss).  Print strings in Ss separated by nl.
println_strings([]).
println_strings([S|Ss]) :-
	name(N, S),
	println_terms([N]),
	println_strings(Ss).

% truncate(S0, N, S).   S consists of the first N elements of S0, if length(S0)>N;
%                       fails otherwise.
truncate([_|_], 0, []) :-
	!.
truncate([E|S0], N0, [E|S]) :-
	N1 is N0 - 1,
	truncate(S0, N1, S).

% ml_member(E, L).      E is occurs in list L; the usual member/2
ml_member(E, [E|_]).
ml_member(E, [_|L]) :-
	ml_member(E, L).

% ml_append(Xs, Ys, Zs).  Zs is Ys appended to Xs; the usual append/3
ml_append([], Ys, Ys).
ml_append([X|Xs], Ys, [X|Zs]) :-
	ml_append(Xs, Ys, Zs).

% ml_length(L,N).       L is a list of length N; the usual length/2
ml_length(L,N):-var(N),!,ml_get_length(L,0,N).
ml_length(L,N):-ml_make_length(L,0,N).

ml_get_length([],I,I).
ml_get_length([_|L],I0,I):-I1 is I0+1,ml_get_length(L,I1,I).

ml_make_length([],I,I):-!.
ml_make_length([_|L],I0,I):-I0<I,I1 is I0+1,ml_make_length(L,I1,I).

% ml_if(Cond, Then, Else).
ml_if(A,B,_):-A,!,B.
ml_if(_,_,C):-C.


% the nth0 predicate
ml_nth0(N, List, Element) :-
        var(N),
        nth0v(List, Element, 0, N).
nth0v([Element|_], Element, Index, Index).
nth0v([_|Tail], Element, M, Index) :-
        N is M + 1,
        nth0v(Tail, Element, N, Index). 

who_am_I :-
	version_number(Vno),
	version_date(Date),
	operating_system(OS),
	prolog_system(PS),
	println_terms(['This is MUltlog ', Vno, ' (', Date, ') for ', PS, '/', OS, '.']).
