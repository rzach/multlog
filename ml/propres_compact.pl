% -*- fundamental -*-

%%% Filename : propres_compact.pl
%%% Date     : 3.2.2001
%%% Contents : Efficient resolution (using binary clause representations)
%%% Author   : Stefan Kral and Stefan Katzenbeisser
%%% Interface: saturate/2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% saturate/2
%   InputClauses: initial set of clauses
%   Saturated:    final set of clauses saturated under prop.
%                 sets-as-signs resolution and subsumption
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

saturate(InputClauses, Saturated) :-
	newclauses_sat0_sat(InputClauses, [], Saturated).

newclauses_sat0_sat([], S, S).
newclauses_sat0_sat([C|Cs], S0, S) :-
	(  ml_member(X, S0), 
	   subsumed_by(C, X)
	-> newclauses_sat0_sat(Cs, S0, S)	% skip C
	;  filter_(S0, C, S1),
	   resolve_(S1, C, Cs, [C|S1], S)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_([], _C, []).
filter_([A|As], C, Bs0) :-
	(  subsumed_by(A, C)
	-> Bs0 = Bs
	;  Bs0 = [A|Bs]
	),
	filter_(As, C, Bs).

resolve_([], _C, Cs, S0, S) :-
	newclauses_sat0_sat(Cs, S0, S).
resolve_([D|Ds], C, Cs, S0, S) :-
	resolvents(D,C,Rs),
	newclauses_sat0_sat(Rs, S0, S1),
	resolve_(Ds, C, Cs, S1, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subsumed_by([], []).
subsumed_by([L1|C1], [L2|C2]) :-
	L2 /\ \(L1) =:= 0,		% L2 =< L1 (bitwise)
	subsumed_by(C1, C2).

% assumes that the goal 'chkTVs(_)' fails.
% (implementation is slightly more complicated otherwise)
resolvents(C1, C2, Rs) :-
	interunion_(C1, C2, Us, Is),
	all_resolvents(Is, [], Us, Rs).

interunion_([], [], [], []).
interunion_([L1|C1], [L2|C2], [U|Us], [I|Is]) :-
	U is L1\/L2,
	I is L1/\L2,
	interunion_(C1, C2, Us, Is).

all_resolvents([],_,_,[]).
all_resolvents([I|Is],UnionBisher,[U|Un],[R|Rs]) :-
        ml_append(UnionBisher, [I|Un], R),
        ml_append(UnionBisher, [U], UnionNachher),
        all_resolvents(Is, UnionNachher, Un, Rs).  
