%%% Filename : ml_conf.pl
%%% Date     : 3.3.1998
%%% Contents : Configuration file
%%% Author   : Gernot Salzer
%%% Interface: max_length(N, L).     L is the maximal length of name type N.
%%%            version_number/1, operating_system/1, prolog_system/1
%%%            eol.                  End of line marker for DCGs.

% Length limits

max_length(logname,40).
max_length(ordname,10).
max_length(opname,10).
max_length(quname,10).
max_length(truthvalue,10).
max_length(ordspec,300).
max_length(errcontext_before, 30). % Has to be greater than 3
max_length(errcontext_after,  30). % Has to be greater than 3

version_number('@ml_version@').
version_date('@ml_date@').


% Below you have to choose an operating system (DOS/Mac/Unix)
% and a Prolog version (BinProlog/Sicstus/SWI).
% Choosing means:
% Make sure that the line containing the name of the preferred
% OS/Prolog starts with "%/*****", and that all alternatives
% start with "/*****".


/************************* DOS **************************

operating_system('DOS').

% End of line marker for DCGs
eol --> [13,10].

%********************************************************/



/********************** Apple/Mac ***********************

operating_system('Mac').

% End of line marker for DCGs
eol --> [13].

%********************************************************/



%/************************* Unix ************************

operating_system('Unix').

% End of line marker for DCGs
eol --> [10].

%********************************************************/



/********************** BinProlog ***********************

prolog_system('BinProlog 5.25').

% To create "ml.bp" or "ilc.bp" (=compiled versions of
% MUltlog or the iLC-Converter), do the following:
% 1. Uncomment the two lines below containing :-
% 2. In BinProlog, say 'make(ml).' or 'make(ilc).'
% NOTE: environment variable BP_PATH must point to BinProlog's
%       "src" directory, and either there are already *.wam
%       files, or "src" must be writeable for you.

% To use MUltlog in interpreter mode, make sure that the two
% lines below start with a comment sign. Then just say
% '[ml].'. A bunch of warnings will appear, which is normal.

%:- [wam].
%main :- read(Command), Command.

%********************************************************/


/*********************** SICStus ************************

prolog_system('SICStus 3.0 #5').

%********************************************************/


/************************* SWI **************************

prolog_system('SWI Prolog 2.9.6').

%********************************************************/

%/************************* Generic **************************

prolog_system('Standard Prolog').

%********************************************************/
