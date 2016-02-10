%   File   : PROJEC.PL
%   Author : R.A.O'Keefe
%   Updated: 14 August 1984
%   Purpose: Select Kth argument of each element of a list

:- module(project).			% SEPIA header
:- export
	keys_and_values/3,		%   KeyValList -> KeyList x ValList
	project/3.			%   TermList x ArgNo -> ArgList

:- mode
	keys_and_values(?, ?, ?),
	project(+, +, ?),
	    '$project'(+, +, ?),
	    '$project'(+, ?).


%   keys_and_values({K1-V1,...,Kn-Vn], [K1,...,Kn], [V1,...,Vn])
%   is true when its arguments look like the picture above.  It is meant
%   for splitting a list of Key-Value pairs (such as keysort/2 wants and
%   produces) into separate lists of Keys and of Values.  It may just as
%   well be used for building a list of pairs from a pair of lists.   In
%   fact one usually wants just the keys or just the values, but you can
%   supply _ as the other argument.   For example, suppose you wanted to
%   sort a list without having duplicates removed.  You could do
%	keys_and_values(RawPairs, RawKeys, _),
%	keysort(RawPairs, OrdPairs),
%	keys_and_values(OrdPairs, OrdKeys, _).
%   (In fact this operation is msort/2 and should be available somewhere.)

keys_and_values([], [], []) :- !.
keys_and_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
	keys_and_values(Pairs, Keys, Values).



%   This problem has cropped up in several programs.  You have a
%   list of structures s(A1,...,Ak,...An), which you can guarantee
%   are all the same, and you want a list with just the Ak in the
%   same order.  The best thing to do is to write
/*  type s(T1,...,Tn) --> s(T1,...,Tn).
    pred s_project(list(s(T1,...,Tn)), list(T1), ..., list(Tn)).
    mode s_project(+, -, ..., -).

s_project([s(A1,...,An)|Ss], [A1|A1s], ..., [An|Ans]) :-
	s_project(Ss, A1s, ..., Ans).
s_project([], [], ..., []).
*/
%   This is second best.  Doing it this way does mean that you can
%   extract just the argument you want, and that you only have to
%   remember one predicate, but it also means that it can't be
%   type-checked.  project(Structs, K, Args) unifies Args with the
%   list of Kth arguments of the Structs.  K = 0 means the principal
%   function symbol, which is occasionally useful.
%   This could be defined as maplist(arg(K), TermList, ArgList).


project(Structs, 0, Functors) :- !,
	'$project'(Structs, Functors).
project(Structs, K, Args) :-
	integer(K), K > 0,
	'$project'(Structs, K, Args).


'$project'([], []).
'$project'([Struct|Structs], [Functor|Functors]) :-
	functor(Struct, Functor, _),
	'$project'(Structs, Functors).


'$project'([], _, []).
'$project'([Struct|Structs], K, [Arg|Args]) :-
	arg(K, Struct, Arg),
	'$project'(Structs, K, Args).

