%   File   : ORDER.PL
%   Author : R.A.O'Keefe
%   Updated: 12 June 1984, conv to K Johnson, NIP 11-8-87
%   Purpose: Define the "ordered" predicates.

%   Ported to SEPIA by Joachim Schimpf, ECRC, 14.2.91
%   - len/2 removed since we have length/2
%   - ordered/2 is a tool

:- module(order).			% SEPIA header
:- export
%	len/2,
	ordered/1,
	ordered/2.

:- lib(apply).

:- mode
	ordered(+),
	    ordered_(+, +),
	ordered(+, +),
	ordered(+, +, +),
	    ordered_(+, +, +, +).

%   ordered(List)
%   is true when List is a list of terms [T1,T2,...,Tn] such that
%   for all k in 2..n Tk-1 @=< Tk, i.e. T1 @=< T2 @=< T3 ...
%   The output of keysort/2 is always ordered, and so is that of
%   sort/2.  Beware: just because a list is ordered does not mean
%   that it is the representation of an ordered set; it might contain
%   duplicates.  E.g. L = [1,2,2,3] & sort(L,M) => ordered(L) & M\=L.

ordered([]).
ordered([Head|Tail]) :-
	ordered_(Tail, Head).

ordered_([], _).
ordered_([Head|Tail], Left) :-
	Left @=< Head,
	ordered_(Tail, Head).



%   ordered(P, [T1,T2,...,Tn]) means P(T1,T2) & P(T2,T3) & ...
%   i.e. that the second argument is ordered if you regard the
%   first argument as =<.  This is good for generating prefixes
%   of sequences, e.g. L = [1,_,_,_,_] & ordered(times(2),L) yields
%   L = [1,2,4,8,16].

:- tool(ordered/2, ordered/3).

ordered(_, [], _).
ordered(Relation, [Head|Tail], Module) :-
	ordered_(Tail, Head, Relation, Module).

ordered_([], _, _, _).
ordered_([Head|Tail], Left, Relation, Module) :-
	apply(Relation, [Left,Head])@Module,
	ordered_(Tail, Head, Relation, Module).



