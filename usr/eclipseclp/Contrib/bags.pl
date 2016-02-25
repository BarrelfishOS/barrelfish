
%   Ported to SEPIA by Joachim Schimpf, ECRC, 14.2.91
%   - checkbag/2 and mapbag/3 are tools

:- module(bags).			% SEPIA header
:- export
	bag_inter/3,
	bag_to_list/2,
	bag_to_set/2,
	bag_union/3,
	bagmax/2,
	bagmin/2,
	checkbag/2,
	is_bag/1,
	length/3,
	list_to_bag/2,
	make_sub_bag/2,
	mapbag/3,
	member/3,
	portray_bag/1,
	test_sub_bag/2.

:- lib(apply).


%   File   : BAGUTL.PL
%   Author : R.A.O'Keefe
%   Updated: 18 February 1984
%   Purpose: Bag Utilities
/*
    A bag B is a function from a set dom(B) to the non-negative integers.
For the purposes of this module, a bag is constructed from two functions:
	
	bag		- creates an empty bag
	bag(E, M, B)	- extends the bag B with a new (NB!) element E
			  which occurs with multiplicity M, and which
			  precedes all elements of B in Prolog's order.

For instance the bag with an a and two bs in it is represented by the
term
	bag(a,1,bag(b,2,bag)).

A bag is represented by a Prolog term mirroring its construction.  There
is one snag with this: what are we to make of
	bag(f(a,Y), 1, bag(f(X,b), 1, bag))	?
As a term it has two distinct elements, but f(a,b) will be reported as
occurring in it twice.  But according to the definition above,
	bag(f(a,b), 1, bag(f(a,b), 1, bag))
is not the representation of any bag, that bag is represented by
	bag(f(a,b), 2, bag)
alone.  We are apparently stuck with a scheme which is only guaranteed
to work for "sufficiently instantiated" terms, but then, that's true of 
a lot of Prolog code.

    The reason for insisting on the order is to make union and 
intersection linear in the sizes of their arguments.

*/

% Defined in this file
%	bag_inter/3,
%	bag_to_list/2,
%	bag_to_set/2,
%	bag_union/3,
%	bagmax/2,
%	bagmin/2,
%	checkbag/2,
%	is_bag/1,
%	length/3,
%	list_to_bag/2,
%	make_sub_bag/2,
%	mapbag/3,
%	member/3,
%	member/3,
%	portray_bag/1,
%	test_sub_bag/2.
%

is_bag(bag).
is_bag(bag(E,M,B)) :-
	integer(M), M > 0,
	is_bag(B, E).

	is_bag(bag, _).
	is_bag(bag(E,M,B), P) :-
		E @> P,
		integer(M), M > 0,
		is_bag(B, E).



portray_bag(bag(E,M,B)) :-
	write('[% '), portray_bag(E, M, B), write(' %]').
portray_bag(bag) :-
	write('[% '), write(' %]').

	portray_bag(E, M, B) :-
		var(B), !,
		portray_bag(E, M), write(' | '), write(B).
	portray_bag(E, M, bag(F,N,B)) :- !,
		portray_bag(E, M), write(', '),
		portray_bag(F, N, B).
	portray_bag(E, M, bag) :- !,
		portray_bag(E, M).
	portray_bag(E, M, B) :-
		portray_bag(E, M), write(' | '), write(B).

		portray_bag(E, M) :-
			print(E), write(':'), write(M).


%   If bags are to be as useful as lists, we should provide mapping
%   predicates similar to those for lists.  Hence
%	checkbag(Pred, Bag)		- applies Pred(Element, Count)
%	mapbag(Pred, BagIn, BagOut)	- applies Pred(Element, Answer)
%   Note that mapbag does NOT give the Count to Pred, but preserves it.
%   It wouldn't be hard to apply Pred to four arguments if it wants them.


:- tool(checkbag/2, checkbag/3).

checkbag(_, bag, _).
checkbag(Pred, bag(E,M,B), Module) :-
	apply(Pred, [E, M])@Module,
	checkbag(Pred, B, Module).


:- tool(mapbag/3, mapbag/4).

mapbag(Pred, BagIn, BagOut, Module) :-
	mapbaglist(Pred, BagIn, Listed, Module),
	keysort(Listed, Sorted),
	bagform(Sorted, BagOut).

	mapbaglist(_, bag, [], _).
	mapbaglist(Pred, bag(E,M,B), [R-M|L], Module) :-
		apply(Pred, [E, R])@Module,
		mapbaglist(Pred, B, L, Module).



bag_to_list(bag, []).
bag_to_list(bag(E,M,B), R) :-
	bag_to_list(M, E, L, R),
	bag_to_list(B, L).

	bag_to_list(0, _, L, L) :- !.
	bag_to_list(M, E, L, [E|R]) :-
		N is M-1,
		bag_to_list(N, E, L, R).



list_to_bag(L, B) :-
	addkeys(L, K),
	keysort(K, S),
	bagform(S, B).

	addkeys([], []).
	addkeys([Head|Tail], [Head-1|Rest]) :-
		addkeys(Tail, Rest).

	bagform([], bag) :- !.
	bagform(List, bag(E,M,B)) :-
		bagform(E, List, Rest, 0, M), !,
		bagform(Rest, B).

		bagform(Head, [Head-N|Tail], Rest, K, M) :-!,
			L is K+N,
			bagform(Head, Tail, Rest, L, M).
		bagform(_, Rest, Rest, M, M).



bag_to_set(bag, []).
bag_to_set(bag(E,_,B), [E|S]) :-
	bag_to_set(B, S).


/*  There are two versions of the routines member, bagmax, and bagmin.
    The slow versions, which are commented out, try to allow for the
    possibility that distinct elements in the bag might unify, while
    the faster routines assume that all elements are ground terms.


member(E, M, bag(E,K,B)) :-
	member(B, E, K, M).
member(E, M, bag(_,_,B)) :-
	member(E, M, B).

	member(bag(E,L,B), E, K, M) :- !,
		N is K+L,
		member(B, E, N, M).
	member(bag(_,_,B), E, K, M) :-
		member(B, E, K, M).
	member(bag,	   E, M, M).

%  These routines are correct, but Oh, so costly!

bagmax(B, E) :-
	member(E, M, B),
	\+ (member(F, N, B), N > M).

bagmin(B, E) :-
	member(E, M, B),
	\+ (member(F, N, B), N < M).

*//*	The faster versions follow    */


member(Element, Multiplicity, bag(Element,Multiplicity,_)).
member(Element, Multiplicity, bag(_,_,Bag)) :-
	member(Element, Multiplicity, Bag).


memberchk(Element, Multiplicity, bag(Element,Multiplicity,_)) :- !.
memberchk(Element, Multiplicity, bag(_,_,Bag)) :-
	memberchk(Element, Multiplicity, Bag).



bagmax(bag(E,M,B), Emax) :-
	bag_scan(B, E, M, Emax, >).

bagmin(bag(E,M,B), Emin) :-
	bag_scan(B, E, M, Emin, <).

	bag_scan(bag(Eb,Mb,B), _, Mi, Eo, C) :-
		compare(C, Mb, Mi), !,
		bag_scan(B, Eb, Mb, Eo, C).
	bag_scan(bag(_,_,B), Ei, Mi, Eo, C) :-
		bag_scan(B, Ei, Mi, Eo, C).
/*	bag_scan(bag(Eb,Mb,B), Ei, Mi, Eo, C) :-
		bag_scan(B, Eb, Mb, Eo, C).	%  for all extrema
*/	bag_scan(bag,	       Ei, _, Ei, _).




length(B, BL, SL) :-
	length(B, 0, BL, 0, SL).

	length(bag,	   BL, BL, SL, SL).
	length(bag(_,M,B), BA, BL, SA, SL) :-
		BB is BA+M, SB is SA+1,
		length(B, BB, BL, SB, SL).


%  sub_bag, if it existed, could be used two ways: to test whether one bag
%  is a sub_bag of another, or to generate all the sub_bags.  The two uses
%  need different implementations.


make_sub_bag(bag, bag).
make_sub_bag(bag(E,M,B), bag(E,N,C)) :-
	countdown(M, N),
	make_sub_bag(B, C).
make_sub_bag(bag(_,_,B), C) :-
	make_sub_bag(B, C).

	countdown(M, M).
	countdown(M, N) :-
		M > 1, K is M-1,
		countdown(K, N).



test_sub_bag(bag, _).
test_sub_bag(bag(E1,M1,B1), bag(E2,M2,B2)) :-
	compare(C, E1, E2),
	test_sub_bag(C, E1, M1, B1, E2, M2, B2).

	test_sub_bag(>, E1, M1, B1, _, _, B2) :-
		test_sub_bag(bag(E1, M1, B1), B2).
	test_sub_bag(=, E1, M1, B1, E1, M2, B2) :-
		M1 =< M2,
		test_sub_bag(B1, B2).


bag_union(bag(E1,M1,B1), bag(E2,M2,B2), B3) :-
	compare(C, E1, E2), !,
	bag_union(C, E1, M1, B1, E2, M2, B2, B3).
bag_union(bag, Bag, Bag) :- !.
bag_union(Bag, bag, Bag).

	bag_union(<, E1, M1, B1, E2, M2, B2, bag(E1,M1,B3)) :-
		bag_union(B1, bag(E2, M2, B2), B3).
	bag_union(>, E1, M1, B1, E2, M2, B2, bag(E2,M2,B3)) :-
		bag_union(bag(E1, M1, B1), B2, B3).
	bag_union(=, E1, M1, B1, E1, M2, B2, bag(E1,M3,B3)) :-
		M3 is M1+M2,
		bag_union(B1, B2, B3).



bag_inter(bag(E1,M1,B1), bag(E2,M2,B2), B3) :-
	compare(C, E1, E2), !,
	bag_inter(C, E1, M1, B1, E2, M2, B2, B3).
bag_inter(_, _, bag).

	bag_inter(<, _, _, B1, E2, M2, B2, B3) :-
		bag_inter(B1, bag(E2,M2,B2), B3).
	bag_inter(>, E1, M1, B1, _, _, B2, B3) :-
		bag_inter(bag(E1,M1,B1), B2, B3).
	bag_inter(=, E1, M1, B1, E1, M2, B2, bag(E1, M3, B3)) :-
		(   M1 < M2, M3 = M1  ;  M3 = M2   ), !,
		bag_inter(B1, B2, B3).


