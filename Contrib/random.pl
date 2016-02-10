%   File   : RANDOM.PL
%   Author : R.A.O'Keefe
%   Updated: 1 October 1984
%   NIP version: 13 May 1987
%   Adapted for ECLiPSe 1994
%   Purpose: Random number generator.

:- module(random).

:- export random/2, randomise/0, randomise/1, random/3, rand_perm/2.
:- ensure_loaded(library(listut)).
:- import nth0/4 from listut.

:- pragma(nodebug).

:- local variable(seed, [3172, 9814, 20125]).

%   given an integer N >= 1, random(N, I) unifies I with a random
%   integer between 0 and N - 1.

random(N, I) :-
	getval(seed, [A0,A1,A2]),
	B0 is (A0*171) mod 30269,
	B1 is (A1*172) mod 30307,
	B2 is (A2*170) mod 30323,
	setval(seed,[B0,B1,B2]),
	R is A0/30269 + A1/30307 + A2/30323,
	I is fix((R - fix(R)) * N).

%	The next bit: K R Johnson, 13-5-87
%	Restart the random sequence from the beginning

randomise :-
	setval(seed, [3172, 9814, 20125]).

%	Instantiate the seeds to your own favourite value

randomise(Seed) :-
	integer(Seed),
	Seed > 0,
	S0 is Seed mod 30269,
	S1 is Seed mod 30307,
	S2 is Seed mod 30323,
	setval(seed, [S0,S1,S2]).

%   given an non-empty List, random(List, Elem, Rest) unifies Elem with
%   a random element of List and Rest with the other elements.

random(List, Elem, Rest) :-
	length(List, N),
	N > 0,
	random(N, I),
	nth0(I, List, Elem, Rest).


%   rand_perm(List, Perm) unifies Perm with a random permutation
%   of List.  What good this may be I'm not sure, and there's bound
%   to be a more efficient way of doing it.  Oh well.

rand_perm([], []).
rand_perm([H1|T1], [H2|T2]) :-
	random([H1|T1], H2, T3),
	rand_perm(T3, T2).

