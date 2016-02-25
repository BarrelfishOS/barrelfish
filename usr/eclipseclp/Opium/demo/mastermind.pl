
/*
 *	MASTERMIND GAME
 */

:- module(mastermind).

:- lib(master_tools, master_tools).
:- lib(master_basic, master_basic).

:- import 
	selects/2,
	exact_matches/3 
   from master_tools.

:- import
	length/2
   from master_basic.

:- dynamic query/3.


mastermind :-
	cleanup,
	guess(Code),
	check(Code),
	announce.

guess([X1, X2, X3, X4]) :-
	selects([X1, X2, X3, X4], [1,2,3,4,5,6,7,8,9,0]).


/*
 *  verify the proposed guess
 */

check(Guess) :-
	~ inconsistent(Guess),
	ask(Guess).

inconsistent(Guess) :-
	query(OldGuess, Bulls, Cows),
	~ bulls_and_cows_match(OldGeuss, Guess, Bulls, Cows).

buls_and_cows_match(OldGuess, Guess, Bulls, Cows) :-
	exact_matches(Oldguess, Guess, N1),
	Bulls = N1,			% correct number of bulls
	common_members(OldGuess, Guess, N2),
	Cows is N2 - Bulls.		% correct number of cows


/*
 * asking a guess
 */

ask(Guess) :-
	repeat,
		printf("How many bulls and cows in %w ? ", [Guess]),
		read((Bulls, Cows)),
		sensible(Bulls, Cows),
		!,
		assert(query(Guess, Bulls, Cows)),
		Bulls == 4.

sensible(Bulls, Cows) :-
	integer(Bulls),
	integer(Cows),
	Bulls + Cows =< 4.


/*
 *  bookkeeping
 */

cleanup :-
	retract_all(query(_,_,_)).

announce :-
	size_of(X, A^(B^(query(X, A, B))), N),
	printf("Found the answer after %w queries.\n", [N]).

size_of(X, Goal, N) :-
	setof(X, Goal, Instances),
	length(Instances, N).




