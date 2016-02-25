/*
 *	nqueens(N, Queens)
 * 	Sterling & Shapiro program 14.2 p210
 *
 *	There is a bug in attack/3.
 */

:- module(nqueens).

:- local select/3.

getbug :- writeln('\nCheck that you are in "module(nqueens).", then'),
	writeln('to start the program type "nqueens(4, Qs).".\n').

bug :- 
	nl,
	explanation.

explanation :-	
writeln(' \n \
 \n \
N-1 should be N+1 in the last clause \n \
Same bug as missing26, but in a simple  \n \
generate and test context. \n \
 \n \
GOAL:	queens(4,Qs). \n \
CORRECT: [3,1,4,2] ? ; \n \
	 [2,4,1,3] ? ; \n \
	 no (more) solutions \n \
BUGGY: 	 no (more) solutions \n \
').

% ============================================================================

nqueens(N, Qs) :-
	range(1, N, Ns),
	permutation(Ns, Qs), 
	safe(Qs).


/*
 *  range(M, N, Ns)
 *  Ns is the list of integers between M and N inclusive
 */
range(M, N, [M|Ns]) :-
	M < N,
	M1 is M + 1,
	range(M1, N, Ns).
range(N,N,[N]).


permutation(Xs, [Z|Zs]) :-
	select(Z, Xs, Ys),
	permutation(Ys, Zs).
permutation([], []).


/*
 *  select(X, HasXs, OnelessXs) <-
 *  The list OneLessXs is the result of removing
 *  one occurence of X from the list HasXs.
 */
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :-
	select(X, Ys, Zs).

/*
 *  safe/1
 */
safe([Q|Qs]) :-
	safe(Qs),
	not attack(Q, Qs).
safe([]).


attack(X, Xs) :-
	attack(X, 1, Xs).

attack(X, N, [Y|_Ys]) :-
	X is Y + N.
attack(X, N, [Y|_Ys]) :-
	X is Y - N.
attack(X, N, [_Y|Ys]) :-
	N1 is N - 1,		% fix: N1 is N + 1
	attack(X, N1, Ys).

