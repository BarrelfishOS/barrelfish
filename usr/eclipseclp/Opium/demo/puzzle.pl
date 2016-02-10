/*
 *  program copied from Sterling/Shapiro, p. 214 - 216
 */

test(Solution) :-
	test_puzzle(puzzle(Clues, Queries, Solution)),
	solve_puzzle(puzzle(Clues, Queries, Solution)).

/*
 *  puzzle solver
 */
solve_puzzle(puzzle(Clues,Queries,Solution)) :-
	solve(Clues),
	solve(Queries).

solve([Clue|Clues]) :-
	Clue,
	solve(Clues).
solve([]).

/*
 *  description of test puzzle
 */
test_puzzle(puzzle(Clues, Queries, Solution)) :-
	structure(Structure),
	clues(Structure, Clues),
	queries(Structure, Queries, Solution).

structure([friend(N1,C1,S1), friend(N2,C2,S2), friend(N3,C3,S3)]).

clues(Friends,
	[(did_better(M1C1, M2C1, Friends),	% Clue 1
	  name_is(M1C1, michael),
	  sport(M1C1, basketball),
	  nationality(M2C1, american)),
	 (did_better(M1C2, M2C2, Friends),	% Clue 2
	  name_is(M1C2, simon),
	  nationality(M1C2, israeli),
	  sport(M2C2, tennis)),
	 (first(Friends, MC3),			% Clue 3
	  sport(MC3, cricket))
	]).

queries(Friends,
	[member(Q1, Friends),
	 name_is(Q1, Name),
	 nationality(Q1, australian),		% Query 1
	 member(Q2, Friends),
	 name_is(Q2, richard),
	 sport(Q2, Sport)			% Query 2
	],
	[['The Australian is ', Name], ['Richard plays ', Sport]]).

did_better(A, B, [A,B,C]).
did_better(A, C, [A,B,C]).
did_better(B, C, [A,B,C]).

name_is(friend(A,B,C), A).

nationality(friend(A,B,C), B).

sport(friend(A,B,C), C).

first([X|Xs], X).

