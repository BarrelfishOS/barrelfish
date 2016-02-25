
/*
 *   some tools used for games
 */

:- module(master_tools).

:- export selects/2,
	exact_matches/3,
	common_members/3.

:- import 
	select/3,
	member/2
   from master_basic.


selects([X|Xs], Ys) :-
	select(X, Ys, Ys1),
	selects(Xs, Ys1).
selects([], Ys).


exact_matches(Xs, Ys, N) :-
	exact_matches(Xs, Ys, 0, N).

exact_matches([X|Xs], [X|Ys], K, N) :-
	K1 is K + 1,
	exact_matches(Xs, Ys, K1, N).
exact_matches([X|Xs], [Y|Ys], K, N) :-
	X =\= Y,
	exact_matches(Xs, Ys, K, N).
exact_matches([], [], N, N).


common_members(Xs, Ys, N) :-
	common_members(Xs, Ys, 0, N).

common_members([X|Xs], Ys, K, N) :-
	member(X, Ys),
	!,
	K1 is K + 1,
	common_members(Xs, Ys, K1, N).
common_members([X|Xs], Ys, K, N) :-
	common_members(Xs, Ys, K, N).
common_members([], Ys, N, N).

