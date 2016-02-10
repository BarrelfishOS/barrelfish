
/*
 *  some basic procedures
 */

:- module(master_basic).

:- export delete/3,
	select/3,
	append/3,
	reverse/2,
	member/2,
	length/2.


delete([X|Xs], X, Zs) :-
	!,
	delete(Ys, X, Zs).
delete([Y|Ys], X, [Y|Zs]) :-
	!,
	delete(Ys, X, Zs).
delete([], X, []).


select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :-
	select(X, Ys, Zs).


append([], X).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).


reverse([], []).
reverse([X|Xs], Zs) :-
	reverse(Xs, Ys),
	append(Ys, [X], Zs).


member(X, [X|Xs]).
member(X, [Y|Ys]) :-
	member(X, Ys).


length([], 0).
length([X|Xs], N) :-
	length(Xs, N0),
	N is N0 + 1.


