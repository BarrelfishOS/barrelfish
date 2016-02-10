% LOOP27

:- module(digin).

getbug :-	
	writeln('\nCheck that you are in "module(digin).", then'),
	writeln('to start the program type "test_show(D).".\n').


bug :- 
	nl,
	explanation.

explanation :-	
writeln(' \n\
Typing error: in the last subgoal of dolevels/3, it should be "Nextlevel" \n\
instead of "Nextlevels". \n\
In traverse/4, it should be "==" instead of "=". \n\
\n\
GOAL:	 test_show(D) \n\
CORRECT: all the possibilities to build a sorted binary tree \n\
	 are shown on backtracking. \n\
BUGGY:   endless loop (direct recursion, exact pattern). \n').


% ============================================================================
% vertical display of a tree.
% 1) solution of the book and some tests.

show(Tree) :- 
	dolevels(Tree, 0, more).

dolevels(Tree, Level, alldone) :-!.
dolevels(Tree, Level, more) :-
	traverse(Tree, Level, 0, Continue),
	nl,
	Nextlevel is Level+1,
	dolevels(Tree, Nextlevels, Continue).

traverse(nil, _, _, _).
traverse(t(Left, X, Right), Level, Xdepth, Continue) :-
	NextDepth is Xdepth+1,
	traverse(Left, Level, NextDepth, Continue),
	(Level = Xdepth,
		!,
		write(X),
		Continue=more
	;
		write(' ')
	),
	traverse(Right, Level, NextDepth, Continue).


% to construct a tree

add(D, X, D1) :- 
	addroot(D, X, D1).
add(t(L,Y,R), X, t(L1,Y,R)) :-
	gt(Y, X),
	add(L, X, L1).
add(t(L,Y,R), X, t(L,Y,R1)) :-
	gt(X, Y),
	add(R, X, R1).

addroot(nil, X, t(nil,X,nil)).
addroot(t(L,Y,R), X, t(L1,X, t(L2,Y,R))) :-
	gt(Y, X),
	addroot(L, X, t(L1,X,L2)).
addroot(t(L,Y,R), X, t(t(L,Y,R1), X,R2)) :-
	gt(X, Y),
	addroot(R, X, t(R1,X,R2)).

gt(X,Y) :- 
	name(X, [A]),
	name(Y, [B]),
	A > B.

test_show(D4) :-
	add(nil, a, D1),
	add(D1, b, D2),
	add(D2, c, D3),
	add(D3, d, D4),
	show(D4).
	
