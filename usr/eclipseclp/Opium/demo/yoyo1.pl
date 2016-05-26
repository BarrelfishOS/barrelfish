% LOOP6

:- module(yoyo1).

getbug :-
	writeln('\nCheck that you are in "module(yoyo1).", then'),
	writeln('to start the program type "subset(S, [1,2,3])."\n and ask for more solutions.\n').

bugcollection(loop6).

bug :- 
	nl,
	explanation.

explanation :-	
write(' \n\
Wrong algorithm, therefore subset(S, ground_list) loops on backtracking. \n\
\n\
In predicate subset/2 there is a left-recursion together with a very \n\
general base clause which always succeeds if at least one of the \n\
parameters is non-ground, and this is true for the recursive call. \n'),
write('If subset/2 is called with the first parameter non-ground the first \n\
clause succeeds --> member/2 is called with non-ground parameters --> \n\
S1 is unified with a list containing X --> del/3 can never succeed, \n\
but member/2 will produces infinitely many of such lists on \n\
backtracking. \n'),
writeln(' \n\
GOAL:	 subset(S, [1,2,3]) \n\
CORRECT: S = [1,2,3] ; \n\
	 S = [1,2] ; \n\
	 ... \n\
BUGGY:   S = [1,2,3] ; \n\
	 endless loop \n\
	 (direct recursion, no exact pattern, loop occurs on backtracking).').

 
% ============================================================================
/*
   p.67 ff.  chapter 3.2
   p.76 ff.  exercises 3.3 - 3.6, 3.8 - 3.9, 3.11

   Operations on Lists
*/

empty([]).

member(X,[X|Tail]).
member(X,[Head|Tail]) :- 
	member(X,Tail).

conc([],L,L).
conc([X|L1],L2,[X|L3]) :- 
	conc(L1,L2,L3).

head(X,[X|L]).

del(X,[X|Tail],Tail).
del(X,[Y|Tail],[Y|Tail1]) :-
	del(X,Tail,Tail1).
           
% shift(L1,L2) : first element of L1 will be the last one of L2

subset(L,L).
subset(L,S) :- 
	subset(L,S1),
	member(X,S1),
	del(X,S,S1).    


% flatten(List,FlatList): FlatList is List 'flattened' so that
% the elements of List's sublists are reorganized as one plain list.
% Example: flatten([a,b,[c,d],[],[[[e]]],L) --> L = [a,b,c,d,e]

flatten([[X]],[X]).
flatten([Head|Tail],F) :- 
	flatten(Head,F1),
	flatten(Tail,F2),
	conc(F1,F2,F).
