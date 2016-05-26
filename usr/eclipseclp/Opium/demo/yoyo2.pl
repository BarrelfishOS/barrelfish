% LOOP49
:- module(yoyo2).

getbug :-	
	writeln('\nCheck that you are in "module(yoyo2).", then'),
	writeln('to start the program type "mcs([arc(1,2,0), arc(1,3,5), arc(2,3,0)], Tree)."\n').

bug :- 
	nl,
	explanation.

explanation :-	
writeln(' \n\
The problem is that subset/2 is used in a generating way \n\
but this is not possible. \n\
\n\
GOAL:	 mcs([arc(1,2,0), arc(1,3,5), arc(2,3,0)], Tree). \n\
CORRECT: Tree = [arc(1,2,0), arc(2,3,0)] \n\
BUGGY:   endless loop (failure-driven endless recursion). \n').


% ============================================================================
/*
 p. 230  ex. 9.15

 Finding a minimum-cost spanning tree of a graph.

 This program is only a variation of the program for finding 
 an arbitrary spanning tree in a graph, given on p. 231.
 In addition I changed the representation of the graph. 
*/

% mcs : minimum-cost spanning tree

mcs(Graph,Tree) :-
	spanning_tree(Graph,Tree),
	not (spanning_tree(Graph,Othertree),
	     cheaper(Othertree,Tree)). 
      
spanning_tree(Graph,Tree) :-
	subset(Tree, Graph),
	tree(Tree),
	covers(Tree,Graph).

tree(Graph) :-
	connected(Graph),
	not hasacycle(Graph).
	
connected(Graph) :-
	node_of(Graph, [], Nodes), 
	not (member(A, Nodes),
	     member(B, Nodes),
	     not path_in_graph(A,B,Graph,_)).

node_of([], Set, Set).
node_of([arc(X, Y, _)|G], Accu, Res) :-
	add_to(X, Y, Accu, NewAccu),
	node_of(G, NewAccu, Res).

add_to(X, Y, L, L) :-
	member(X, L),
	member(Y, L),
	!.
add_to(X, Y, L, [X|L]) :-
	member(Y, L),
	!.
add_to(X, Y, L, [Y|L]) :-
	member(X, L),
	!.
add_to(X, Y, L, [X, Y|L]).

hasacycle(Graph) :-
	!,
	fail.
hasacycle(Graph) :-
	node_of(Graph, [], Nodes),
	member(A, Nodes),
	member(B, Nodes),
	adjacent(A,B,Graph),
	(	adjacent(B,A,Graph)
	;
		path_in_graph(B,A,Graph,[X,Y,Z|Rest])
	).

covers(Graph1,Graph2) :- 
	not (node(A,Graph2), 
	     not node(A,Graph1)).

cheaper(Graph1,Graph2) :-
	sumcosts(Graph1,Sum1),
	sumcosts(Graph2,Sum2),
	Sum1 < Sum2. 
 
sumcosts([],0).
sumcosts([X|Rest],Sum) :-
	sumcosts(Rest,Sum1),
	X = arc(P1,P2,Costs),
	Sum is Sum1 + Costs.

subset([], L).
subset([X|Xs], L) :-
	member(X, L),
	subset(Xs, L).

% Representation of the graph with costs attached to the arcs:
% List of objects 'arc(Point1,Point2,Costs)'

node(A,Graph) :-
	member(X,Graph),
	( X = arc(A,_,_)
	;
	X = arc(_,A,_)
	).

adjacent(A,B,Graph) :-
	member(X,Graph),
	X = arc(A,B,_),
	!.

path_in_graph(A,Z,Graph,Path) :-
	path_in_graph1(A,[Z],Graph,Path).

path_in_graph1(A,[A|Path1],_,[A|Path1]) :- 
	!.
path_in_graph1(A,[Y|Path1],Graph,Path) :-
	( adjacent(X,Y,Graph)
	;
	adjacent(Y,X,Graph)
	),
	not member(X,Path1),			% no cycle
	path_in_graph1(A,[X,Y|Path1],Graph,Path).

member(X,[X|Rest]).
member(X,[Y|Rest]) :- 
	member(X,Rest).

