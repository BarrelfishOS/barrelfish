% LOOP48
:- module(pendulum).

bugcollection(loop48).

getbug :-
	writeln('\nCheck that you are in "module(pendulum).", then'),
	writeln('to start the program type "strongly_connected_components."\n.').

bug :- 
	nl,
	explanation.

explanation :-	
writeln(' \n\
my_abolish/2 runs into an endless loop if applied to a non-existent predicate.\n\
\n\
GOAL:	 strongly_connected_components \n\
CORRECT: yes. \n\
BUGGY:   endless loop (pendulum, constructed) \n').


data :- nl,
	writeln('Data are contained in this file.'),
	nl.
envi.


% ============================================================================
/* ---------------------------------------------------------------------
|									|
|	   ANALYSIS OF THE PREDICATE CALL GRAPH FOR DIRECTLY          	|
|		AND INDIRECTLY RECURSIVE PREDICATES			|
|									|
 --------------------------------------------------------------------- */

:- dynamic 
	new/1,
	count/1,
	stack/1,
	dfnumber/2,
	lowlink/2.

my_error_handler(68, dir_recursive__db(X, Y), M) :-
	!,
	generate_directly_recursive, 
	dir_recursive__db(X,Y).
my_error_handler(68, indir_recursive__db(X, Y), M) :-
	!,
	generate_indirectly_recursive, 
	indir_recursive__db(X,Y).
my_error_handler(68, indirect_recursion__db(X), M) :-
	!,
	strongly_connected_components, 
	indirect_recursion__db(X).
my_error_handler(68, Goal, Module) :-
	error(default(68), Goal, Module).	

:- set_error_handler(68, my_error_handler/3).

% number of predicates in the predicate call graph

number_of_predicates(I) :-
	collect_vertices(List),
	length(List,I).

% collect all predicates defined in the db in a list called Vertices,
% where each predicate is represented as (PredName/Arity,FileName)

collect_vertices(Vertices) :-
	setof((PredName/Arity,FileName),
		definition_in__db((PredName,Arity), FileName),
		Vertices).


%  ----------------------------------------------------
%   determination of the directly recursive predicates
%  ----------------------------------------------------

all_direct_recursions(PredList) :-
	setof((PredName/Arity,FN), directly_recursive(PredName/Arity,FN), 
						PredList).

directly_recursive(PredName/Arity,FileName) :-
	dir_recursive__db(PredName/Arity,FileName).
directly_recursive(PredName/Arity) :-
	dir_recursive__db(PredName/Arity,FileName).

generate_directly_recursive :-
	definition_in__db((PredName,Arity),FileName),
	subgoals_of__db((PredName,Arity),Subgoals,FileName),
	member((PredName,Arity),Subgoals),
	assert(dir_recursive__db(PredName/Arity,FileName)),
	fail.
generate_directly_recursive.


%  --------------------------------------------------
%   determination of indirectly recursive predicates
%  --------------------------------------------------

all_indirect_recursions(List) :-
	strongly_connected_components,
	setof(PredList, indirect_recursion__db(PredList), List).

indirectly_recursive(PredName/Arity,FileName) :-
	indir_recursive__db(PredName/Arity,FileName).
indirectly_recursive(PredName/Arity) :-
	indir_recursive__db(PredName/Arity,FileName).

generate_indirectly_recursive :-
	indirect_recursion__db(PredList),
	member((PredName/Arity,FileName),PredList),
	assert(indir_recursive__db(PredName/Arity,FileName)),
	fail.
generate_indirectly_recursive.

%  ---------------------------------------------------------
%   computation of indirect recursions = strongly connected 
%   components (scc) with at least two elements      
%								
%   Implementation of algorithm 5.4. (LOWLINK) given in	
%   AHU, "Design and Analysis of Computer Algorithms", 1974.
%  ---------------------------------------------------------

strongly_connected_components :-
%	collect_vertices(Vertices),
	vertices(Vertices),		% modified in order to avoid 'setof'
	clean_database,
	new_vertices(Vertices),
	assert(count(1)),
	assert(stack([])),
	search_all_vertices(Vertices).

% for each vertex assert new(Vertex)

new_vertices([]).
new_vertices([Vertex|Vs]) :-
	assert(new(Vertex)),
	new_vertices(Vs).

% search(Vertices)

search_all_vertices([]).
search_all_vertices([NewVertex|NewVs]) :-
	search_vertex(NewVertex),
	search_all_vertices(NewVs).
	
search_vertex(Vertex) :-
	retract(new(Vertex)),
	!,
	count(N),
	assert(dfnumber(Vertex,N)),
	assert(lowlink(Vertex,N)),
	increment_counter,
	push(Vertex),
	Vertex = (P/A,F),
	subgoals_of__db((P,A),Sons,F),
	search_sons(Sons, Vertex),
	process_scc(Vertex).
search_vertex(Vertex).

process_scc(Vertex) :-
	lowlink(Vertex,N),
	dfnumber(Vertex,N),
	!,
	pop_scc_from_stack(Vertex,SCC),
	length(SCC,Length),
	Length > 1,
	assert(indirect_recursion__db(SCC)).
process_scc(Vertex).
	
pop_scc_from_stack(Vertex,[Vertex]) :-
	pop(Vertex),
	!.
pop_scc_from_stack(Vertex,[V|Vs]) :-
	pop(V),
	pop_scc_from_stack(Vertex,Vs).	

% for-loop of procedure LOWLINK
% ----
% Note that sons don't contain file names!!

search_sons([], Father) :-
	!.
search_sons([(PN,A)|Vs], Father) :-
	defined((PN,A),Vertex),
	process_vertex(Vertex,Father),
	fail.
search_sons([Vertex|Vs], Father) :-
	search_sons(Vs, Father).

defined((PN,A),(PN/A,FileName)) :-
	definition_in__db((PN,A),FileName).
	
process_vertex(Vertex,Father) :-
	new(Vertex),
	search_vertex(Vertex),
	retract(lowlink(Father,LLFather)),
	lowlink(Vertex,LLVertex),
	min(LLFather,LLVertex,MinLink),
	assert(lowlink(Father,MinLink)),
	!.
process_vertex(Vertex,Father) :-
	dfnumber(Vertex,DFVertex),
	dfnumber(Father,DFFather),
	DFVertex < DFFather,
	on_stack(Vertex),
	retract(lowlink(Father,LLFather)),
	min(DFVertex,LLFather,MinLink),
	assert(lowlink(Father,MinLink)),
	!.
process_vertex(Vertex,Father).
		

%   -------------------------------
%   b a s i c   p r e d i c a t e s
%   -------------------------------

conc([],L,L).
conc([X|Xs],Y,[X|L]) :-
	conc(Xs,Y,L).

increment_counter :-
	retract(count(N)),
	NewN is N + 1,
	assert(count(NewN)).

member(X,[X|Xs]).
member(X,[Y|Ys]) :-
	member(X,Ys).

% remove all the global variables ...

clean_database :-
	my_abolish(lowlink, 2),
	my_abolish(dfnumber, 2),
	my_abolish(new, 1),
	my_abolish(stack, 1).

my_abolish(F, N) :-
	functor(Head, F, N),
	repeat,
		retract((Head :- Body)),
		no_more_clauses(F, N).

no_more_clauses(F, N) :-
	functor(Head, F, N),
	clause(Head, Body),
	!,
	fail.
no_more_clauses(F, N).

% stack handling procedures

push(NewElement) :-
	retract(stack(List)),
	assert(stack([NewElement|List])),
	!.

pop(Element) :-
	retract(stack([Element|List])),
	assert(stack(List)),
	!.

on_stack(Element) :-
	stack(List),
	member(Element,List).


% ---------------------------------------


% data needed for the test

definition_in__db((test, 0), 'loop14.pl').
definition_in__db((traperror, 3), 'loop14.pl').
definition_in__db((number_of_predicates, 1), 'loop14.pl').
definition_in__db((collect_vertices, 1), 'loop14.pl').
definition_in__db((all_direct_recursions, 1), 'loop14.pl').
definition_in__db((directly_recursive, 2), 'loop14.pl').
definition_in__db((directly_recursive, 1), 'loop14.pl').
definition_in__db((generate_directly_recursive, 0), 'loop14.pl').
definition_in__db((all_indirect_recursions, 1), 'loop14.pl').
definition_in__db((indirectly_recursive, 2), 'loop14.pl').
definition_in__db((indirectly_recursive, 1), 'loop14.pl').
definition_in__db((generate_indirectly_recursive, 0), 'loop14.pl').
definition_in__db((strongly_connected_components, 0), 'loop14.pl').
definition_in__db((new_vertices, 1), 'loop14.pl').
definition_in__db((search_all_vertices, 1), 'loop14.pl').
definition_in__db((search_vertex, 1), 'loop14.pl').
definition_in__db((process_scc, 1), 'loop14.pl').
definition_in__db((pop_scc_from_stack, 2), 'loop14.pl').
definition_in__db((search_sons, 2), 'loop14.pl').
definition_in__db((defined, 2), 'loop14.pl').
definition_in__db((process_vertex, 2), 'loop14.pl').
definition_in__db((conc, 3), 'loop14.pl').
definition_in__db((increment_counter, 0), 'loop14.pl').
definition_in__db((member, 2), 'loop14.pl').
definition_in__db((min, 3), 'loop14.pl').
definition_in__db((restore_program, 0), 'loop14.pl').
definition_in__db((push, 1), 'loop14.pl').
definition_in__db((pop, 1), 'loop14.pl').
definition_in__db((on_stack, 1), 'loop14.pl').

subgoals_of__db((test, 0), [(strongly_connected_components, 0)], 'loop14.pl').
subgoals_of__db((traperror, 3), [], 'loop14.pl').
subgoals_of__db((number_of_predicates, 1), [(collect_vertices, 1)], 'loop14.pl').
subgoals_of__db((collect_vertices, 1), [(setof, 3)], 'loop14.pl').
subgoals_of__db((all_direct_recursions, 1), [(setof, 3)], 'loop14.pl').
subgoals_of__db((directly_recursive, 2), [(dir_recursive__db, 2)], 'loop14.pl').
subgoals_of__db((directly_recursive, 1), [(dir_recursive__db, 2)], 'loop14.pl').
subgoals_of__db((generate_directly_recursive, 0), [(definition_in__db, 2), (subgoals_of__db, 3), (member, 2)], 'loop14.pl').
subgoals_of__db((all_indirect_recursions, 1), [(strongly_connected_components, 0), (setof, 3)], 'loop14.pl').
subgoals_of__db((indirectly_recursive, 2), [(indir_recursive__db, 2)], 'loop14.pl').
subgoals_of__db((indirectly_recursive, 1), [(indir_recursive__db, 2)], 'loop14.pl').
subgoals_of__db((generate_indirectly_recursive, 0), [(indirect_recursion__db, 1), (member, 2)], 'loop14.pl').
subgoals_of__db((strongly_connected_components, 0), [(vertices, 1), (new_vertices, 1), (search_all_vertices, 1)], 'loop14.pl').
subgoals_of__db((new_vertices, 1), [(new_vertices, 1)], 'loop14.pl').
subgoals_of__db((search_all_vertices, 1), [(search_vertex, 1), (search_all_vertices, 1)], 'loop14.pl').
subgoals_of__db((search_vertex, 1), [(count, 1), (increment_counter, 0), (push, 1), (subgoals_of__db, 3), (search_sons, 2), (process_scc, 1)], 'loop14.pl').
subgoals_of__db((process_scc, 1), [(lowlink, 2), (dfnumber, 2), (pop_scc_from_stack, 2)], 'loop14.pl').
subgoals_of__db((pop_scc_from_stack, 2), [(pop_scc_from_stack, 2), (pop, 1)], 'loop14.pl').
subgoals_of__db((search_sons, 2), [(search_sons, 2), (defined, 2), (process_vertex, 2)], 'loop14.pl').
subgoals_of__db((defined, 2), [(definition_in__db, 2)], 'loop14.pl').
subgoals_of__db((process_vertex, 2), [(dfnumber, 2), (dfnumber, 2), (on_stack, 1), (new, 1), (search_vertex, 1), (lowlink, 2), (min, 3)], 'loop14.pl').
subgoals_of__db((conc, 3), [(conc, 3)], 'loop14.pl').
subgoals_of__db((increment_counter, 0), [], 'loop14.pl').
subgoals_of__db((member, 2), [(member, 2)], 'loop14.pl').
subgoals_of__db((min, 3), [], 'loop14.pl').
subgoals_of__db((restore_program, 0), [], 'loop14.pl').
subgoals_of__db((push, 1), [], 'loop14.pl').
subgoals_of__db((pop, 1), [], 'loop14.pl').
subgoals_of__db((on_stack, 1), [(stack, 1), (member, 2)], 'loop14.pl').


vertices([(process_scc / 1, 'loop48.pl'), (pop_scc_from_stack / 2, 'loop48.pl'), (search_sons / 2, 'loop48.pl'), (defined / 2, 'loop48.pl'), (process_vertex / 2, 'loop48.pl'), (conc / 3, 'loop48.pl'), (increment_counter / 0, 'loop48.pl'), (member / 2, 'loop48.pl'), (min / 3, 'loop48.pl'), (push / 1, 'loop48.pl'), (pop / 1, 'loop48.pl'), (on_stack / 1, 'loop48.pl')]).
%%%% vertices([(number_of_predicates / 1, 'loop48.pl'), (collect_vertices / 1, 'loop48.pl'), (all_direct_recursions / 1, 'loop48.pl'), (directly_recursive / 2, 'loop48.pl'), (directly_recursive / 1, 'loop48.pl'), (generate_directly_recursive / 0, 'loop48.pl'), (all_indirect_recursions / 1, 'loop48.pl'), (indirectly_recursive / 2, 'loop48.pl'), (indirectly_recursive / 1, 'loop48.pl'), (generate_indirectly_recursive / 0, 'loop48.pl'), (strongly_connected_components / 0, 'loop48.pl'), (new_vertices / 1, 'loop48.pl'), (search_all_vertices / 1, 'loop48.pl'), (search_vertex / 1, 'loop48.pl'), (process_scc / 1, 'loop48.pl'), (pop_scc_from_stack / 2, 'loop48.pl'), (search_sons / 2, 'loop48.pl'), (defined / 2, 'loop48.pl'), (process_vertex / 2, 'loop48.pl'), (conc / 3, 'loop48.pl'), (increment_counter / 0, 'loop48.pl'), (member / 2, 'loop48.pl'), (min / 3, 'loop48.pl'), (push / 1, 'loop48.pl'), (pop / 1, 'loop48.pl'), (on_stack / 1, 'loop48.pl')]).







