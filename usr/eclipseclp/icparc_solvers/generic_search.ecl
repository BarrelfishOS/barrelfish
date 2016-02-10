% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Helmut Simonis, Parc Technologies
%                 Joachim Schimpf and Kish Shen, IC-Parc
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Generic search routine and search utilities for fd/ic problems
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Helmut Simonis, Parc Technologies Ltd
%               Joachim Schimpf, IC-Parc
%               Kish Shen, IC-Parc
% Version:	$Id: generic_search.ecl,v 1.8 2013/03/14 14:12:05 jschimpf Exp $
%
% ----------------------------------------------------------------------

% TO-DO: generise to floats for IC, other solvers (e.g. fd_sets)

:-export
	search/6,
	delete/5,
	indomain/2.


% Declare the daVinci predicates that are called from fd_search.
% Don't load the daVinci library yet because it may not be available.
% It is loaded at runtime, just before daVinci_begin/0 is called.
:- import
	daVinci_begin/0,
	daVinci_end/0,
	daVinci_node/2,
	daVinci_edge/3,
	daVinci_node_attribute/3
    from daVinci.

:-local variable(backtrack).
:-local variable(backtrack_limit).
:-local variable(one_level).
:-local variable(nodes).
:-local variable(node_limit).


/***********************************************************************

top level entry

***********************************************************************/

% search(+List:list,
%        ++Arg:integer,
%	++Select:atom,
%	+Choice:atom,
%	++Method:term,
%	?Option:list of options
%	++Module)
% search/6
% most predicates have a Module argument at the end, in order to pass the 
% caller module name to the meta-call predicates
%
:-tool(search/6,search_body/7).
search_body(Vars,Arg,Select,Choice,Method,Option,Module):-
	collection_to_list(Vars, List),
	integer(Arg),
	callable(Select),
	callable(Choice),
	is_search_method(Method),
	is_list(Option),
	!,
	reset_backtrack_count(Option),
	in_out(Choice,In,Out),
	(option_for_daVinci(Option) ->
	    use_module(library(daVinci)),
	    daVinci_begin % only called for the right option
	;
	    true
	),
	tree_option(Option,Node_option,Root_node,Module),
	% top-level block to handle the limited number of nodes
	block(search1(List,Arg,Select,Choice,Method,In,Out,Node_option,Root_node,Module),nodes,search_nodes_failed),
	(option_for_daVinci(Option) ->
	    daVinci_end % only called for the right option
	;
	    true
	),
	get_backtrack_count(Option).
search_body(Vars,Arg,Select,Choice,Method,Option,Module):-
	error(5, search(Vars,Arg,Select,Choice,Method,Option), Module).

% called when the number of search nodes is exceeded
search_nodes_failed:-
	writeln(search_nodes_exceeded),
	fail.

% branch one the different search methods
search1(L,Arg,Select,Choice,complete,In,Out,Node_option,Root_node,Module):-
	labeling(L,Arg,Select,Choice,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,sbds,In,Out,Node_option,Root_node,Module):-
	sbds(L,Arg,Select,Choice,In,Out,Node_option,Root_node,Module).
search1(L,Arg,Select,Choice,gap_sbds,In,Out,Node_option,Root_node,Module):-
	gap_sbds(L,Arg,Select,Choice,In,Out,Node_option,Root_node,Module).
search1(L,Arg,Select,Choice,gap_sbdd,In,Out,Node_option,Root_node,Module):-
	gap_sbdd(L,Arg,Select,Choice,In,Out,Node_option,Root_node,Module).
search1(L,Arg,Select,Choice,bbs(Steps),In,Out,Node_option,Root_node,Module):-
	bbs(L,Arg,Select,Choice,Steps,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,credit(Credit,Steps),In,Out,Node_option,Root_node,Module):-
	credit(L,Arg,Select,Choice,Credit,Steps,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,lds(Disc),In,Out,Node_option,Root_node,Module):-
	lds(L,Arg,Select,Choice,Disc,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,dbs(Level,Steps),In,Out,Node_option,Root_node,Module):-
	dbs(L,Arg,Select,Choice,Level,Steps,In,Out,Node_option, Root_node,Module).

is_search_method(complete) :- !.
is_search_method(sbds) :- !.
is_search_method(gap_sbds) :- !.
is_search_method(gap_sbdd) :- !.
is_search_method(bbs(N)) :- integer(N), !.
is_search_method(credit(N,M)) :- integer(N), integer(M), !.
is_search_method(credit(N,bbs(M))) :- integer(N), integer(M), !.
is_search_method(credit(N,lds(M))) :- integer(N), integer(M), !.
is_search_method(lds(N)) :- integer(N), !.
is_search_method(dbs(N,M)) :- integer(N), integer(M), !.
is_search_method(dbs(N,bbs(M))) :- integer(N), integer(M), !.
is_search_method(dbs(N,lds(M))) :- integer(N), integer(M), !.


/***********************************************************************

different search methods

***********************************************************************/


% labeling(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode labeling(+,++,++,+,?,?,?,++,++).
labeling(Xs,Arg,Select,Choice,In,Out,Node_option, Node,Module):-
	( search_delete(X,Xs,R,Arg,Select,Module) ->
	    tree_fixed(X,Node_option,Arg,Choice,Node,complete),
	    choose(X,Arg,Choice,In,In1,Module),
	    inc_backtrack_count,
	    tree_node(X,Node_option, Node,Node1,Module),
	    labeling(R,Arg,Select,Choice,In1,Out,Node_option, Node1,Module)
	;
	    Out=In
	).

% sbds(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode sbds(+,++,++,+,?,?,?,++,++).
sbds(L,Arg,Select,Choice,In,Out,Node_option,Node,Module):-
	(
	    sbds_var_list(L,Arg),
	    ( sbds_indomain_choice(Choice) ->
		SBDSChoice = sbds(Choice)
	    ;
		% If it's not an explicitly supported built-in method, it
		% must be a user-supplied predicate (with the assumption
		% that the user has used sbds_try/2 for labelling, as
		% required).
		\+ translate_indomain_atom(Choice, _),
		SBDSChoice = Choice
	    )
	->
	    labeling(L,Arg,Select,SBDSChoice,In,Out,Node_option,Node,Module)
	;
	    error(5, search(L,Arg,Select,Choice,sbds,[]), Module)
	).

    % Check the search list only contains SBDS variables (or non-variables).
sbds_var_list(L,Arg) :-
	(
	    foreach(X,L),
	    param(Arg)
	do
	    access(X,Arg,Var),
	    once((nonvar(Var) ; sbds_module:is_sbds_var(Var)))
	).

% gap_sbds(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode gap_sbds(+,++,++,+,?,?,?,++,++).
gap_sbds(L,Arg,Select,Choice,In,Out,Node_option,Node,Module):-
	(
	    gap_sbds_var_list(L,Arg),
	    ( gap_sbds_indomain_choice(Choice) ->
		SBDSChoice = gap_sbds(Choice)
	    ;
		% If it's not an explicitly supported built-in method, it
		% must be a user-supplied predicate (with the assumption
		% that the user has used sbds_try/2 for labelling, as
		% required).
		\+ translate_indomain_atom(Choice, _),
		SBDSChoice = Choice
	    )
	->
	    labeling(L,Arg,Select,SBDSChoice,In,Out,Node_option,Node,Module)
	;
	    error(5, search(L,Arg,Select,Choice,gap_sbds,[]), Module)
	).

    % Check the search list only contains SBDS variables (or non-variables).
gap_sbds_var_list(L,Arg) :-
	(
	    foreach(X,L),
	    param(Arg)
	do
	    access(X,Arg,Var),
	    once((nonvar(Var) ; ic_gap_sbds:is_sbds_var(Var)))
	).

% gap_sbdd(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode gap_sbdd(+,++,++,+,?,?,?,++,++).
gap_sbdd(L,Arg,Select,Choice,In,Out,Node_option,Node,Module):-
	(
	    gap_sbdd_var_list(L,Arg),
	    ( gap_sbdd_indomain_choice(Choice) ->
		SBDDChoice = gap_sbdd(Choice)
	    ;
		% If it's not an explicitly supported built-in method, it
		% must be a user-supplied predicate (with the assumption
		% that the user has used sbdd_try/2 for labelling, as
		% required).
		\+ translate_indomain_atom(Choice, _),
		SBDDChoice = Choice
	    )
	->
	    labeling(L,Arg,Select,SBDDChoice,In,Out,Node_option,Node,Module)
	;
	    error(5, search(L,Arg,Select,Choice,gap_sbdd,[]), Module)
	).

    % Check the search list only contains SBDD variables (or non-variables).
gap_sbdd_var_list(L,Arg) :-
	(
	    foreach(X,L),
	    param(Arg)
	do
	    access(X,Arg,Var),
	    once((nonvar(Var) ; ic_gap_sbdd:is_sbdd_var(Var)))
	).


% bbs(+List:list,
%        ++Arg:integer,
%	++Select:atom,
%	+Choice:atom or p/2,
%	++Steps:integer,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	++Module:atom)
% same as labeling, but stops after Steps backtracking steps
%
:-mode bbs(+,++,++,+,++,?,?,?,++,++).
bbs(L,Arg,Select,Choice,Steps,In,Out,Node_option, Node,Module):-
	getval(backtrack, CurrentBacktracks),
	BacktrackLimit is CurrentBacktracks+Steps,
	setval(backtrack_limit,BacktrackLimit),
	block(bbs1(L,Arg,Select,Choice,In,Out,Node_option, Node,Module),bbs,fail).

:-mode bbs1(+,++,++,+,?,?,?,++,++).
bbs1(Xs,Arg,Select,Choice,In,Out,Node_option, Node,Module):-
	( search_delete(X,Xs,R,Arg,Select,Module) ->
	    tree_fixed(X,Node_option,Arg,Choice,Node,bbs),
	    choose(X,Arg,Choice,In,In1,Module),
	    inc_backtrack_count_check,
	    tree_node(X,Node_option, Node,Node1,Module),
	    bbs1(R,Arg,Select,Choice,In1,Out,Node_option, Node1,Module)
	;
	    Out=In
	).


% credit(+List:list,++Arg:integer,++Select:atom,+Choice:atom or p/2,
%	 ++Credit:integer,
%	 ++Extra:integer or bbs(integer) or lds(integer),
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%        ++ Module)
% same as labeling, but uses credit to control search
% always give half the credit to the first child,
% half of the remaining credit to the next child, etc

:-mode credit(+,++,++,+,++,++,?,?,?,++,++).
credit([],_Arg,_Select,_Choice,_Credit,_Extra,In,In,_Node_option, _Node,_Module).
credit(L,Arg,Select,Choice,Credit,Extra,In,Out,Node_option, Node,Module):-
	L = [_|_],
	credit1(L,Arg,Select,Choice,Credit,Extra,In,Out,Node_option, Node,Module).

:-mode credit1(+,++,++,+,++,++,?,?,?,++,++).
credit1(Xs,Arg,Select,Choice,1,Extra,In,Out,Node_option, Node,Module):-
	integer(Extra),
	!,
	bbs(Xs,Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
credit1(Xs,Arg,Select,Choice,1,bbs(Extra),In,Out,Node_option, Node,Module):-
	!,
	bbs(Xs,Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
credit1(Xs,Arg,Select,Choice,1,lds(Extra),In,Out,Node_option, Node,Module):-
	!,
	lds(Xs,Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
credit1(Xs,Arg,Select,Choice,Credit,Extra,In,Out,Node_option, Node,Module):-
	Credit > 1,
	( search_delete(X,Xs,R,Arg,Select,Module) ->
	    set_credit(Shelf,Credit),
	    tree_fixed(X,Node_option,Arg,Choice,Node,credit),
	    choose(X,Arg,Choice,In,In1,Module),
	    inc_backtrack_count,
	    tree_node(X,Node_option, Node,Node1,Module),
	    distribute_credit(Shelf,Credit_child,Rest),
	    (Rest = 0 ->
		!  % cut away remaining choices in choose
	    ;
		true
	    ),
	    credit1(R,Arg,Select,Choice,Credit_child,Extra,In1,Out,Node_option, Node1,Module)
	;
	    Out=In
	).

:-mode set_credit(++,++).
set_credit(Shelf,Credit):-
	shelf_create(credit(Credit),Shelf).

% the credit distribution
% always give (a bit more than) half the credit to the next child
% keep the rest of the credit for the other children
% do not use up credit yourself
% if credit remains, and there are no more children, the credit is lost
% if children do not use their credit, it is lost
:-mode distribute_credit(++,-,-).
distribute_credit(Shelf,Credit,Rest):-
	shelf_get(Shelf,1,Old),
	Credit is (Old+1)//2,
	Rest is Old-Credit,
	shelf_set(Shelf,1,Rest).


% lds(+List:list,++Arg:integer,++Select:atom,++Choice:atom,
%	 ++LDS:integer,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%        ++Module:atom)
% same as labeling, but only allows max LDS discrepancies against heuristic
% solution
% first tries 0, then 1, then 2, up to LDS discrepancies
%

:-mode lds(+,++,++,+,++,?,?,?,++,++).
lds(L,Arg,Select,Choice,Lds,In,Out,Node_option, Node,Module):-
	between(0,Lds,1,Disc),
	lds1(L,Arg,Select,Choice,Disc,In,Out,Node_option, Node,Module).

:-mode lds1(+,++,++,+,++,?,?,?,++,++).
lds1(Xs,Arg,Select,Choice,Disc,In,Out,Node_option, Node,Module):-
	( search_delete(X,Xs,R,Arg,Select,Module) ->
	    ( Disc=0 ->
		tree_fixed(X,Node_option,Arg,Choice,Node,lds_0),
		once(choose(X,Arg,Choice,In,In1,Module)), % allows only shallow backtracking
		update_nodes_counter, % create new node name
		tree_node(X,Node_option, Node,Node1,Module),
		lds1(R,Arg,Select,Choice,0,In1,Out,Node_option, Node1,Module)
	    ;
		set_discrepancy(Shelf,Disc),	% Disc >= 1
		tree_fixed(X,Node_option,Arg,Choice,Node,lds),
		choose(X,Arg,Choice,In,In1,Module),
		inc_backtrack_count,
		tree_node(X,Node_option, Node,Node1,Module),
		(dec_discrepancy(Shelf,Disc1) ->
		    true
		;
		    !,  % cut away remaining choices in choose
		    Disc1 = 0
		),
		lds1(R,Arg,Select,Choice,Disc1,In1,Out,Node_option, Node1,Module)
	    )
	;
	    Disc = 0, % do not allow to use less than given discrepancies
	    Out=In
	).

:-mode set_discrepancy(++,++).
set_discrepancy(Shelf,Disc):-
	shelf_create(disc(Disc),Shelf).

:-mode dec_discrepancy(++,++).
dec_discrepancy(Shelf,Disc):-
	shelf_get(Shelf,1,Disc),
	shelf_dec(Shelf,1).	% fail if already 0


% dbs(+List:list,++Arg:integer,++Select:atom,+Choice:atom,
%	 ++Level:integer,
%	 ++Extra:integer or bbs(integer) or lds(integer),
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%        ++ Module:atom)
% same as labeling, but uses depth bounded search to control search
% explore all choice points in the first Level variables

:-mode dbs(+,++,++,+,++,++,?,?,?,++,++).
dbs(Xs,Arg,Select,Choice,0,Extra,In,Out,Node_option, Node,Module):-
	integer(Extra),
	!,
	bbs(Xs,Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
dbs(Xs,Arg,Select,Choice,0,bbs(Extra),In,Out,Node_option, Node,Module):-
	!,
	bbs(Xs,Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
dbs(Xs,Arg,Select,Choice,0,lds(Extra),In,Out,Node_option, Node,Module):-
	!,
	lds(Xs,Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
dbs(Xs,Arg,Select,Choice,Level,Extra,In,Out,Node_option, Node,Module):-
	Level >= 1,
	( search_delete(X,Xs,R,Arg,Select,Module) ->
	    tree_fixed(X,Node_option,Arg,Choice,Node,dbs),
	    choose(X,Arg,Choice,In,In1,Module),
	    inc_backtrack_count,
	    tree_node(X,Node_option, Node,Node1,Module),
	    Level1 is Level-1,
	    dbs(R,Arg,Select,Choice,Level1,Extra,In1,Out,Node_option, Node1,Module)
	;
	    Out=In
	).

/***********************************************************************

value choice

***********************************************************************/

% choose(?X,++Arg:integer,++Method:atom,?In,?Out,++Module:atom)
% this predicate chooses a value for the selected term
% this choice is non-deterministic
% for the user defined case, the whole selected term is passed so that
% the user-predicate can assign more than one argument inside
%
:-mode choose(?,++,++,?,?,++).
choose(X,N,LM:Method,In,Out,_Module) ?-
	!,
	choose(X,N,Method,In,Out,LM).
choose(X,N,indomain,_In, _Out, _Module):-
	!,
	access(X,N,Var),
	indomain(Var).
choose(X,N,Type,_In, _Out, _Module):-
	translate_indomain_atom(Type, IndomainType),
	!,
	access(X,N,Var),
	indomain(Var,IndomainType).
choose(X,_Arg,Method,_In,_Out,Module):- % this is called for a user-defined method
	atom(Method),
	!,
	call(Method,X)@Module.		% may be non-deterministic
choose(X,_Arg,Method,In,Out,Module):- % this is called for a user-defined method
	functor(Method,F,A),
	% this is an awkward convention, due by backwards compatibility:
	( A==1 -> arg(1,Method,Fixed), call(F,X,Fixed)@Module
	; A==2 ->                      call(F,X,In,Out)@Module
	; A==3 -> arg(1,Method,Fixed), call(F,X,Fixed,In,Out)@Module
	).


/************************************************************

utilities

************************************************************/

% Translate search/6's indomain choice atoms to those used by indomain/2
translate_indomain_atom(indomain, enum).
translate_indomain_atom(indomain_min, min).
translate_indomain_atom(indomain_max, max).
translate_indomain_atom(outdomain_min, reverse_min).	% Zinc
translate_indomain_atom(outdomain_max, reverse_max).	% Zinc
translate_indomain_atom(indomain_reverse_min, reverse_min).
translate_indomain_atom(indomain_reverse_max, reverse_max).
translate_indomain_atom(indomain_middle, middle).
translate_indomain_atom(indomain_median, median).
translate_indomain_atom(indomain_split, split).
translate_indomain_atom(indomain_reverse_split, reverse_split).
translate_indomain_atom(indomain_interval, interval).
translate_indomain_atom(indomain_random, random).
translate_indomain_atom(sbds(Choice), Atom) :-
	translate_indomain_atom_sbds(Choice, Atom).
translate_indomain_atom(gap_sbds(Choice), Atom) :-
	translate_indomain_atom_gap_sbds(Choice, Atom).
translate_indomain_atom(gap_sbdd(Choice), Atom) :-
	translate_indomain_atom_gap_sbdd(Choice, Atom).

% Translate search/6's indomain choice atoms to those used by indomain/2
% when the search method is SBDS.
translate_indomain_atom_sbds(indomain_min, sbds_min).
translate_indomain_atom_sbds(indomain_max, sbds_max).
translate_indomain_atom_sbds(indomain_middle, sbds_middle).
translate_indomain_atom_sbds(indomain_median, sbds_median).
translate_indomain_atom_sbds(indomain_random, sbds_random).

% Translate search/6's indomain choice atoms to those used by indomain/2
% when the search method is GAP SBDS.
translate_indomain_atom_gap_sbds(indomain_min, gap_sbds_min).
translate_indomain_atom_gap_sbds(indomain_max, gap_sbds_max).
translate_indomain_atom_gap_sbds(indomain_middle, gap_sbds_middle).
translate_indomain_atom_gap_sbds(indomain_median, gap_sbds_median).
translate_indomain_atom_gap_sbds(indomain_random, gap_sbds_random).

% Translate search/6's indomain choice atoms to those used by indomain/2
% when the search method is GAP SBDD.
translate_indomain_atom_gap_sbdd(indomain_min, gap_sbdd_min).
translate_indomain_atom_gap_sbdd(indomain_max, gap_sbdd_max).
translate_indomain_atom_gap_sbdd(indomain_middle, gap_sbdd_middle).
translate_indomain_atom_gap_sbdd(indomain_median, gap_sbdd_median).
translate_indomain_atom_gap_sbdd(indomain_random, gap_sbdd_random).

% Defines the allowable value selection methods to be used with SBDS
sbds_indomain_choice(Method) :-
	translate_indomain_atom_sbds(Method, _).

% Defines the allowable value selection methods to be used with GAP SBDS
gap_sbds_indomain_choice(Method) :-
	translate_indomain_atom_gap_sbds(Method, _).

% Defines the allowable value selection methods to be used with GAP SBDD
gap_sbdd_indomain_choice(Method) :-
	translate_indomain_atom_gap_sbdd(Method, _).

% access argument N of term X, if N=0, X is returned
:-mode access(?,++,-).
access(X,0,X) :- !.
access(X,N,Var):-
	N > 0,
	arg(N,X,Var).

% Initialize the accumulator variable for the search choice
% this is only used if Choose is a functor of arity 2
:-mode in_out(?,-,-).
in_out(T,In,Out):-
	arity(T,A),
	( A==2 -> arg(1,T,In), arg(2,T,Out)
	; A==3 -> arg(2,T,In), arg(3,T,Out)
	; In=(-), Out=(-)
	).

reset_backtrack_count(Option):-
	nr_nodes(Option,N),
	setval(node_limit,N),
	setval(nodes,0),
	setval(backtrack,0).

nr_nodes(Option,N):-
	member(nodes(N),Option),
	!.
nr_nodes(Option,2000):-
	member(node(_),Option),
	!.
nr_nodes(_Option,none).

	
:-mode get_backtrack_count(?).
get_backtrack_count(L):-
	memberchk(backtrack(N),L), !,
	getval(backtrack,N).
get_backtrack_count(_).

inc_backtrack_count:-
	update_nodes_counter,
	setval(one_level,true).
inc_backtrack_count:-
	getval(one_level,true),
	setval(one_level,false),
	incval(backtrack),
	fail.

inc_backtrack_count_check :-
	update_nodes_counter,
	setval(one_level,true).
inc_backtrack_count_check :-
	getval(one_level,true),
	setval(one_level,false),
	incval(backtrack),
	( getval(backtrack) =< getval(backtrack_limit) ->
	    fail
	;
	    exit_block(bbs)
	).

update_nodes_counter:-
	incval(nodes),
	getval(node_limit, Max),
	( integer(Max), getval(nodes) >= Max ->
%	    writeln(jumping(nodes)),
	    exit_block(nodes)
	;
	    true
	).


/*
value_range([H|T],Arg,Range):-
	access(H,Arg,H1),
	value_range(T,H1,Msg),
	dom(Msg,Range).

value_range([],X,X).
value_range([H|T],Old,End):-
	access(H,Arg,H1),
	dvar_msg(H1,Old,New),
	value_range(T,New,End).

*/

/*

this checks the option list to find the node(_) option
it also creates the root node with parent node -1
and the initial node number is returned

if no node option is used, then it uses none for call and node number
*/

:-mode tree_option(?,-,-,++).
tree_option(Option,none,none,_Module):-
	var(Option),
	!,
	writeln(option_should_be_nonvar),
	abort.
tree_option(Option,Call,Node,Module):-
	member(node(Call),Option),
	!,
	tree_node(root,Call,-1,Node,Module).
tree_option(_Option,none,none,_Module).

/*

this predicate is called after the choice point
depending on the type of node action, different routines are called

returns the newly generated node name
*/

:-mode tree_node(?,+,++,-,++).
tree_node(_X,none,none,none,_Module):- % this case if no node option is used
	!.
tree_node(X,daVinci,Parent,Node,_Module):- % display the complete term
	integer(Parent),
	!,
	getval(nodes,Node),
	daVinci_node(Node,X),
	(Parent \= -1 ->
	    daVinci_edge(Node,Parent,Node)
	;
	    true
	).
tree_node(X,daVinci(Info_pred),Parent,Node,Module):- % display only Info returned
	integer(Parent),
	!,
	getval(nodes,Node),
	call(Info_pred,X,Info)@Module,
	daVinci_node(Node,Info),
	(Parent \= -1 ->
	    daVinci_edge(Node,Parent,Node)
	;
	    true
	).
tree_node(X,Call,Parent,Node,Module):- % custom routine
	integer(Parent),
	!,
	getval(nodes,Node),
	call(Call,daVinci,X,Parent,Node)@Module.
tree_node(X,Call,Parent,Node,Module):- % catch error in calling pattern
	writeln(wrong_tree_node(X,Call,Parent,Node,Module)),
	abort.


/*

this predicate is called before the value choice part
it decides on some attributes of the current node

*/
:-mode tree_fixed(?,+,++,+,++,++).
tree_fixed(_X,none,_Arg,_Choice,_Node,_Type):-
	!.
tree_fixed(X,_Node_option,Arg,Choice,Node,Type):-
	translate_indomain_atom(Choice, _),	% Test for predefined choice
	!,
	access(X,Arg,Var),
	fixed_style(Node,Var,Type).
tree_fixed(X,_Node_option,Arg,_Choice,Node,_Type):-
	access(X,Arg,Var),
	fixed_style(Node,Var,custom).

fixed_style(Node,Var,Type):-
	type_color(Type,Color),
	daVinci_node_attribute(Node,'COLOR',Color),
	(integer(Var) ->
	    daVinci_node_attribute(Node,'FONTSTYLE',italic)
	;
	    true
	).
fixed_style(Node,_Var,_Type):-
	daVinci_node_attribute(Node,'BORDER','double'),
	fail.
/*

data for the mapping of search type to color

custom means that it is a custom assignment routine, not a custom search routine

*/

:-mode type_color(++,-).
type_color(complete,white).
type_color(credit,goldenrod).
type_color(lds,lightgray).
type_color(lds_0,gray). % this is a lds branch were all discrepancies have been used
type_color(bbs,white).
type_color(dbs,pink).
type_color(custom,orchid).

/*

test if we are in a daVinci based operation

*/

option_for_daVinci(Option):-
	member(node(daVinci),Option),
	!.
option_for_daVinci(Option):-
	member(node(daVinci(_)),Option),
	!.



/***********************************************************************

variable selection 

***********************************************************************/

search_delete(H, List, T, Arg, LM:Select, _Module) ?- !,
	search_delete(H, List, T, Arg, Select, LM).
search_delete(H, List, T, Arg, Select, Module) :- 
	compound(Select),
	functor(Select, Name, 1),
	arg(1, Select, Method),
	once call(Name,H,List,T,Arg,Method)@Module.
search_delete(H, List, T, Arg, SelectMethod, Module) :-
	atom(SelectMethod),
	delete(H,List,T,Arg,SelectMethod,Module).


:-tool(delete/5, delete/6).

% delete(-X,+List:non_empty_list,-R:list,++Arg:integer,++Select:atom,
%            ++Module:atom)
% choose one entry in the list based on a heuristic
% this is a deterministic selection
% a special case for input order to speed up the selection in that case
%
:-mode delete(-,+,-,++,++,++).
delete(H,List,T,_Arg,input_order,_Module):-
	!, List = [H|T].
delete(X,List,R,Arg,Select,Module):-
	List = [H|T],
	find_criteria(H,Arg,Select,Crit,Module),
	( var(Crit) ->
	    X=H, R=T	% we can't do any better!
	;
	    find_best_and_rest(T,List,Crit,X,R,Arg,Select,Module)
	).


% find_best_and_rest(
%	+List:list,		the unscanned tail
%	+BestSoFar:list,	the tail starting with the current best
%	?Crit: variable, number or crit(Crit,Crit),
%	-Best, -Rest_best:list,	the result
%	++Arg:integer,++Select:atom,++Module:atom)
%
:- mode find_best_and_rest(+,+,?,-,-,++,++,++).
find_best_and_rest([], BestSoFar, _OldCrit, BestVar, Rest, _Arg, _Select, _Module) :- !,
	BestSoFar = [BestVar|Rest].
find_best_and_rest(List, BestSoFar, CritOld, BestVar, Rest, Arg, Select, Module) :-
	List = [Var|Vars],
	find_criteria(Var, Arg, Select, CritNew, Module),
	( CritNew @>= CritOld ->	% no better than the old one, continue
	    find_best_and_rest(Vars, BestSoFar, CritOld, BestVar, Rest, Arg, Select, Module)
	; nonvar(CritNew) ->		% found a better one, continue
	    % copy the chunk between old and new best
	    copy_until_elem(BestSoFar, Var, Rest, Rest0),
	    find_best_and_rest(Vars, List, CritNew, BestVar, Rest0, Arg, Select, Module)
	;
	    % we can't do any better, stop
	    BestVar = Var,
	    % copy the chunk between old and new best, and append the unscanned rest
	    copy_until_elem(BestSoFar, Var, Rest, Vars)
	).


% find_criteria(?Term,++Arg:integer,++Select:atom,
%		-Crit:integer or crit(integer,integer),
%               ++Module:atom)
%
% find a heuristic value from a term
:-mode find_criteria(?,++,++,-,++).
find_criteria(Term,0,Select,Crit,Module):-
	!,
	find_value(Term,Select,Crit,Module).
find_criteria(Term,Arg,Select,Crit,Module):-
	arg(Arg,Term,X),
	find_value(X,Select,Crit,Module).

% find_value(?X:dvarint,++Select:atom,
%	     -Crit:integer or crit(integer,integer),
%            ++Module:atom)
%
% Find a heuristic value from a domain variable: the smaller, the better.
% Values will be compared using @<, so be aware of standard term ordering!
% If the Criterion remains uninstantiated, this indicates an optimal value,
% which will be picked without looking any further down the list.
:-mode find_value(?,++,-,++).
find_value(X,first_fail,Size,_Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_size(X,Size0),
	    ( integer(Size0) -> Size=Size0 ; Size=inf )	% 99 @< 'inf'
	).
find_value(X,anti_first_fail,Number,_Module):-
	!,
	get_size(X,Size),				% can be 1.0Inf
	Number is -Size.				% -1.0Inf @< -99
find_value(X,smallest,Min,_Module):-
	!,
	get_lwb(X,Min).
find_value(X,largest,Number,_Module):-
	!,
	get_upb(X,Max),
	Number is -Max.
find_value(X,occurence,Number,Module):-	% mis-spelt in first version
	!,
	find_value(X,occurrence,Number,Module).
find_value(X,occurrence,Number,_Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_constraints_number(X,Nr), % this is very heavy
	    Number is -Nr
	).
find_value(X,max_regret,Number,_Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_compact_domain_rep(X,L),
	    nth_value(L,2,V),
	    get_lwb(X,Min),
	    Number is -(V-Min)
	).
find_value(X,most_constrained,Crit,Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    Crit = crit(Size,Number),
	    find_value(X,first_fail,Size,Module),
	    find_value(X,occurrence,Number,Module)
	).
find_value(X,User_method,Value,Module):-
	atom(User_method),
	% CAUTION: X passed, not list Term (compatibility)!
	once call(User_method,X,Value)@Module.	% make deterministic


% Copy list until first occurrence of K and return as difference list
:- mode copy_until_elem(+,?,?,?).
copy_until_elem([X|Xs], K, Ys, Ys0) :-
	( X==K ->
	    Ys = Ys0
	;
	    Ys = [X|Ys1],
	    copy_until_elem(Xs, K, Ys1, Ys0)
	).


/****************************************************

some indomain variants

****************************************************/

% indomain(?X:dvarint,++Type:atomic)
% Type is either one of min, max, middle or an integer
% these indomain versions remove the previous value on backtracking
:-mode indomain(?,++).
indomain(X,Type):-
	( is_integer_type(X) ->
	    indomain1(X,Type)
	;
	    error(5, indomain(X, Type))
	).

:-mode indomain1(?,++).
indomain1(X,enum):-
	indomain(X).
indomain1(X,min):-
	get_lwb(X,Min),
	indomain_min(X,Min).
indomain1(X,sbds_min):-
	sbds_indomain_min(X).
indomain1(X,gap_sbds_min):-
	gap_sbds_indomain_min(X).
indomain1(X,gap_sbdd_min):-
	gap_sbdd_indomain_min(X).
indomain1(X,max):-
	get_upb(X,Max),
	indomain_max(X,Max).
indomain1(X,sbds_max):-
	sbds_indomain_max(X).
indomain1(X,gap_sbds_max):-
	gap_sbds_indomain_max(X).
indomain1(X,gap_sbdd_max):-
	gap_sbdd_indomain_max(X).
indomain1(X,reverse_min):-
	get_lwb(X,Min),
	outdomain_min(X,Min).
indomain1(X,reverse_max):-
	get_upb(X,Max),
	outdomain_max(X,Max).
indomain1(X,middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,Value).
indomain1(X,sbds_middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,sbds(Value)).
indomain1(X,gap_sbds_middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,gap_sbds(Value)).
indomain1(X,gap_sbdd_middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,gap_sbdd(Value)).
indomain1(X,median):-
	select_initial_value_median(X,Value),
	indomain1(X,Value).
indomain1(X,sbds_median):-
	select_initial_value_median(X,Value),
	indomain1(X,sbds(Value)).
indomain1(X,gap_sbds_median):-
	select_initial_value_median(X,Value),
	indomain1(X,gap_sbds(Value)).
indomain1(X,gap_sbdd_median):-
	select_initial_value_median(X,Value),
	indomain1(X,gap_sbdd(Value)).
indomain1(X,split):-
	indomain_split(X).
indomain1(X,reverse_split):-
	indomain_reverse_split(X).
indomain1(X,interval):-
	indomain_interval(X).
indomain1(X,random):-
	indomain_random(X).
indomain1(X,sbds_random):-
	sbds_indomain_random(X).
indomain1(X,gap_sbds_random):-
	gap_sbds_indomain_random(X).
indomain1(X,gap_sbdd_random):-
	gap_sbdd_indomain_random(X).
indomain1(X,Value):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    indomain_min(X,Min)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    indomain_max(X,Max)
	;
	    % enumerate from a starting value inside the domain
	    % is this enough in all cases ??
	    Range is 2*max(Max-Value,Value-Min)+1,
	    indomain_from(X,Value,1,Range)
	).
indomain1(X,sbds(Value)):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    sbds_indomain_min(X)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    sbds_indomain_max(X)
	;
	    % enumerate from a starting value inside the domain
	    sbds_indomain_from(X,Value,0)
	).
indomain1(X,gap_sbds(Value)):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    gap_sbds_indomain_min(X)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    gap_sbds_indomain_max(X)
	;
	    % enumerate from a starting value inside the domain
	    gap_sbds_indomain_from(X,Value,0)
	).
indomain1(X,gap_sbdd(Value)):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    gap_sbdd_indomain_min(X)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    gap_sbdd_indomain_max(X)
	;
	    % enumerate from a starting value inside the domain
	    gap_sbdd_indomain_from(X,Value,0)
	).

    % translate middle into a starting value
select_initial_value_middle(X,Value) :-
	get_bounds(X,Min,Max),
	Value is (Min+Max)//2. % HS: remember to use integer division

    % translate median into a starting value
select_initial_value_median(X,Value) :-
	get_size(X,Size),
	Index is 1+Size//2,
	get_compact_domain_rep(X, L),
	nth_value(L,Index,Value).

% indomain_from(?X:dvar, ++Value:integer, ++Inc:integer, ++Range:integer)
% the choice consists in either taking the proposed value or in excluding it
% and choosing another one
% the next value is always the old value plus the increment
% the next increment is one bigger than the previous, but of opposite sign
% 1, -2, 3, -4, 5, -6, 7 ...
% if the increment becomes too large, you can stop
:-mode indomain_from(?,++,++,++).
indomain_from(X,X,_,_).
indomain_from(X,Value,Inc,Range):-
	X #\= Value,
	Value1 is Value+Inc,
	Inc1 is -sgn(Inc)*(abs(Inc)+1),
	Range >= abs(Inc1),
	indomain_from(X,Value1,Inc1,Range).

:-mode sbds_indomain_from(?,++,++).
sbds_indomain_from(X,_,_):-
	nonvar(X).
sbds_indomain_from(X,Value0,Inc0):-
	var(X),
	Value is Value0+(2*(Inc0 mod 2)-1)*Inc0,
	( check_in(Value, X) ->
	    % Don't want to call this if Value not in X's domain
	    sbds_module:sbds_try(X,Value)
	;
	    true
	),
	Inc is Inc0+1,
	sbds_indomain_from(X,Value,Inc).

:-mode gap_sbds_indomain_from(?,++,++).
gap_sbds_indomain_from(X,_,_):-
	nonvar(X).
gap_sbds_indomain_from(X,Value0,Inc0):-
	var(X),
	Value is Value0+(2*(Inc0 mod 2)-1)*Inc0,
	( check_in(Value, X) ->
	    % Don't want to call this if Value not in X's domain
	    ic_gap_sbds:sbds_try(X,Value)
	;
	    true
	),
	Inc is Inc0+1,
	gap_sbds_indomain_from(X,Value,Inc).

:-mode gap_sbdd_indomain_from(?,++,++).
gap_sbdd_indomain_from(X,_,_):-
	nonvar(X).
gap_sbdd_indomain_from(X,Value0,Inc0):-
	var(X),
	Value is Value0+(2*(Inc0 mod 2)-1)*Inc0,
	( check_in(Value, X) ->
	    % Don't want to call this if Value not in X's domain
	    ic_gap_sbdd:sbdd_try(X,Value)
	;
	    true
	),
	Inc is Inc0+1,
	gap_sbdd_indomain_from(X,Value,Inc).

% indomain_min(?X:dvar, ++Value:integer)
% the choice consists in either taking the proposed value or in excluding it
% and choosing another one
:-mode indomain_min(?,++).
indomain_min(X,X).
indomain_min(X,Min):-
	X #> Min,
	get_lwb(X,New),
	indomain_min(X,New).

:-mode outdomain_min(?,++).
outdomain_min(X,Min):-
	X #> Min,
	get_lwb(X,New),
	outdomain_min(X,New).
outdomain_min(X,X).

sbds_indomain_min(X):-
	nonvar(X).
sbds_indomain_min(X):-
	var(X),
	get_lwb(X,Min),
	sbds_module:sbds_try(X,Min),
	sbds_indomain_min(X).

gap_sbds_indomain_min(X):-
	nonvar(X).
gap_sbds_indomain_min(X):-
	var(X),
	get_lwb(X,Min),
	ic_gap_sbds:sbds_try(X,Min),
	gap_sbds_indomain_min(X).

gap_sbdd_indomain_min(X):-
	nonvar(X).
gap_sbdd_indomain_min(X):-
	var(X),
	get_lwb(X,Min),
	ic_gap_sbdd:sbdd_try(X,Min),
	gap_sbdd_indomain_min(X).

% indomain_max(?X:dvar, ++Value:integer)
% the choice consists in either taking the proposed value or in excluding it
% and choosing another one
:-mode indomain_max(?,++).
indomain_max(X,X).
indomain_max(X,Max):-
	X #< Max,
	get_upb(X,New),
	indomain_max(X,New).

:-mode outdomain_max(?,++).
outdomain_max(X,Max):-
	X #< Max,
	get_upb(X,New),
	outdomain_max(X,New).
outdomain_max(X,X).

sbds_indomain_max(X):-
	nonvar(X).
sbds_indomain_max(X):-
	var(X),
	get_upb(X,Max),
	sbds_module:sbds_try(X,Max),
	sbds_indomain_max(X).

gap_sbds_indomain_max(X):-
	nonvar(X).
gap_sbds_indomain_max(X):-
	var(X),
	get_upb(X,Max),
	ic_gap_sbds:sbds_try(X,Max),
	gap_sbds_indomain_max(X).

gap_sbdd_indomain_max(X):-
	nonvar(X).
gap_sbdd_indomain_max(X):-
	var(X),
	get_upb(X,Max),
	ic_gap_sbdd:sbdd_try(X,Max),
	gap_sbdd_indomain_max(X).

% split the domain into intervals until only an integer value is left
:-mode indomain_split(?).
indomain_split(X):-
	integer(X),
	!.
indomain_split(X):-
	get_bounds(X,Min,Max),
	Middle is (Min+Max) div 2,
	(
	    X #=< Middle
	;
	    X #> Middle
	),
	indomain_split(X).

:-mode indomain_reverse_split(?).
indomain_reverse_split(X):-
	integer(X),
	!.
indomain_reverse_split(X):-
	get_bounds(X,Min,Max),
	Middle is (Min+Max) div 2,
	(
	    X #> Middle
	;
	    X #=< Middle
	),
	indomain_reverse_split(X).

% assign values by first choosing one interval from the domain and
% then assigning values from the middle of that domain
:-mode indomain_interval(?).
indomain_interval(X):-
	get_compact_domain_as_list(X,L),
	fix_interval(X,L).

:-mode fix_interval(?,++).
fix_interval(X,[A|_R]):-
	integer(A),
	X #= A.
fix_interval(X,[A|R]):-
	integer(A),
	X #\= A,
	fix_interval(X,R).
fix_interval(X,[_A..B|_R]):-
	X #=< B,
	indomain(X,split).  % there are many alternatives here
fix_interval(X,[_A..B|R]):-
	X #> B,
	fix_interval(X,R).

% choose values from the domain at random; on backtracking, the previous value
% is removed, so that it can be used for a complete enumeration
:-mode indomain_random(?).
indomain_random(X):-
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	indomain_random(X,Try).

:-mode indomain_random(?,++).
indomain_random(X,X).
indomain_random(X,Try):-
	X #\= Try,
	indomain_random(X).

sbds_indomain_random(X):-
	nonvar(X).
sbds_indomain_random(X):-
	var(X),
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	sbds_module:sbds_try(X,Try),
	sbds_indomain_random(X).

gap_sbds_indomain_random(X):-
	nonvar(X).
gap_sbds_indomain_random(X):-
	var(X),
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	ic_gap_sbds:sbds_try(X,Try),
	gap_sbds_indomain_random(X).

gap_sbdd_indomain_random(X):-
	nonvar(X).
gap_sbdd_indomain_random(X):-
	var(X),
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	ic_gap_sbdd:sbdd_try(X,Try),
	gap_sbdd_indomain_random(X).


/****************************************************

other useful stuff

****************************************************/

:-export(nth_value/3).

nth_value(V, 1, V) :-
	integer(V).
nth_value(I, N, V) :-
	I = _.._,
	nth_value1(I, [], N, V).
nth_value([I | R], N, V) :-
	nth_value1(I, R, N, V).

nth_value1(A..B, R, N, V) :-
	A1 is A + N - 1,
	N1 is A1 - B,
	( N1 > 0 ->
	    nth_value(R, N1, V)
	;
	    A1 >= A,
	    V = A1
	).
nth_value1(A, R, N, V) :-
	atomic(A),
	nth_value2(A, R, N, V).

nth_value2(A, _R, 1, V) :-
	!,
	V = A.
nth_value2(_A, R, N, V) :-
	N1 is N - 1,
	nth_value(R, N1, V).

