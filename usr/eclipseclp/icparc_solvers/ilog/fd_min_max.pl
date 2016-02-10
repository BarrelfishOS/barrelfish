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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: fd_min_max.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $
% ----------------------------------------------------------------------

% $Id: fd_min_max.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $

% From original fd.pl

:- module_interface(fd_min_max).

:- use_module(fd_ilog).

:- begin_module(fd_min_max).

:- TopPreds = (
    minimize/2,
    minimize/4,
    minimize/5,
    minimize/6,
    minimize/8,
    minimize_bound_check/0,
    min_max/2,
    min_max/4,
    min_max/5,
    min_max/6,
    min_max/8),

    export(TopPreds).

:-  tool(minimize/2, minimize_body/3),
    tool(minimize/4, minimize_body/5),
    tool(minimize/5, minimize_body/6),
    tool(minimize/6, minimize_body/7),
    tool(minimize/8, minimize_body/9),
    tool(min_max/2, min_max_body/3),
    tool(min_max/4, min_max_body/5),
    tool(min_max/5, min_max_body/6),
    tool(min_max/6, min_max_body/7),
    tool(min_max/8, min_max_body/9).

:- export 
    minimize_body/3,
    minimize_body/5,
    minimize_body/6,
    minimize_body/7,
    minimize_body/9,
    min_max_body/3,
    min_max_body/5,
    min_max_body/6,
    min_max_body/7,
    min_max_body/9.

:- import
	% general-purpose predicates
	call_local/1,
	maxint/1,
	minint/1,
	par_true/0,
	prune_woken_goals/1,
	worker_boundary/0
    from sepia_kernel.

:- pragma(debug).
:- pragma(silent_debug).
:- pragma(system).
:- pragma(expand).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Higher-order predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This lock is needed for atomic update of the cost bound in minimize
:- make_local_array(minimize_lock),
   mutex_init(minimize_lock).

:- make_local_array(minimize_stack, global_reference).

push(StackName, Item) :-
    getval(StackName, Stack),
    setval(StackName, [Item|Stack]).

top(StackName, Top) :-
    getval(StackName, Stack),
    nonvar(Stack), Stack = [Top|_].


%
% Min-Max: Branch & Bound by restarting each time a new solution is found.
%

% Simplified versions with less arguments

min_max_body(Goal, Cost, Module) :-
    minint(Min), maxint(Max),
    min_max_body(Goal, Goal, Goal, Cost, Min, Max, 0, 0, Module).

min_max_body(Goal, Template, Solution, Cost, Module) :-
    minint(Min), maxint(Max),
    min_max_body(Goal, Template, Solution, Cost, Min, Max, 0, 0, Module).

min_max_body(Goal, Cost, Lower, Upper, Percent, Module) :-
    min_max_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, 0, Module).

min_max_body(Goal, Cost, Lower, Upper, Percent, Timeout, Module) :-
    min_max_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, Timeout, Module).


% The general min_max with all options

min_max_body(Goal, Template, Solution, Value, Lower, Upper, Percent, Timeout, Module) :-
    prune_woken_goals(Goal),
    ( var(Value) ->	List = [Value]
    ; Value = [_|_] ->	List = Value
    ;			List = [Value]
    ),
    ( max_list_range(List, expr, MinList, MaxList) ->
    	Low is max(MinList, Lower),
    	Max is min(MaxList, Upper),
	term_arr(sol(no_solution,Max), Index),
	% the s/1 wrapper makes it fail safely if no solution
	bbr2(Goal, s(Template), s(Solution), List, Low, Max, Percent, Timeout, Index, Module)
    ;
	error(5, min_max(Goal, Value, Lower, Upper, Percent, Timeout), Module)
    ).

bbr2(Goal, Template, _Solution, List, Low, Max, Percent, Timeout, Index, Module) :-
    set_timeout(Timeout),
    block(call_local(branch_and_bound_restart(Goal, Template, List,
    		Low, Max, Percent, Index, Module)), Tag, handle_exit(Tag)).
bbr2(_Goal, _Template, Solution, _, _, _, _, Timeout, Index, _) :-
    reset_timeout(Timeout),
    arr_get(Index, 1, Solution),	% fail here if no solution
    arr_abolish(Index),
    fd_true.

branch_and_bound_restart(Goal, Solution, List, Low, Max, Percent, Index, Module) :-
    worker_boundary,
    push(minimize_stack, min_max),
    par_true,	% for better incremental stack copying

    repeat,
    arr_get(Index, 2, M),
    (
	constrain_max_list(List, M),		% post new cost constraints
	call(Goal, Module),
	schedule_suspensions(postponed), wake
    ->
	max_of_min_list_domains(List, Cost),
	(nonvar(Cost) ->
	    error(280, (Cost, Goal)),
	    % In case the solution still contains variables,
	    % we want to strip most of their attributes.
	    % Otherwise we might copy the whole constraint store!
	    copy_term(Solution, StrippedSolution),
	    arr_set(Index, 1, StrippedSolution),
	    NewUp is min(Cost - fix(Cost * Percent)//100, Cost - 1),
	    arr_set(Index, 2, NewUp),
	    % According to the chipc manual this should be NewUp < Low !
	    Cost < Low				% restart from repeat
	;
	    error(4, min_max(Goal, List, Low, Max, Percent), Module)
	)
    ;
	true
    ),
    !, fail.


%
% Minimize: Branch & Bound by backtracking, the cost limit is represented
%	    by a global variable. This version might not be as efficient
%	    as when a new (non-backtrackable) constraint is actually added
%	    to the store. It can be improved by an explicit check
%	    on every labeling step: minimize_bound_check/0
%

% Simplified versions with less arguments

minimize_body(Goal, Cost, Module) :-
    minint(Min), maxint(Max),
    minimize_body(Goal, Goal, Goal, Cost, Min, Max, 0, 0, Module).

minimize_body(Goal, Template, Solution, Cost, Module) :-
    minint(Min), maxint(Max),
    minimize_body(Goal, Template, Solution, Cost, Min, Max, 0, 0, Module).

minimize_body(Goal, Cost, Lower, Upper, Percent, Module) :-
    minimize_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, 0, Module).

minimize_body(Goal, Cost, Lower, Upper, Percent, Timeout, Module) :-
    minimize_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, Timeout, Module).


% The general minimize with all options

minimize_body(Goal, Template, Solution, Value, Lower, Upper, Percent, Timeout, Module) :-
    prune_woken_goals(Goal),
    ( var(Value) ->	List = [Value]
    ; Value = [_|_] ->	List = Value
    ;			List = [Value]
    ),
    ( max_list_range(List, var, MinList, MaxList) ->
    	Low is max(MinList, Lower),
    	Max is min(MaxList, Upper),
	term_arr(sol(no_solution,Max), Index),
	% the s/1 wrapper makes it fail safely if no solution
	bb2(Goal, s(Template), s(Solution), List, Low, Max, Percent, Timeout, Index, Module)

    ;
	( Goal == Template ->
	    error(5, minimize(Goal, Value, Lower, Upper, Percent, Timeout), Module)
	;
	    error(5, minimize(Goal, Template, Solution, Value, Lower, Upper, Percent, Timeout), Module)
	)
    ).

bb2(Goal, Template, _Solution, List, Low, Max, Percent, Timeout, Index, Module) :-
    set_timeout(Timeout),
    block(call_local(branch_and_bound(Goal, Template, List,
    		Low, Max, Percent, Index, Module)), Tag, handle_exit(Tag)).
bb2(_Goal, _Template, Solution, _, _, _, _, Timeout, Index, _) :-
    reset_timeout(Timeout),
    arr_get(Index, 1, Solution),	% fail here if no solution
    arr_abolish(Index),
    fd_true.

branch_and_bound(Goal, Solution, List, Low, Max, Percent, Index, Module) :-
    worker_boundary,
    constrain_max_list_index(List, Index),
    push(minimize_stack, List/Index),

    call(Goal, Module),

    max_of_min_list_domains(List, Cost),
    schedule_suspensions(postponed), wake,

    (nonvar(Cost) ->
	% Update cost if better. This must be atomic.
	mutex(minimize_lock, (
	    Cost =< arr_get(Index, 2),
	    error(280, (Cost, Goal)),
	    copy_term(Solution, StrippedSolution),
	    arr_set(Index, 1, StrippedSolution),
	    NewUp is min(Cost - fix(Cost * Percent)//100, Cost - 1),
	    arr_set(Index, 2, NewUp),
	    Cost < Low			% backtrack into Goal
	))
    ;
	error(4, minimize(Goal, List, Low, Max, Percent), Module)
    ),
    !, fail.

% Get the minimum and maximum value of a list of domain vars
% Where gets unified with 'expr' when the list contains expressions
max_list_range(List, Where, Min, Max) :-
    maxint(Maxint),
    minint(Minint),
    max_list_range(List, Where, Minint, Max, Maxint, Min).

max_list_range([], _, Max, Max, Min, Min).
max_list_range([Var|Rest], Where, SoFar, Max, MinSoFar, Min) :-
    ( dvar_domain(Var, Domain) -> true
    ; default_domain(Var), dvar_domain(Var, Domain) ),
    dom_range(Domain, DMin, DMax),
    !,
    NewMin is min(MinSoFar, DMin),
    NewMax is max(SoFar, DMax),
    max_list_range(Rest, Where, NewMax, Max, NewMin, Min).
max_list_range([Term|Rest], expr, SoFar, Max, MinSoFar, Min) :-
    term_to_linear(Term, LTerm),
    linear_term_range(LTerm, DMin, DMax),
    NewMin is min(MinSoFar, DMin),
    NewMax is max(SoFar, DMax),
    max_list_range(Rest, _Where, NewMax, Max, NewMin, Min).

% Constrain all variables in the list to be smaller than Max
constrain_max_list([], _).
constrain_max_list([Term|Rest], Max) :-
    Term #<= Max,
    constrain_max_list(Rest, Max).

% Constrain all variables in the list to be smaller than the cost bound
constrain_max_list_index([], _).
constrain_max_list_index([Term|Rest], Index) :-
    constrain_max_index(Term, Index),
    constrain_max_list_index(Rest, Index).

constrain_max_index(Term, Index) :-
    var(Term),
    arr_get(Index, 2, Max),
    Term #<= Max,
    make_suspension(constrain_max_index(Term, Index), 2, Susp),
    insert_suspension(Term, Susp, min of fd, fd).
constrain_max_index(Term, Index) :-
    nonvar(Term),
    arr_get(Index, 2, Max),
    Term =< Max.

%
% Explicit check that can be used with minimize in additional choice points
%
minimize_bound_check :-
    ( top(minimize_stack, List/Index) ->
      check_max_list_index(List, Index)
    ;
      true    % stack empty or we are in a min_max (no check needed)
    ).

check_max_list_index([], _).
check_max_list_index([Term|Rest], Index) :-
    check_max_index(Term, Index),
    check_max_list_index(Rest, Index).

check_max_index(Term, Index) :-
    var(Term),
    arr_get(Index, 2, Max),
    Term #<= Max.
check_max_index(Term, Index) :-
    nonvar(Term),
    arr_get(Index, 2, Max),
    Term =< Max.

max_of_min_list_domains([], Cost, Cost, _, _).
max_of_min_list_domains([Term|Rest], SoFar, Max, Var, Val) :-
    nonvar(Term),
    !,
    min_domain(Term, DMin),
    (DMin > SoFar -> 
	max_of_min_list_domains(Rest, DMin, Max, Var, Val)
    ;
	max_of_min_list_domains(Rest, SoFar, Max, Var, Val)
    ).
max_of_min_list_domains([Var|Rest], SoFar, Max, Var, DMin) :-
    var(DMin),				% fail if not the first variable
    min_domain(Var, DMin),
    (DMin > SoFar -> 
	max_of_min_list_domains(Rest, DMin, Max, _, DMin)
    ;
	max_of_min_list_domains(Rest, SoFar, Max, _, DMin)
    ).

min_domain(Term, Min) :-
    nonvar(Term),
    !,
    (nonground(Term) ->
	term_to_linear(Term, LTerm),
	linear_term_range(LTerm, Min, Min)
    ;
	Min is Term
    ).
min_domain(Var, Min) :-
    dvar_domain(Var, Domain),
    dom_range(Domain, Min, _).

max_of_min_list_domains([Term|List], Cost) :-
    min_domain(Term, Min),
    (max_of_min_list_domains(List, Min, Cost, Var, Val) ->
	Var = Val
    ;
	true				% unbound if an error
    ).

% A true which is not optimized away so that we have a call instruction which
%   forces a debugger event which traces the woken goals before the exit...
fd_true.

:- pragma(nodebug).

%
% Timeout handling
%
set_timeout(0) :- !.
set_timeout(Timeout) :-
    set_event_handler(alrm, handle_timeout/0),
    set_timer(real, Timeout).

reset_timeout(0) :- !.		% do not reset if we didn't set it
reset_timeout(_) :-
    set_timer(real, 0),
    reset_event_handler(alrm).

handle_exit(timeout) :-
    !,
    fail.
handle_exit(Tag) :-
    exit_block(Tag).

handle_timeout :-
    exit_block(timeout).

cost_handler(_, (Cost, _)) :-
    printf("Found a solution with cost %d\n%b", Cost).

:- current_interrupt(_, alrm) -> set_interrupt_handler(alrm, event/1) ; true.
:- set_event_handler(280, cost_handler/2).

:- untraceable
	branch_and_bound/8,			% called from block
	branch_and_bound_restart/8.
