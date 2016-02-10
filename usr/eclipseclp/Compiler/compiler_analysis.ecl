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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: compiler_analysis.ecl,v 1.9 2010/03/12 10:22:46 jschimpf Exp $
%----------------------------------------------------------------------

:- module(compiler_analysis).

:- comment(summary, "ECLiPSe III compiler - dataflow analysis").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2010/03/12 10:22:46 $").

:- use_module(compiler_common).
:- use_module(compiler_map).

:- local op(200, fx, [--,++,?]).

%----------------------------------------------------------------------

:- comment(binding_analysis/1, [
    summary:"Analyse data flow in one predicate and annotate accordingly",
    amode:binding_analysis(+),
    args:[
	"Body":"Normalised source of the predicate"
    ],
    see_also:[print_goal_state/3],
    desc:ascii("
	This takes the normalised source of a predicate and analyses its
	determinism and dataflow. The result of the analysis is stored in
	the normalised source data structure itself (the state-fields of
	every subgoal's struct(goal)).

	We do conservative analysis, we can only record information that
	cannot change during subsequent (forward) execution, like
	aliasing and instantiation.
	We do not track uninstantiatedness, for example. This could change
	due to wakeups, for example. (we could do uninitialisedness, though)
    ")
]).

:- export binding_analysis/1.


binding_analysis(Body) :-
	initial_state(StartingState),
	binding_analysis(Body, StartingState, _EndState).

    initial_state(state{determinism:det,bindings:Map0}) :-
    	compiler_map:init(Map0).


% binding_analysis(+Goals, +State, -State)
%	Traverse the goals and collect binding information

binding_analysis([], State, State).
binding_analysis([Goal|Goals], State0, State) :-
	binding_analysis(Goal, State0, State1),
	binding_analysis(Goals, State1, State).
binding_analysis(disjunction{branches:Branches,state:State0}, State0, State) :-
	(
	    foreach(Branch,Branches),
	    foreach(EndState,EndStates),
	    param(State0)
	do
	    binding_analysis(Branch, State0, EndState)
	),
	merge_alternative_states(State0, EndStates, State).
binding_analysis(goal{kind:head,functor:Pred,args:Args,state:State,definition_module:Mod}, State0, State) :- !,
	( get_flag(Pred, mode, Modes)@Mod ->
	    (
		foreacharg(Mode,Modes),
	    	foreach(Arg,Args),
		fromto(State0,State1,State2,State)
	    do
	    	use_mode(Mode, Arg, State1, State2)
	    )
	;
	    mark_args_as(?univ, Args, State0, State)
	).
binding_analysis(goal{functor:F/A,args:Args,state:State0,path:File,line:Line,callpos:Pos}, State0, State) :-
	( goal_effect(F, A, Args, Pos, State0, State) ->
	    true
	;
	    update_struct(state, [determinism:failure], State0, State),
	    ( expected_failure(F, A) ->
		true
	    ;
		( File == '' -> true ;
		    local_file_name(File, LocalFile),
		    printf(warning_output, "File %w, line %d:%n  ", [LocalFile,Line])
		),
		printf(warning_output, "WARNING: calling %Kw will always fail%n", [F/A])
	    )
	).


% Analyse the effect of an individual goal
% Fail if the goal would certainly fail at runtime

goal_effect((=), 2, [A1,A2], _, State0, State) :- !,
	unify_effect(A1, A2, State0, State).
goal_effect(atom, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++atom, State0, State).
goal_effect(atomic, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++atomic, State0, State).
goal_effect(breal, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++breal, State0, State).
goal_effect(compound, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, +compound, State0, State).
goal_effect(float, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++float, State0, State).
goal_effect(free, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ?univ, State0, State).
goal_effect(get_cut, 1, [A1], Pos, State0, State) :- !,
	constrain_type(A1, ++cutpoint(Pos), State0, State).
goal_effect(ground, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++univ, State0, State).
goal_effect(integer, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++integer, State0, State).
goal_effect(is_handle, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++handle, State0, State).
goal_effect(meta, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ?univ, State0, State).
goal_effect(nonvar, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, +univ, State0, State).
goal_effect(number, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++number, State0, State).
goal_effect(rational, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++rational, State0, State).
goal_effect(real, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++number, State0, State).
goal_effect(string, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ++string, State0, State).
goal_effect(var, 1, [A1], _, State0, State) :- !,
	constrain_type(A1, ?univ, State0, State).
goal_effect(_, _, Args, _, State0, State) :-
	mark_args_as(?univ, Args, State0, State).

expected_failure(fail, 0).
expected_failure(false, 0).

mark_args_as(_, [], State, State).
mark_args_as(Domain, [Arg|Args], State0, State) :-
	mark_arg_as(Domain, Arg, State0, State1),
	mark_args_as(Domain, Args, State1, State).

    mark_arg_as(Domain, variable{varid:VarId}, State0, State) :- !,
	enter_binding(VarId, Domain, State0, State).
    mark_arg_as(Domain, [Arg1|Arg2], State0, State) :- !,
	mark_arg_as(Domain, Arg1, State0, State1),
	mark_arg_as(Domain, Arg2, State1, State).
    mark_arg_as(Domain, structure{args:Args}, State0, State) :- !,
	mark_args_as(Domain, Args, State0, State).
    mark_arg_as(_, _, State, State).


%use_mode(--, _Arg, State, State).	% i.e. mark_arg_as(--univ,...)
%use_mode(-, Arg, State0, State) :-
%	mark_arg_as(-univ, Arg, State0, State).
use_mode(-, _Arg, State, State).	% i.e. mark_arg_as(--univ,...)
use_mode(?, Arg, State0, State) :-
	mark_arg_as(?univ, Arg, State0, State).
use_mode(+, Arg, State0, State) :-
	mark_arg_as(+univ, Arg, State0, State).
use_mode(++, Arg, State0, State) :-
	mark_arg_as(++univ, Arg, State0, State).


/*
Further candidates for exploiting type information:

functor(_, value(N), value(A))
functor(N/A, _, _)
	-> functor(N/A, value(N), value(A))

functor(_, _, _)
	-> functor(univ, atom, integer)

_ =.. _
	-> univ =.. ./2

N/A =.. [_|_]
	N/A =.. [value(N)|_]

+(integer, integer, _)
	-> +(integer, integer, integer)

*/

% constrain_type(+Term, +Type, +State0, -State)

constrain_type(variable{varid:VarId}, Domain, State0, State) :- !,
	enter_binding(VarId, Domain, State0, State).
constrain_type(X, Domain, State, State) :-
    	term_abstract(X, State, XDomain),
	abstract_unify(XDomain, Domain, _).	% may fail


%----------------------------------------------------------------------
% Compute the effect of the unification.
% Fails if unification will surely fail at runtime.
%----------------------------------------------------------------------

unify_effect(variable{varid:VarId1}, variable{varid:VarId2}, State0, State) :- !,
	alias_effect(VarId1, VarId2, State0, State).
unify_effect(variable{varid:VarId}, NonVar, State0, State) :- !,
	binding_effect(VarId, NonVar, State0, State).
unify_effect(NonVar, variable{varid:VarId}, State0, State) :- !,
	binding_effect(VarId, NonVar, State0, State).
unify_effect(_, _, _State0, _State) :-
	unreachable("unify_effect/4: unexpected unnormalised unification").
%unify_effect([Arg1|Args1], [Arg2|Args2], State0, State) :-
%	unify_effect(Arg1, Arg2, State1, State2),
%	unify_effect(Args1, Args2, State1, State2).
%unify_effect(structure{name:N,arity:A,args:Args1},
%	structure{name:N,arity:A,args:Args2}, State0, State) :-
%	unify_effect(Args1, Args2, State0, State).


% binding_effect(+VarId, +NonVar, +State, -State)

binding_effect(VarId, structure{name:F,arity:A,args:Args}, State0, State) :- !,
	% TODO: propagate groundness to Args if VarId is ground
	enter_binding(VarId, +(F/A), State0, State1),
	mark_args_as(?univ, Args, State1, State).
binding_effect(VarId, [Arg1|Arg2], State0, State) :- !,
	enter_binding(VarId, +((.)/2), State0, State1),
	mark_arg_as(?univ, Arg1, State1, State2),
	mark_arg_as(?univ, Arg2, State2, State).
binding_effect(VarId, Constant, State0, State) :- !,
	enter_binding(VarId, ++value(Constant), State0, State).

    enter_binding(VarId, NewBinding, State0, State) :-
	State0 = state{bindings:Map0},
	update_struct(state, [bindings:Map1], State0, State),
	( lookup_binding(Map0, VarId, OldBinding, AliasVarId) ->
	    abstract_unify(OldBinding, NewBinding, Binding),	% may fail
	    compiler_map:det_update(Map0, AliasVarId, Binding, Map1)
	;
	    compiler_map:det_insert(Map0, VarId, NewBinding, Map1)
	).

% lookup with dereferencing kown aliases
lookup_binding(Map, VarId, Binding) :-
    lookup_binding(Map, VarId, Binding, _AliasVarId).

lookup_binding(Map, VarId, Binding, AliasVarId) :-
    compiler_map:search(Map, VarId, Binding1), % may fail
    ( Binding1 = alias(NextVarId) ->
	lookup_binding(Map, NextVarId, Binding, AliasVarId)
    ;
	AliasVarId = VarId,
	Binding = Binding1
    ).


% alias_effect(+VarId1, +VarId2, +State, -State)

alias_effect(VarId, VarId, State0, State) ?-
	State = State0.
alias_effect(VarId1, VarId2, State0, State) :-
	State0 = state{bindings:Map0},
	update_struct(state, [bindings:Map1], State0, State),
	enter_alias(VarId1, VarId2, Map0, Map1).

    enter_alias(VarId1, VarId2, Map0, Map) :-
	( lookup_binding(Map0, VarId1, Binding1, AliasVarId1) ->
	    ( lookup_binding(Map0, VarId2, Binding2, AliasVarId2) ->
		( abstract_unify(Binding1, Binding2, Binding) ->
		    true
		;
%		    printf(warning_output,
%			"WARNING: unification of %w with %w will always fail%n",
%			[Binding1, Binding2]),
		    fail
		),
		compiler_map:det_update(Map0, AliasVarId1, alias(AliasVarId2), Map1),
		compiler_map:det_update(Map1, AliasVarId2, Binding, Map)
	    ;
		compiler_map:det_insert(Map0, VarId2, alias(AliasVarId1), Map1),
		( abstract_alias(Binding1, Binding) ->
		    compiler_map:det_update(Map1, AliasVarId1, Binding, Map)
		;
		    Map = Map1
		)
	    )
	; lookup_binding(Map0, VarId2, Binding2, AliasVarId2) ->
	    compiler_map:det_insert(Map0, VarId1, alias(AliasVarId2), Map1),
	    ( abstract_alias(Binding2, Binding) ->
		compiler_map:det_update(Map1, AliasVarId2, Binding, Map)
	    ;
		Map = Map1
	    )
	;
	    compiler_map:det_insert(Map0, VarId1, alias(VarId2), Map1),
	    compiler_map:det_insert(Map1, VarId2, ?univ, Map)
	).

    

%----------------------------------------------------------------------
% Primitive operations on the representation of variable bindings
%
% The type tree:
% 
% univ
%   +---------------------------------------------------------------+
% atomic                                                         compound
%   +-------------------------------+-------+-------+-------+       |
% number                          string  atom   handle  cutpoint  N/A
%   +-------+-------+--------+      |       |
% integer float   rational breal  value() value()
%   |       |       |        |
% value() value() value()  value()
% 
%
% Instantiations:
%
%               ? any
%              / \
%      nonvar +   - var (possibly aliased)
%             |   |
%     ground ++   -- uninit
%
% No binding information is equivalent to --univ (uninitialised).
%
% Currently, '--' only occurs together with univ.
%
% We do not track '-' currently, because a variable
% 1. may be instantiated as a side effect of instantiating another 
%    variable to which it is aliased.
% 2. may be instantiated as a side effect of waking a delayed goal in
%    which it (or a variable to which it may be aliased) appears.
% Only '--' variables do neither suffer nor cause these effects.
% Note that we traditionally treat mode(-) as meaning '--', because
% otherwise it's not much use.

% Get the abstract representation of a (variable or nonvariable) term
:- mode term_abstract(+,+,-).
term_abstract(variable{varid:VarId}, State, Domain) :-
	State = state{bindings:Map},
	( lookup_binding(Map, VarId, Domain) ->
	    true
	;
	    Domain = --univ
	).
term_abstract(structure{name:N,arity:A}, _State, +(N/A)).	% TODO: groundness
term_abstract([_|_], _State, +((.)/2)).
term_abstract(X, _State, ++(value(X))) :- atomic(X).
	

abstract_union(D1, D2, D) :-
	functor(D1, I1, 1), arg(1, D1, T1),
	functor(D2, I2, 1), arg(1, D2, T2),
	inst_union(I1, I2, I),
	type_union(T1, T2, T),
	functor(D, I, 1), arg(1, D, T).


abstract_unify(D1, D2, D) :-
	functor(D1, I1, 1), arg(1, D1, T1),
	functor(D2, I2, 1), arg(1, D2, T2),
	inst_unify(I1, I2, I),
	type_unify(T1, T2, T),
	functor(D, I, 1), arg(1, D, T).


% The effect of unifying something with --univ. If no effect, fail.
% Same as abstract_unify(T1,--univ,T), T1\==T
%abstract_alias(--T, -T).
abstract_alias(--T, ?T).


% supertype(++Type, -Level, -SuperType)

supertype(value(X), 8, integer) :- integer(X), !.
supertype(value(X), 8, float) :- float(X), !.
supertype(value(X), 8, rational) :- rational(X), !.
supertype(value(X), 8, breal) :- breal(X), !.
supertype(value(X), 7, atom) :- atom(X), !.
supertype(value(X), 7, string) :- string(X), !.
supertype(integer, 7, number).
supertype(float, 7, number).
supertype(rational, 7, number).
supertype(breal, 7, number).
supertype(_/_, 6, compound).
supertype(number, 6, atomic).
supertype(handle, 6, atomic).
supertype(cutpoint(_), 6, atomic).
supertype(string, 6, atomic).
supertype(atom, 6, atomic).
supertype(compound, 5, univ).
supertype(atomic, 5, univ).
supertype(univ, 1, top).


type_union(T1, T2, T) :-
    	supertype(T1, L1, P1),
    	supertype(T2, L2, P2),
	( L1 < L2 ->
	    type_union(T1, P2, T)
	; L1 > L2 ->
	    type_union(P1, T2, T)
	; T1 == T2 ->
	    T = T1
	;
	    type_union(P1, P2, T)
	).


type_unify(T1, T2, T) :-
    	supertype(T1, L1, P1),
    	supertype(T2, L2, P2),
	( L1 < L2 ->
	    upto(L1, P2, T1),
	    T = T2
	; L1 > L2 ->
	    upto(L2, P1, T2),
	    T = T1
	;
	    T1 == T2,
	    T=T1
	).

    upto(L, T, A) :-
    	supertype(T, LT, P),
	( L < LT ->
	    upto(L, P, A)
	;
	    A = T
	).


inst_unify(Inst1, Inst2, Inst) :-
	N is max(inst_order(Inst1), inst_order(Inst2)),
	once inst_order(Inst, N).

    inst_order( -, 1).	% this clause first!
    inst_order(--, 1).	% -- = -- gives -
    inst_order( ?, 2).
    inst_order( +, 3).
    inst_order(++, 4).


inst_union(--, Y, LUB) :- 'lub--'(Y, LUB).
inst_union( -, Y, LUB) :- 'lub-'(Y, LUB).
inst_union( ?, _, ?).
inst_union( +, Y, LUB) :- 'lub+'(Y, LUB).
inst_union(++, Y, LUB) :- 'lub++'(Y, LUB).

    'lub--'(--, LUB) :- !, LUB = (--).
    'lub--'( -, LUB) :- !, LUB = (-).
    'lub--'( _, ?).
    'lub-'(--, LUB) :- !, LUB = (-).
    'lub-'( -, LUB) :- !, LUB = (-).
    'lub-'( _, ?).
    'lub+'(++, LUB) :- !, LUB = (+).
    'lub+'( +, LUB) :- !, LUB = (+).
    'lub+'( _, ?).
    'lub++'(++, LUB) :- !, LUB = (++).
    'lub++'( +, LUB) :- !, LUB = (+).
    'lub++'( _, ?).


%----------------------------------------------------------------------
% Merging the final analysis states of disjunctions:
% Only if something nontrivial has been derived about a variable
% in _every_ disjunctive branch, we can merge this information and
% proceed with it.
% Aliasing: with the current alias-chain representation it is difficult
% to extract the information common to alternative branches (we would
% have to intersect sets of aliased variables).  For the time being,
% we simply lose any aliasing information collected within the branches.
%----------------------------------------------------------------------

merge_alternative_states(State, [], State).
merge_alternative_states(State0, EndStates, State) :-
	State0 = state{bindings:Map0},
	EndStates = [state{bindings:FirstEndMap}|_],
	compiler_map:keys(FirstEndMap, VarIds),
	(
	    foreach(VarId, VarIds),
	    fromto(State0, State1, State2, State),
	    param(Map0,EndStates)
	do
	    % get what was known about this variable before the disjunction
	    ( lookup_binding(Map0, VarId, InitialBinding, AliasVarId) ->
		true
	    ;
	        InitialBinding = --univ, AliasVarId = VarId
	    ),
	    % if all branches derived something, then merge and use it
	    (
		merge_end_bindings(InitialBinding, VarId, EndStates, EndBinding),
		enter_binding(AliasVarId, EndBinding, State1, State2)
	    ->
		true
	    ;
	    	State2 = State1
	    )
	).
	
    % Merge the binding information from all the disjunctive branches.
    % There are several occasions where we stop early and fail:
    % When a branch has no information, or when the merged information
    % is the same as the initial one before the disjunction.

    merge_end_bindings(InitialBinding, VarId, EndStates, EndBinding) :-
	EndStates = [state{bindings:Map0}|MoreEndStates],
	certainly_once lookup_binding(Map0, VarId, FirstEndBinding),
	FirstEndBinding \= InitialBinding,		% may fail
	(
	    foreach(state{bindings:MapI}, MoreEndStates),
	    fromto(FirstEndBinding,MergedBinding1,MergedBinding2,EndBinding),
	    param(InitialBinding,VarId)
	do
	    lookup_binding(MapI, VarId, EndBindingI),	% may fail
	    abstract_union(MergedBinding1, EndBindingI, MergedBinding2),
	    MergedBinding2 \= InitialBinding		% may fail
	).


%----------------------------------------------------------------------
% Print the analysis result
%----------------------------------------------------------------------

:- export print_goal_state/3.

print_goal_state(_Stream, _Indent, State) :-
    var(State), !.	% no analysis results yet
print_goal_state(Stream, Indent0, state{determinism:Det,bindings:Map}) :-
    Indent is Indent0+1,
    indent(Stream, Indent),
    printf("DETERMINISM: %w%n", [Det]),
    compiler_map:to_sorted_assoc_list(Map, Bindings),
    ( Bindings = [_|_] ->
	indent(Stream, Indent),
	printf("BINDING INFO:%n", []),
	( foreach(Binding,Bindings), param(Stream,Indent,Map) do
	    indent(Stream, Indent),
	    ( Binding = VarId - alias(_Alias) ->
		lookup_binding(Map, VarId, FinalBinding),
		writeln(Binding -> FinalBinding)
	    ;
		writeln(Binding)
	    )
	)
    ;
        true
    ).


:- export state_lookup_binding/3.
state_lookup_binding(state{bindings:Map}, VarId, Binding) :-
	lookup_binding(Map, VarId, Binding).
