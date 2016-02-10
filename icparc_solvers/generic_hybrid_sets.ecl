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
% Contributor(s): Joachim Schimpf, IC-Parc
% Contributor(s): Andrew Sadler, IC-Parc
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Solver for constraints over finite sets of integers
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: generic_hybrid_sets.ecl,v 1.4 2010/07/25 13:29:05 jschimpf Exp $
%
%	Many thanks to Neng-Fa Zhou, on whose ideas this solver
%	implementation is based. We started work on this solver
%	during his unexpected - but very welcome - visit during
%	August 1-3 2000. The solver functionality is roughly that
%	of Carmen Gervet's Conjunto solver. The implementation
%	techniques are the same Neng-Fa used in his own set solver
%	for B-Prolog, i.e. the use of add/remove events for incre-
%	mental propagators, and the use of finite domain variables
%	to represent the set bounds. The latter has been replaced
%	now due to poor performance in ECLiPSe.
%
% Todo:
%	- weight propagation using cardinality
%	- improve membership such that domain updates in X get propagated
%	- largest(?Set, ?El) and smallest(?Set, ?El)
%	- n_largest(?Set, ?Elems) and n_smallest(?Set, ?Elems)
%	- set ordering constraint
%	- msg/3
%	- more complete interface for writing your own constraints
%	- use more efficient algorithms for initial constraint consistency
%	  (currently done by simulating incremental additions/removals)
% ----------------------------------------------------------------------


% This file is intended to be included into a module of the form:
% :- module(xx_sets).

:- export
	op(500, yfx, (\)),
	op(700, xfx, in_set_range),
	op(700, xfx, disjoint),
	op(700, xfx, sameset),
	op(700, xfx, in),
	op(700, xfx, notin),
	op(700, xfx, includes),
	op(700, xfx, subset),
	op(700, xfx, subsetof),
        op(700, xfx, less),
        op(700, xfx, satisfies),
        op(700, xfx, leq).

:-tool((satisfies)/2, satisfies/3).

:- export
	(::)/2,			% ?Set1 :: +Gset..+Gset
	(in_set_range)/2,	% ?Set1 in_set_range +Gset..+Gset
	intset/3,		% intset(?Set, +Min, +Max)
	intsets/4,		% intsets(-Sets, ?N, +Min, +Max)
	(in)/2,			% ?X in ?Set		membership
	(in)/3,			% in(?X, ?Set, ?Bool)	reified membership
	(notin)/2,		% ?X notin ?Set		non-membership
	(disjoint)/2,		% ?Set1 disjoint Set2	disjoint
	(sameset)/2,		% ?Set1 sameset Set2	equality
        less/2,                 % lexicographic strict less-than
        leq/2,                  % lexicographic less-than-or-equal-to
	all_disjoint/1,		% all_disjoint(+Sets)
	all_union/2,		% all_union(+Sets, +Union)
	all_intersection/2,	% all_intersection(+Sets, +Intersection)
	(includes)/2,		% ?Set1 includes Set2	superset
	(subset)/2,		% ?Set1 subset Set2	subset
	union/3,		% union(?Set1, ?Set2, ?Set3)
	intersection/3,		% intersection(?Set1, ?Set2, ?Set3)
	difference/3,		% difference(?Set1, ?Set2, ?Set3)
	symdiff/3,		% symdiff(?Set1, ?Set2, ?Set3)
	(#)/2,			% #(?Set, ?Card)	cardinality
	weight/3,		% weight(?Set, +ElementWeights, ?SetWeight)
	membership_booleans/2,	% membership_booleans(?Set, ?BoolArray)
	insetdomain/4.		% insetdomain(?Set, +CardSel, +ElemSel, +Order)

:- export			% ------- Low-level interface --------
	get_set_attribute/2,	% get_set_attribute(?Set, -Attr)
	is_solver_var/1,	% is_solver_var(?Thing)
	is_solver_type/1,	% is_solver_type?Thing)
	potential_members/2,	% potential_members(?Set, -UpbMinusLwb)
	set_range/3.		% set_range(?Set, -Lwb, -Upb) [conjunto compat]

:-export
  get_set_attribute/2,
  next_lex_min/3,
  prev_lex_max/3,
  set_lex_min/3,
  set_lex_max/3.

:-export
        lex_min_max/3,          % lex_min_max(?Set, -LexMin, -LexMax)
                                % lexicographicaly least and greatest set
        lex_set_range/3.        % lex_set_range(?Set, -LexLwb, -LexUpb) Glb and Lub in lex order (ie.reversed)

:-export
  satisfies/2,           % ?S satisfies +Pred
  intersect_atmost_n/4,  % intersect_atmost_n(?S1, ?S2, +N)
  local_intersect_atmost_n/4,  % local_intersect_atmost_n(?S1, ?S2, +N)
  intersect_lex/3,       % intersect_lex(?S1, ?S2, ?S3) with lex propagation
  union_lex/3,           % union_lex(?S1, ?S2, ?S3) with lex propagation
  all_union_lex/2,	 % all_union_lex(+Sets, +Union)
  difference_lex/3.      % difference_lex(?S1, ?S2, ?S3) with lex propagation


% backtrack counter
:- setval(bt_count,0).
:- setval(set_lex_min_count,0).
:- setval(set_lex_max_count,0).

%:- lib(ordset).
:- use_module(lex_set).

:- meta_attribute(int_sets, [
	print:print_setvar/2,
	unify:unify_sets/2,
	test_unify:test_unify_sets/2,
	suspensions:suspensions_set/3,
	delayed_goals_number:delayed_goals_number_set/2,
	copy_term:copy_term_set/2,
	compare_instances:compare_instances_set/3
    ]).

:- export struct(int_sets(
	    dom,	% set domain representation
            lex_min,    % lexicographicaly least valid instantiation
            lex_max,    % lexicographicaly greatest valid instantiation
            lex_glb,    % lex ordered GLB
            lex_lub,    % lex ordered GLB
            lex_dirty,  % flag which when set will result in lex_glb
                        % and lex_lub being recalculated
	    off,	% integer
	    lcard,	% integer
	    ucard,	% integer
	    added,	% notification send-port
	    removed,	% notification send-port
	    add,	% suspension list
	    rem,	% suspension list
            min_susp,        % suspension list
            max_susp,        % suspension list
	    card,	% fd/ic variable
	    booleans,	% vector of fd/ic variables
%	    invoc,	% integer
	    value	% set variable
    	)).

:- comment(struct(int_sets), [
    summary:"Attribute structure for set variables (and constants)",
    fields:[
	    "dom":"set domain representation (array of booleans)",
            "lex_min":"lexicographicaly least valid instantiation",
            "lex_max":"lexicographicaly greatest valid instantiation",
            "lex_glb":"lex ordered GLB",
            "lex_lub":"lex ordered GLB",
            "lex_dirty":"flag which when set will result in lex_glb",
	    "off":"offset between set element and corresponding array index (integer)",
	    "lcard":"lower bound cardinality (integer)",
	    "ucard":"upper bound cardinality (integer)",
	    "added":"notification send-port for lower bound increases",
	    "removed":"notification send-port for upper bound decreases",
	    "add":"suspension list woken when lower bound increases",
	    "rem":"suspension list woken when upper bound decreases",
            "min_susp":"suspension list woken when lex_min changes",
            "max_susp":"suspension list woken when lex_max changes",
	    "card":"fd/ic variable for cardinality (see #/2)",
	    "booleans":"array of fd/ic variables (see membership_booleans/2)",
	    "value":"set variable belonging to this attribute"
    ],
    see_also:[library(notify_ports),suspend/3,suspend/4]
]).


:- lib(lists).

%----------------------------------------------------------------------
% Debugging stuff
%----------------------------------------------------------------------

replace_with_true(_, true).
replace_with_arg(Stuff, Arg) :- arg(1, Stuff, Arg).

% uncomment the next line to disable debugger ports
:- inline(trace_set/3, replace_with_true/2).
trace_set(_,_,_).
%trace_set(Port, Elem, X{int_sets:int_sets with invoc:I}) ?-
%	trace_port(I, Port, X).
	
% Uncomment one of the next two lines to enable/disable checks
%:- inline(verify/1, expand_verify/2).
:- inline((verify)/1, replace_with_true/2).
:- op(900, fx, verify).
expand_verify(verify Goal, (Goal->true;printf(error, "Check failed: %w%n", [Goal]),abort) ).
verify _Goal :-
	verify _Goal.

:- local variable(prop), variable(events).
:- setval(prop,0), setval(events,0).
% Uncomment one of the next two lines to enable/disable counters
count(Counter) :- incval(Counter).
%count(_).

% Uncomment next line to enable counter printing in the toplevel
%:- set_error_handler(155, print_stat/3).
print_stat(N, G, M) :-
	getval(prop, P), setval(prop, 0),
	getval(events, E), setval(events, 0),
	printf("Propagations: %d, Events: %d%n", [P,E]),
	error(default(N), G, M).


check_instantiation(Cstr) :-
	suspend(check_instantiation(Cstr, Susp), 9,  Cstr->inst, Susp),
	check_instantiation(Cstr, Susp).

:- demon check_instantiation/2.
check_instantiation(Cstr, _Susp) :-
	Cstr =..[_,X,Y,Z|_],
	( nonvar(X),nonvar(Y),var(Z) ->
	    writeln(single_var:Cstr)
	;
	    true
	).

%----------------------------------------------------------------------
% Event notification mechanism
% We use lib(notify_ports) to implement communication from the variables
% to the constraints. Each variable has two send ports in its attribute:
% 'added' where we send the information about added set elements, and
% 'removed' where we send the information about removed set elements.
% The messages simply consist of the set element itself (an integer).
% Receiver ports are usually in a struct(rec), paired with the
% suspension of a propagator who gets killed after the last message
% has been processed and the port is closed.
%----------------------------------------------------------------------

:- lib(notify_ports).

:- local struct(rec(port,susp)).

init_event_receiver(Pos, Attr, Susp, Receiver) :-
	Receiver = rec with susp:Susp,
	open_receiver(Pos, Attr, port of rec, Receiver).

init_event_receiver(Pos, Attr, Susp, InitialEvents, InitialTail, Receiver) :-
	Receiver = rec with susp:Susp,
	open_receiver_init(Pos, Attr, InitialEvents, InitialTail, port of rec, Receiver).

receive_events(Receiver, Events) :-
	nonvar(Receiver),
	receive_notifications(port of rec, Receiver, Events, Status),
	kill_receiver_suspension(Status, Receiver).

kill_receiver_suspension(closed, rec with susp:Susp) :-
    	kill_suspension(Susp).
kill_receiver_suspension(open, _).

kill_receiver_suspension(rec with susp:Susp) :-
    	kill_suspension(Susp).


:- inline(foreachevent/5, tr_foreachevent/2).	
tr_foreachevent(foreachevent(BaseName, Event, Params, Receiver, Goals),
	call_priority((
	    foreachnotification(BaseName, Event, Params, port of rec, Receiver, Status, Goals),
	    kill_receiver_suspension(Status, Receiver)
	), 2)
    ).


:- tool(foreachevent/5, foreachevent/6).
foreachevent(BaseName, Event, Params, Receiver, Goals, Module) :-
	tr_foreachevent(foreachevent(BaseName, Event, Params, Receiver, Goals),
		Transformed),
	call(Transformed)@Module.

%----------------------------------------------------------------------
% Making set variables
%----------------------------------------------------------------------

intsets(Xs, N, MinUniv, MaxUniv) :-
	( count(_,1,N), foreach(X,Xs), param(MinUniv,MaxUniv) do
	    intset(X, MinUniv, MaxUniv)
	).

intset(X{int_sets:Attr}, MinUniv, MaxUniv) ?- nonvar(Attr), !,
	( foreach(I,UnivList), for(I,MaxUniv,MinUniv,-1) do true ),
	intset_intersect_upb(UnivList, Attr).
intset(X, MinUniv, MaxUniv) :- var(X), !,
	intset_domain(SetAttr, MinUniv, MaxUniv),
	add_set_attribute(X, SetAttr).
intset(X, MinUniv, MaxUniv) :-
	set_or_var(X),
	!,
	intset(New, MinUniv, MaxUniv),
	X=New.
intset(X, MinUniv, MaxUniv) :-
	error(5, intset(X, MinUniv, MaxUniv)).

X :: Lwb..Upb ?- !,
	impose_set_bounds(X, Lwb, Upb).
X :: Range :-
	error(5, X :: Range).

subscript(A,I) in_set_range Lwb..Upb ?- !,
	subscript(A, I, X),
	impose_set_bounds(X, Lwb, Upb).
X in_set_range Lwb..Upb ?- !,
	impose_set_bounds(X, Lwb, Upb).
X in_set_range Range :-
	error(5, X in_set_range Range).

impose_set_bounds(_X{int_sets:SetAttr}, Lwb, Upb) ?- nonvar(SetAttr), !,
	intset_union_lwb(Lwb, SetAttr),
	intset_intersect_upb(Upb, SetAttr).
impose_set_bounds(X, Lwb, Upb) :- set_or_var(X), !,
	intset_domain_from_lists(SetAttr, Lwb, Upb),
	( var(X) ->
	    add_set_attribute(X, SetAttr)
	;
	    add_set_attribute(New, SetAttr),
	    X=New
	).
impose_set_bounds(X, Lwb, Upb) :-
	error(5, X in_set_range Lwb..Upb).


% Make the link between a (non-set-attribute) variable
% and a newly constructed set-attribute
add_set_attribute(X, SetAttr) :-
	SetAttr = int_sets with [value:X],
	( var(X) ->
	    add_attribute(X, SetAttr, int_sets),
	    trace_set('NEW_SET', [], X),
	    notify_constrained(X),
	    wake
	;
	    true
	).


%----------------------------------------------------------------------
% Print and access the set attribute
%----------------------------------------------------------------------

print_setvar(_{int_sets:Attr}, Pretty) ?-
	nonvar(Attr),
	Attr = int_sets with [card:C],
        attr_min_max(Attr,LexMin,LexMax),
	lset_to_list(Attr, LwbList),
	( LwbList = [] ->
	    uset_to_list(Attr, UpbList),
	    Pretty = (LwbList..UpbList):C:(LexMin<LexMax)
	    %Pretty = (LexMin<LexMax):C
	;
	    uldiff_to_list(Attr, Maybe),
            Pretty = (LwbList \/ ([]..Maybe)):C:(LexMin<LexMax)
	    %Pretty = (LexMin<LexMax):C
	).

% fail for non-set variable (so it can be used to test for
% set variables, compatible with conjunto)
set_range(_{int_sets:Attr}, LwbList, UpbList) ?-
	nonvar(Attr),
	!,
        lset_uset_to_lists(Attr, LwbList, UpbList).
	%lset_to_list(Attr, LwbList),
	%uset_to_list(Attr, UpbList).
set_range(Set, LwbList, UpbList) :-
	nonvar(Set),
	get_set_attribute(Set, Attr),
        lset_uset_to_lists(Attr, LwbList, UpbList).
	%lset_to_list(Attr, LwbList),
	%uset_to_list(Attr, UpbList).

is_solver_var(_{int_sets:(int_sets with [])}) ?- true.

is_solver_type(_{int_sets:(int_sets with [])}) ?- true.
is_solver_type([]) ?- true.
is_solver_type([_|_]) ?- true.

:-export get_set_attribute/2.
get_set_attribute(_{int_sets:SetAttr0}, SetAttr) ?-
	nonvar(SetAttr0), !,
	SetAttr=SetAttr0.
get_set_attribute(Var, SetAttr) :- var(Var), !,
	error(4, get_set_attribute(Var, SetAttr)).
get_set_attribute(List, SetAttr) ?-
	intset_domain_from_lists(SetAttr, List, List),
	arg(value of int_sets, SetAttr, List).

seteval(S1 \/ S2, S) ?- !,
	union(S1, S2, S).
seteval(S1 /\ S2, S) ?- !,
	intersection(S1, S2, S).
seteval(S1 \ S2, S) ?- !,
	difference(S1, S2, S).
seteval(subscript(Array,Index), S) ?- !,
	% allow array access in set expressions
	subscript(Array, Index, Element),
	seteval(Element, S).
seteval(S , S).

set_or_var(X) :- var(X), !.
set_or_var([]).
set_or_var([_|_]).

% Like get_set_attribute, but allow a free (non-set) variable.
% The free variable is turned into a set variable whose universe
% overlaps the universe of the set variables in the list Others.
get_new_set_attribute(_{int_sets:SetAttr0}, _Others, SetAttr) ?-
	nonvar(SetAttr0), !,
	SetAttr=SetAttr0.
get_new_set_attribute(Var, Others, SetAttr) :- var(Var), !,
	max_universe(Others, MinUniv, MaxUniv),
	intset_domain(SetAttr, MinUniv, MaxUniv),
	add_set_attribute(Var, SetAttr).
get_new_set_attribute(List, _Others, SetAttr) :-
	get_set_attribute(List, SetAttr).

max_universe([XAttr1|XAttrs], Min, Max) ?-
	uset_min_max(XAttr1, MinX1, MaxX1),
	(
	    foreach(XAttr, XAttrs),
	    fromto(MinX1, Min0, Min1, Min),
	    fromto(MaxX1, Max0, Max1, Max)
	do
	    uset_min_max(XAttr, MinX, MaxX),
	    Min1 is min(MinX,Min0),
	    Max1 is max(MaxX,Max0)
	).



:- export watch/1.
watch(Set) :-
	get_set_attribute(Set, SetAttr),
	init_event_receiver(added of int_sets, SetAttr, Susp, AddEvents),
	init_event_receiver(removed of int_sets, SetAttr, Susp, RemEvents),
	suspend(watch_demon(Set, AddEvents, RemEvents), 1,
		[Set->add,Set->rem], Susp).

:- demon watch_demon/3.
watch_demon(V, AddReceiver, RemReceiver) :-
	receive_events(AddReceiver, Added),
	receive_events(RemReceiver, Removed),
	writeln(V-add(Added)-rem(Removed)).


%----------------------------------------------------------------------
% Generic "set must satisfy predicate" constraint
%----------------------------------------------------------------------
satisfies(S,Pred,Mod) :-
        Pred =.. [Functor|Args],
        suspend(satisfies_min(S,Mod,Functor,Args,SuspMin),6,[S->min_susp, S->inst], SuspMin),
        suspend(satisfies_max(S,Mod,Functor,Args,SuspMax),6,[S->max_susp, S->inst], SuspMax),
        schedule_suspensions(1,s([SuspMin,SuspMax])), wake,
        %suspend(satisfies_glb_lub(S),4,[S->add, S->inst]),
        true.

:-demon satisfies_min/5.
satisfies_min(S, Mod,Functor, Args, Susp):-
        (ground(S) ->
            kill_suspension(Susp),
            Pred =.. [Functor,S|Args],
            call(Pred)@Mod
        ;
            lex_min_max(S,Min,_Max),
            Pred0 =.. [Functor,Min|Args],
            (call(Pred0)@Mod ->
                true
            ;
                get_set_attribute(S, Attr),
                next_lex_min(Attr, NewLexMin, NewCMin),
                Pred1 =.. [Functor,NewLexMin|Args],
                call(Pred1)@Mod,
                !,
                set_lex_min(Attr, NewCMin, NewLexMin)
            )
        ).
        
:-demon satisfies_max/5.
satisfies_max(S, Mod, Functor, Args, Susp):-
        (ground(S) ->
            kill_suspension(Susp),
            Pred =.. [Functor,S|Args],
            call(Pred)@Mod
        ;
            lex_min_max(S,_Min,Max),
            Pred0 =.. [Functor,Max|Args],
            (call(Pred0)@Mod ->
                true
            ;
                get_set_attribute(S, Attr),
                prev_lex_max(Attr, NewLexMax, NewCMax),
                Pred1 =.. [Functor,NewLexMax|Args],
                call(Pred1)@Mod,
                !,
                set_lex_max(Attr, NewCMax, NewLexMax)
            )
        ).
        


%----------------------------------------------------------------------
% Membership/Nonmembership constraints
%----------------------------------------------------------------------

E in Set0 :-
	seteval(Set0, Set),
	( var(Set) ->
	    get_set_attribute(Set, SetAttr),
	    ( var(E) ->
		uset_to_list(SetAttr, List),
		solver_module:(E :: List),
		init_event_receiver(removed of int_sets, SetAttr, Susp, Events),
		suspend(membership_demon(E, SetAttr, Events), 2, [E->inst,Set->rem], Susp)
	    ;
		integer(E),
		intset_add(E, SetAttr)
	    )
	;
	    ( var(E) ->
		solver_module:(E :: Set)
	    ;
		integer(E),
		memberchk(E, Set)
	    )
	).
	
	% delays on
	% E->inst
	% Set->rem for rem-events
    :- demon membership_demon/3.
    membership_demon(E, _SetAttr, Receiver) :- var(E),
	receive_events(Receiver, Events),
	( foreach(RemovedElement,Events), param(E) do
	    E #\= RemovedElement
	).
    membership_demon(E, SetAttr, Receiver) :- integer(E),
	kill_receiver_suspension(Receiver),
	intset_add(E, SetAttr).
	

E notin Set0 :-
	seteval(Set0, Set),
	( var(Set) ->
	    get_set_attribute(Set, SetAttr),
	    ( var(E) ->
		lset_to_list(SetAttr, List),
		::(E, List, 0),
		init_event_receiver(added of int_sets, SetAttr, Susp, Events),
		suspend(nonmembership_demon(E, SetAttr, Events), 2, [E->inst,Set->add], Susp)
	    ;
		integer(E),
		intset_remove(E, SetAttr)
	    )
	;
	    ( var(E) ->
		::(E, Set, 0)
	    ;
		integer(E),
		nonmember(E, Set)
	    )
	).
	
	% delays on
	% E->inst
	% Set->add for add-events
    :- demon nonmembership_demon/3.
    nonmembership_demon(E, _SetAttr, Receiver) :- var(E),
	receive_events(Receiver, Events),
	( foreach(AddedElement,Events), param(E) do
	    E #\= AddedElement
	).
    nonmembership_demon(E, SetAttr, Receiver) :- integer(E),
	kill_receiver_suspension(Receiver),
	intset_remove(E, SetAttr).
	

%----------------------------------------------------------------------
% Reified Membership
%----------------------------------------------------------------------

in(E, Set, 0) ?- !,
	E notin Set.
in(E, Set, 1) ?- !,
	E in Set.
in(E, Set0, Bool) :-
	integers([E]),
	solver_module:(Bool :: 0..1),
	seteval(Set0, Set),
	get_set_attribute(Set, SetAttr),
	membership_demon(E, SetAttr, Bool, _Susp).

    :- demon membership_demon/4.
    membership_demon(E, SetAttr, Bool, Susp) :- var(E),
	SetAttr = int_sets with value:Set,
	( var(Bool) ->
	    ( var(Susp) ->
		suspend(membership_demon(E, SetAttr, Bool, Susp), 3,
		    [Bool->inst,E->inst,Set->add,Set->rem], Susp)
	    ;
		true
	    )
	; Bool == 1 ->
	    kill_suspension(Susp),
	    E in Set
	; Bool == 0,
	    kill_suspension(Susp),
	    E notin Set
	).
    membership_demon(E, SetAttr, Bool, Susp) :- integer(E),
	( var(Bool) ->
	    ( lset_member(E, SetAttr) -> kill_suspension(Susp), Bool = 1
	    ; uset_nonmember(E, SetAttr) -> kill_suspension(Susp), Bool = 0
	    ; var(Susp) ->
		SetAttr = int_sets with value:Set,
	    	suspend(membership_demon(E, SetAttr, Bool, Susp), 3,
		    [Bool->inst,Set->add,Set->rem], Susp)
	    ; true
	    )
	; Bool == 1 ->
	    kill_suspension(Susp),
	    intset_add(E, SetAttr)
	; Bool == 0,
	    kill_suspension(Susp),
	    intset_remove(E, SetAttr)
	).


%----------------------------------------------------------------------
% Equality constraint
%----------------------------------------------------------------------

X sameset Y :-
	var(X), !,
	seteval(Y, X).
X sameset Y :-
	var(Y), !,
	seteval(X, Y).
X sameset Y :-
	seteval(X, S),
	seteval(Y, S).

%----------------------------------------------------------------------
% Lex less than constraint
%----------------------------------------------------------------------
X less Y :-
        seteval(X, S1),
        seteval(Y, S2),
        less_lex(S1, S2),
        less2(S1,S2,_Susp).

X leq Y :-
        seteval(X, S1),
        seteval(Y, S2),
        leq_lex(S1, S2),
        leq2(S1,S2,_Susp).


lex_set_range(SExpr,S,S):-
        nonvar(SExpr),
        !,
        seteval(SExpr,S).
lex_set_range(SExpr,Glb,Lub):-
        seteval(SExpr,S),
        get_set_attribute(S,Attr),
        lex_glb_lub(Attr,Glb,Lub).


non_caching_lex_glb_lub(Attr, Glb, Lub):-
        lset_uset_to_lists(Attr, Glb, Lub).

lex_glb_lub(Attr, Glb, Lub):-
        Attr = int_sets with [lex_dirty:1],  % Dirty==1 or var(Dirty)
        !,
        lset_uset_to_lists(Attr, Glb, Lub),
        % store calculated results
        setarg(lex_glb of int_sets, Attr, Glb),
        setarg(lex_lub of int_sets, Attr, Lub),
        setarg(lex_dirty of int_sets, Attr, 0). % clear dirty bit
lex_glb_lub(Attr, Glb, Lub):-
        Attr = int_sets with [lex_dirty:0,
                              lex_glb:Glb, lex_lub:Lub].


lex_glb(Attr, Glb):-
        lex_glb_lub(Attr, Glb, _Lub).
lex_lub(Attr, Lub):-
        lex_glb_lub(Attr, _Glb, Lub).


lex_min_max(SExpr, S, S):-
        nonvar(SExpr),
        !,
        seteval(SExpr,S).
lex_min_max(SExpr, Min, Max):-
        seteval(SExpr, S),
        get_set_attribute(S, Attr),
        attr_min_max(Attr, Min, Max).

attr_min_max(Attr, LexMin, LexMax):-
        Attr = int_sets with [lex_min:LexMin, lex_max:LexMax].
        
lex_min(Attr, LexMin):-
        attr_min_max(Attr, LexMin, _LexMax).
lex_max(Attr, LexMax):-
        attr_min_max(Attr, _LexMin, LexMax).

:-demon less2/3.
less2(S1,S2,Susp):-
        nonvar(S1),
        !,
        (var(Susp) -> true; kill_suspension(Susp)),
        less3(S1,S2).
less2(S1,S2,Susp):-
        % var(S1),
        nonvar(S2),
        !,
        (var(Susp) -> true; kill_suspension(Susp)),
        get_set_attribute(S1, Attr1),
        Attr1 = int_sets with [card:C],
        lex_max(Attr1,LexMax),
        (lex_set:lex_less(LexMax,S2) ->
             % nothing to do
             true
        ;
             lex_glb_lub(Attr1, Glb, Lub),
             get_bounds(C,CMin,CMax),
             once(lex_set:lex_prev(S2,Glb,Lub,CMin,CMax,NewCMax,
                                   NewLexMax)),
             set_lex_max(Attr1, NewCMax, NewLexMax),
             wake
        ),
        true.
less2(S1,S2,Susp):-
        % var(S1),
        % var(S2),
        !,
        get_set_attribute(S1, Attr1),
        get_set_attribute(S2, Attr2),
        Attr1 = int_sets with [card:C1],
        attr_min_max(Attr1, LexMin1, LexMax1),
        Attr2 = int_sets with [card:C2],
        attr_min_max(Attr2, LexMin2, LexMax2),
        (var(Susp) ->
            suspend(less2(S1,S2,Susp),6,
                    [S1->min_susp, S1->max_susp, S1->inst,
                     S2->min_susp,S2->max_susp, S2->inst,
                     C1->min, C1->max, C2->min, C2->max],
                    Susp)
        ;
            true
        ),
        % process upperbounds
        (lex_set:lex_less(LexMax1,LexMax2) ->
             % nothing to do
             true
        ;
             lex_glb_lub(Attr1, Glb1, Lub1),
             get_bounds(C1,CMin1,CMax1),
             once(lex_set:lex_prev(LexMax2,Glb1,Lub1,CMin1,CMax1,NewCMax1,
                                   NewLexMax1)),
             set_lex_max(Attr1, NewCMax1, NewLexMax1)
        ),
        (lex_set:lex_less(LexMin1,LexMin2) ->
            attr_min_max(Attr1, _, CurrentLexMax1),
            (lex_set:lex_less(CurrentLexMax1, LexMin2) ->
                % constraint is entailed
                kill_suspension(Susp)
            ;
                % nothing to do
                true
            )
        ;
             lex_glb_lub(Attr2, Glb2, Lub2),
             get_bounds(C2,CMin2,CMax2),
             once(lex_set:lex_next(LexMin1,Glb2,Lub2,CMin2,CMax2,NewCMin2,NewLexMin2)),
             set_lex_min(Attr2, NewCMin2, NewLexMin2)
        ),
        wake.

less3(S1,S2):-
        % nonvar(S1),
        nonvar(S2),
        !,
        lex_set:lex_less(S1,S2).
less3(S1,S2):-
        % nonvar(S1),
        % var(S2),
        get_set_attribute(S2, Attr2),
        Attr2 = int_sets with [card:C],
        lex_min(Attr2,LexMin),
        (lex_set:lex_less(S1,LexMin) ->
             % nothing to do
             true
        ;
             lex_glb_lub(Attr2, Glb, Lub),
             get_bounds(C,CMin,CMax),
             once(lex_set:lex_next(S1,Glb,Lub,CMin,CMax,NewCMin,
                                   NewLexMin)),
             set_lex_min(Attr2, NewCMin, NewLexMin),
             wake
        ),
        true.

:-demon leq2/3.
leq2(S1,S2,Susp):-
        nonvar(S1),
        !,
        (var(Susp) -> true; kill_suspension(Susp)),
        leq3(S1,S2).
leq2(S1,S2,Susp):-
        % var(S1),
        nonvar(S2),
        !,
        (var(Susp) -> true; kill_suspension(Susp)),
        get_set_attribute(S1, Attr1),
        Attr1 = int_sets with [card:C],
        lex_max(Attr1,LexMax),
        (lex_set:lex_leq(LexMax,S2) ->
             % nothing to do
             true
        ;
             lex_glb_lub(Attr1, Glb, Lub),
             get_bounds(C,CMin,CMax),
             once(lex_set:lex_prev_eq(S2,Glb,Lub,CMin,CMax,NewCMax,
                                      NewLexMax)),
             set_lex_max(Attr1, NewCMax, NewLexMax),
             wake
        ),
        true.
leq2(S1,S2,Susp):-
        % var(S1),
        % var(S2),
        !,
        get_set_attribute(S1, Attr1),
        get_set_attribute(S2, Attr2),
        Attr1 = int_sets with [card:C1],
        attr_min_max(Attr1, LexMin1, LexMax1),
        Attr2 = int_sets with [card:C2],
        attr_min_max(Attr2, LexMin2, LexMax2),
        (var(Susp) ->
            suspend(leq2(S1,S2,Susp),6,
                    [S1->min_susp, S1->max_susp, S1->inst,
                     S2->min_susp,S2->max_susp, S2->inst,
                     C1->min, C1->max, C2->min, C2->max],
                    Susp)
        ;
            true
        ),
        % process upperbounds
        (lex_set:lex_leq(LexMax1,LexMax2) ->
             % nothing to do
             true
        ;
             lex_glb_lub(Attr1, Glb1, Lub1),
             get_bounds(C1,CMin1,CMax1),
             once(lex_set:lex_prev_eq(LexMax2,Glb1,Lub1,CMin1,CMax1,NewCMax1,
                                      NewLexMax1)),
             set_lex_max(Attr1, NewCMax1, NewLexMax1)
        ),
        (lex_set:lex_leq(LexMin1,LexMin2) ->
            attr_min_max(Attr1, _, CurrentLexMax1),
            (lex_set:lex_leq(CurrentLexMax1, LexMin2) ->
                % constraint is entailed
                kill_suspension(Susp)
            ;
                % nothing to do
                true
            )
        ;
             lex_glb_lub(Attr2, Glb2, Lub2),
             get_bounds(C2,CMin2,CMax2),
             once(lex_set:lex_next_eq(LexMin1,Glb2,Lub2,CMin2,CMax2,NewCMin2,NewLexMin2)),
             (set_lex_min(Attr2, NewCMin2, NewLexMin2) ->
                 true
             ;
                 %writeln(failing),
                 %writeln(leq2(S1,S2,Susp)),
                 %writeln(set_lex_min(Attr2, NewCMin2, NewLexMin2)),
                 fail
             )
        ),
        wake.

leq3(S1,S2):-
        % nonvar(S1),
        nonvar(S2),
        !,
        lex_set:lex_leq(S1,S2).
leq3(S1,S2):-
        % nonvar(S1),
        % var(S2),
        get_set_attribute(S2, Attr2),
        Attr2 = int_sets with [card:C],
        lex_min(Attr2,LexMin),
        (lex_set:lex_leq(S1,LexMin) ->
             % nothing to do
             true
        ;
             lex_glb_lub(Attr2, Glb, Lub),
             get_bounds(C,CMin,CMax),
             once(lex_set:lex_next_eq(S1,Glb,Lub,CMin,CMax,NewCMin,
                                      NewLexMin)),
             set_lex_min(Attr2, NewCMin, NewLexMin),
             wake
        ),
        true.




%
% prev_lex_max(+Attr,-NewLexMax,-NewCMax)
%
% returns the next smallest max lex bound for S, subsequent solutions will
% return the progressively smaller bounds
%
prev_lex_max(Attr,NewLexMax,NewCMax):-
        Attr = int_sets with [card:C],
        lex_max(Attr,LexMax),
        lex_glb_lub(Attr, Glb, Lub),
        get_bounds(C,CMin,CMax),
        lex_set:lex_prev(LexMax,Glb,Lub,CMin,CMax,NewCMax,
                         NewLexMax).

prev_lex_max(Attr,NewLexMax,NewCMax,Goal):-
        Attr = int_sets with [card:C],
        lex_max(Attr,LexMax),
        lex_glb_lub(Attr, Glb, Lub),
        get_bounds(C,CMin,CMax),
        lex_set:lex_prev(LexMax,Glb,Lub,CMin,CMax,NewCMax,
                         NewLexMax,Goal).

%
% next_lex_min(+Attr,-NewLexMin,-NewCMin)
%
% returns the next largest min lex bound for S, subsequent solutions will
% return the progressively larger bounds
%
next_lex_min(Attr,NewLexMin,NewCMin):-
        Attr = int_sets with [card:C],
        lex_min(Attr,LexMin),
        lex_glb_lub(Attr, Glb, Lub),
        get_bounds(C,CMin,CMax),
        lex_set:lex_next(LexMin,Glb,Lub,CMin,CMax,NewCMin,
                         NewLexMin).

next_lex_min(Attr,NewLexMin,NewCMin,Goal):-
        Attr = int_sets with [card:C],
        lex_min(Attr,LexMin),
        lex_glb_lub(Attr, Glb, Lub),
        get_bounds(C,CMin,CMax),
        lex_set:lex_next(LexMin,Glb,Lub,CMin,CMax,NewCMin,
                         NewLexMin,Goal).




%----------------------------------------------------------------------
% Disjointness constraints
%----------------------------------------------------------------------

X0 disjoint Y0 :-
	seteval(X0, X), get_set_attribute(X, XAttr),
	seteval(Y0, Y), get_set_attribute(Y, YAttr),

	suspend(remove_demon(YAttr, Events1), 3, X->add, Susp1),
	suspend(remove_demon(XAttr, Events2), 3, Y->add, Susp2),

	initial_member_events(XAttr, Susp1, Events1),
	initial_member_events(YAttr, Susp2, Events2),
	schedule_suspensions(1,s([Susp1,Susp2])), wake,

	verify check_instantiation((X disjoint Y)).


all_disjoint(List) :-
	(
	    foreach(X, List),
	    foreach(Susp, Susps),
	    fromto(_, [XAttr|Tail], Tail, []),
	    fromto([], Head, [XAttr|Head], _)
	do
	    get_set_attribute(X, XAttr),
	    suspend(all_disjoint_demon(Head, Tail, AddEvents), 3, X->add, Susp),
	    initial_member_events(XAttr, Susp, AddEvents)
	),
	% don't schedule until all demons have been set up!
	schedule_suspensions(1,s(Susps)), wake.

    :- demon all_disjoint_demon/3.
    all_disjoint_demon(HAttrs, TAttrs, Receiver) :-
	foreachevent(all_disjoint, Element, [HAttrs,TAttrs], Receiver, (
	    ( foreach(Attr,HAttrs), param(Element) do intset_remove(Element,Attr) ),
	    ( foreach(Attr,TAttrs), param(Element) do intset_remove(Element,Attr) )
	)).


% Improvement:
% use cardinality information (if present) to check if there are
% enough upb elements left to satisfy the constraint.


%----------------------------------------------------------------------
% Inclusion/subset constraint
%----------------------------------------------------------------------

X0 includes Y0 :-
	Y0 subset X0.

X0 subset Y0 :-
	X0 subsetof Y0.

X0 subsetof Y0 :-
	seteval(Y0, Y), get_set_attribute(Y, YAttr),
	seteval(X0, X), get_new_set_attribute(X, [YAttr], XAttr),

	suspend(add_demon(YAttr, Events1), 3, X->add, Susp1),
	suspend(remove_demon(XAttr, Events2), 3, Y->rem, Susp2),

	initial_member_events(XAttr, Susp1, Events1),
	initial_nonmember_events(YAttr, XAttr, Susp2, Events2),
	schedule_suspensions(1,s([Susp1,Susp2])), wake,

	verify check_instantiation((X subset Y)).

:- demon add_demon/2.
add_demon(XAttr, Receiver) :-
	foreachevent(add, Element, [XAttr], Receiver, (
	    intset_add(Element, XAttr)
	)).

:- demon remove_demon/2.
remove_demon(XAttr, Receiver) :-
	foreachevent(remove, Element, [XAttr], Receiver, (
	    intset_remove(Element, XAttr)
	)).


%----------------------------------------------------------------------
% Unification handler
%----------------------------------------------------------------------

unify_sets(_, YAttr) :- var(YAttr).
unify_sets(X, YAttr) :- nonvar(YAttr),
	unify_any_set(X, YAttr).

unify_any_set(_X{int_sets:XAttr}, YSetAttr) ?-
	unify_meta_set(XAttr, YSetAttr).
unify_any_set(X, YSetAttr) :- nonvar(X),
	call_priority((
		% complete the bounds and make them equal
	    intset_union_lwb(X, YSetAttr),
	    intset_intersect_upb(X, YSetAttr),
		% wake both bounds-lists, even if one bound was not touched.
		% this gives the goals a chance to kill themselves
	    close_sender(added of int_sets, YSetAttr),
	    close_sender(removed of int_sets, YSetAttr),
	    schedule_suspensions(add of int_sets, YSetAttr),
	    schedule_suspensions(rem of int_sets, YSetAttr)
	), 2).

unify_meta_set(XAttr, YSetAttr) :- var(XAttr), XAttr=YSetAttr.
unify_meta_set(XSetAttr, YSetAttr) :- nonvar(XSetAttr),
	XSetAttr = int_sets with [card:Card,value:X],
	YSetAttr = int_sets with [card:Card],

	% make the lwbs and upbs identical and wake the necessary
	% suspensions on X and Y
	lset_to_list(XSetAttr, XLwbList),
	lset_to_list(YSetAttr, YLwbList),
	uset_to_list(XSetAttr, XUpbList),
	uset_to_list(YSetAttr, YUpbList),
	call_priority((
	    intset_union_lwb(YLwbList, XSetAttr),	% make the lwbs equal
	    intset_union_lwb(XLwbList, YSetAttr),
	    intset_intersect_upb(YUpbList, XSetAttr),	% make the upbs equal
	    intset_intersect_upb(XUpbList, YSetAttr),

	    % now transfer Y's lists to X
	    % this is not absolutely necessary because we keep the attributes
	    % consistent, but other parts of the system may get confused.
	    merge_suspension_lists(add of int_sets, YSetAttr, add of int_sets, XSetAttr),
	    merge_suspension_lists(rem of int_sets, YSetAttr, rem of int_sets, XSetAttr),

	    % because existing propagators may have YSetAttr in their
	    % arguments, we cannot just forget about it, we keep it in
	    % sync with XSetAttr and vice versa. The priority should be
	    % higher than the propagator priority.
	    init_event_receiver(added of int_sets, XSetAttr, Susp1, XAddEvents),
	    init_event_receiver(removed of int_sets, XSetAttr, Susp1, XRemEvents),
	    suspend(sync_attributes(YSetAttr, XAddEvents, XRemEvents),
	    	2, [X->add,X->rem], Susp1),
	    add_attribute(DummyY, YSetAttr, int_sets),
	    init_event_receiver(added of int_sets, YSetAttr, Susp1, YAddEvents),
	    init_event_receiver(removed of int_sets, YSetAttr, Susp1, YRemEvents),
	    suspend(sync_attributes(XSetAttr, YAddEvents, YRemEvents),
	    	2, [DummyY->add,DummyY->rem], Susp2)

	), 2).



:- demon sync_attributes/3.
sync_attributes(ShadowAttr, AddReceiver, RemReceiver) :-
	receive_events(AddReceiver, AddEvents),
	receive_events(RemReceiver, RemEvents),
	call_priority((
	    ( foreach(Element,AddEvents), param(ShadowAttr) do
	    	intset_add(Element, ShadowAttr)
	    ),
	    ( foreach(Element,RemEvents), param(ShadowAttr) do
		intset_remove(Element, ShadowAttr)
	    )
	), 2).


%----------------------------------------------------------------------
% Unifiability handler
%----------------------------------------------------------------------

test_unify_sets(_, YAttr) :- var(YAttr).
test_unify_sets(X, YAttr) :- nonvar(YAttr),
	test_unify_any_set(X, YAttr).

test_unify_any_set(_X{int_sets:XAttr}, YSetAttr) ?-
	test_unify_meta_set(XAttr, YSetAttr).
test_unify_any_set(X, YSetAttr) :- nonvar(X),
	YSetAttr = int_sets with [card:YCard],
	lset_to_list(YSetAttr, YLwbList),
	lex_subseteq(YLwbList, X),
	uset_to_list(YSetAttr, YUpbList),
	lex_subseteq(X, YUpbList),
	length(X, XCard),
	not not_unify(XCard, YCard).

test_unify_meta_set(XSetAttr, _YSetAttr) :- var(XSetAttr).
test_unify_meta_set(XSetAttr, YSetAttr) :- nonvar(XSetAttr),
	XSetAttr = int_sets with [card:XCard],
	YSetAttr = int_sets with [card:YCard],
	not not_unify(XCard, YCard),
	lset_to_list(XSetAttr, XLwbList),
	uset_to_list(YSetAttr, YUpbList),
	lex_subseteq(XLwbList, YUpbList),
	lset_to_list(YSetAttr, YLwbList),
	uset_to_list(XSetAttr, XUpbList),
	lex_subseteq(YLwbList, XUpbList).


%----------------------------------------------------------------------
% Compare_instances handler
%----------------------------------------------------------------------

	% Precondition: one or both of the terms are attributed variables,
	% (not necessarily with a set attribute).
compare_instances_set(Res, _X{int_sets:XAttr}, Y) ?-
	compare_instances_attr_any(Res, XAttr, Y).
compare_instances_set(Res, X, _Y{int_sets:YAttr}) ?- nonvar(X),
	test_unify_sets(X, YAttr),	% may fail (not instance)
	Res = (<).
compare_instances_set(Res, X, _Y{int_sets:YAttr}) ?- free(X),
	( var(YAttr) -> Res = (=) ; Res = (>) ).

    compare_instances_attr_any(Res, XAttr, _Y{int_sets:YAttr}) ?-
    	compare_instances_attr_attr(Res, XAttr, YAttr).
    compare_instances_attr_any(Res, XAttr, Y) :- free(Y),
	( var(XAttr) -> Res = (=) ; Res = (<) ).
    compare_instances_attr_any(Res, XAttr, Y) :- nonvar(Y),
	test_unify_sets(Y, XAttr),	% may fail (not instance)
	Res = (>).

    compare_instances_attr_attr(Res, XSetAttr, YSetAttr) :- var(XSetAttr), !,
	( var(YSetAttr) -> Res = (=) ; Res = (>) ).
    compare_instances_attr_attr(Res, XSetAttr, YSetAttr) :-
	( var(YSetAttr) -> Res = (<) ; attr_compare(Res, XSetAttr, YSetAttr) ).

    attr_compare(Res, int_sets{dom:XMap,off:XOff,card:XCard},
		  int_sets{dom:YMap,off:YOff,card:YCard}) ?-
	% 1..S..T..Arity
	arity(XMap, XA),
	arity(YMap, YA),
	writeln(XMap-YMap),
	XS is max(0,YOff-XOff),
	YS is max(0,XOff-YOff),
	XT is min(XA,YOff+YA-XOff),
	YT is min(YA,XOff+XA-YOff),
	( for(I,1,XS), param(XMap,Res) do	% all Ys 0
	    arg(I, XMap, Bit), ( var(Bit) -> Res=(>) ; Bit == 0 )
	),
	( for(I,XT+1,XA), param(XMap,Res) do	% all Ys 0
	    arg(I, XMap, Bit), ( var(Bit) -> Res=(>) ; Bit == 0 )
	),
	( for(I,XS+1,XT), for(J,YS+1,YT), param(XMap,YMap,Res) do
	    arg(I, XMap, XBit), arg(J, YMap, YBit),
	    ( XBit==YBit -> true
	    ; var(XBit) -> ( var(YBit) -> true ; Res=(>) )
	    ; Res=(<)
	    )
	),
	( for(I,1,YS), param(YMap,Res) do	% all Xs 0
	    arg(I, YMap, Bit), ( var(Bit) -> Res=(<) ; Bit == 0 )
	),
	( for(I,YT+1,YA), param(YMap,Res) do	% all Xs 0
	    arg(I, YMap, Bit), ( var(Bit) -> Res=(<) ; Bit == 0 )
	),
	% var(Res) iff result so far is =
	compare_instances(Res, XCard, YCard).


%----------------------------------------------------------------------
% Other handlers
%----------------------------------------------------------------------

suspensions_set(_{int_sets:Attr}, Susps, Susps0) ?-
	( var(Attr) ->
	    Susps=Susps0
	;
	    Attr = int_sets with [add:Add, rem:Rem],
	    Susps = [Add,Rem|Susps0]
	).


delayed_goals_number_set(_{int_sets:(int_sets with [add:Add,rem:Rem])}, N) ?- !,
	count_active_suspensions(Add, 0, N1),
	count_active_suspensions(Rem, N1, N).
delayed_goals_number_set(_, 0).
	
    count_active_suspensions([Susp|Susps], N0, N) ?- !,
	( is_suspension(Susp) -> N1 is N0 + 1 ; N1 = N0 ),
	count_active_suspensions(Susps, N1, N).
    count_active_suspensions(_, N, N).


copy_term_set(_{int_sets:(int_sets with [dom:Dom,off:Off,lcard:LC,ucard:UC,card:Card])}, Copy) ?- !,
	copy_term(Dom-Card, DomCopy-CardCopy),
	Attr = int_sets with [dom:DomCopy,off:Off,lcard:LC,ucard:UC,
				card:CardCopy,value:Copy],
	open_sender(added of int_sets, Attr),
	open_sender(removed of int_sets, Attr),
	init_suspension_list(add of int_sets, Attr),
	init_suspension_list(rem of int_sets, Attr),
	init_suspension_list(min_susp of int_sets, Attr),
	init_suspension_list(max_susp of int_sets, Attr),
	add_attribute(Copy, Attr, int_sets).
copy_term_set(_, _).


%----------------------------------------------------------------------
% Intersection constraint
%
% xyz action	propagator
% 11_ -> 111	1,3
% 1_0 -> 100	1,6
% 0__ -> 0_0	2
% _10 -> 010	3,6
% _0_ -> _00	4
% __1 -> 111	5
%----------------------------------------------------------------------

intersection(X0, Y0, Z0) :-
	seteval(X0, X), get_set_attribute(X, XAttr),
	seteval(Y0, Y), get_set_attribute(Y, YAttr),
	seteval(Z0, Z), get_new_set_attribute(Z, [XAttr,YAttr], ZAttr),

	% first make all the propagators, even if the input set is ground.
	% this is necessary to do the initial propagation
	suspend(intersect_add_xy_demon(YAttr, ZAttr, Events1), 4, X->add, Susp1),
	suspend(remove_demon(ZAttr, Events2), 3, X->rem, Susp2),
	suspend(intersect_add_xy_demon(XAttr, ZAttr, Events3), 4, Y->add, Susp3),
	suspend(remove_demon(ZAttr, Events4), 3, Y->rem, Susp4),
	suspend(add2_demon(XAttr, YAttr, Events5), 3, Z->add, Susp5),
	suspend(intersect_rem_z_demon(XAttr, YAttr, Events6), 4, Z->rem, Susp6),

	% establish initial consistency
	initial_member_events(XAttr, Susp1, Events1),
	initial_nonmember_events(XAttr, ZAttr, Susp2, Events2),
	initial_member_events(YAttr, Susp3, Events3),
	initial_nonmember_events(YAttr, ZAttr, Susp4, Events4),
	initial_member_events(ZAttr, Susp5, Events5),
	initial_nonmember_events(ZAttr, XAttr, YAttr, Susp6, Events6),
	schedule_suspensions(1,s([Susp1,Susp2,Susp3,Susp4,Susp5,Susp6])), wake,

	verify check_instantiation(intersection(X, Y, Z)).


:- demon intersect_add_xy_demon/3.
intersect_add_xy_demon(XYAttr, ZAttr, Receiver) :-
	foreachevent(intersect_add_xy, Element, [XYAttr,ZAttr], Receiver, (
	    ( uset_nonmember(Element, ZAttr) -> intset_remove(Element, XYAttr)
	    ; lset_member(Element, XYAttr) -> intset_add(Element, ZAttr)
	    ; true )
	)).

:- demon intersect_rem_z_demon/3.
intersect_rem_z_demon(XAttr, YAttr, Receiver) :-
	foreachevent(intersect_rem_z, Element, [XAttr,YAttr], Receiver, (
	    ( lset_member(Element, XAttr) -> intset_remove(Element, YAttr)
	    ; lset_member(Element, YAttr) -> intset_remove(Element, XAttr)
	    ; true )
	)).

:- demon add2_demon/3.
add2_demon(XAttr, YAttr, Receiver) :-
	foreachevent(add2, Element, [XAttr,YAttr], Receiver, (
	    intset_add(Element, XAttr),
	    intset_add(Element, YAttr)
	)).



%----------------------------------------------------------------------
% Union constraint
%
% xyz action	propagator
% 1__ -> 1_1	1
% 0_1 -> 011	2,5
% 00_ -> 000	2,4
% _1_ -> _11	3
% _01 -> 101	4,5
% __0 -> 000	6
%----------------------------------------------------------------------

union(X0, Y0, Z0) :-
	seteval(X0, X), get_set_attribute(X, XAttr),
	seteval(Y0, Y), get_set_attribute(Y, YAttr),
	seteval(Z0, Z), get_new_set_attribute(Z, [XAttr,YAttr], ZAttr),

	suspend(add_demon(ZAttr, Events1), 3, X->add, Susp1),
	suspend(union_rem_xy_demon(YAttr, ZAttr, Events2), 4, X->rem, Susp2),
	suspend(add_demon(ZAttr, Events3), 3, Y->add, Susp3),
	suspend(union_rem_xy_demon(XAttr, ZAttr, Events4), 4, Y->rem, Susp4),
	suspend(union_add_z_demon(XAttr, YAttr, Events5), 4, Z->add, Susp5),
	suspend(remove2_demon(XAttr, YAttr, Events6), 3, Z->rem, Susp6),

	initial_member_events(XAttr, Susp1, Events1),
	initial_nonmember_events(XAttr, YAttr, ZAttr, Susp2, Events2),
	initial_member_events(YAttr, Susp3, Events3),
	initial_nonmember_events(YAttr, XAttr, ZAttr, Susp4, Events4),
	initial_member_events(ZAttr, Susp5, Events5),
	initial_nonmember_events(ZAttr, XAttr, YAttr, Susp6, Events6),
	schedule_suspensions(1,s([Susp1,Susp2,Susp3,Susp4,Susp5,Susp6])), wake,

	verify check_instantiation(union(X, Y, Z)).


:- demon union_rem_xy_demon/3.
union_rem_xy_demon(XYAttr, ZAttr, Receiver) :-
	foreachevent(union_rem_xy, Element, [XYAttr,ZAttr], Receiver, (
	    ( uset_nonmember(Element, XYAttr) -> intset_remove(Element, ZAttr)
	    ; lset_member(Element, ZAttr) -> intset_add(Element, XYAttr)
	    ; true )
	)).

:- demon union_add_z_demon/3.
union_add_z_demon(XAttr, YAttr, Receiver) :-
	foreachevent(union_add_z, Element, [XAttr,YAttr], Receiver, (
	    ( uset_nonmember(Element, XAttr) -> intset_add(Element, YAttr)
	    ; uset_nonmember(Element, YAttr) -> intset_add(Element, XAttr)
	    ; true )
	)).

:- demon remove2_demon/3.
remove2_demon(XAttr, YAttr, Receiver) :-
	foreachevent(remove2, Element, [XAttr,YAttr], Receiver, (
	    intset_remove(Element, XAttr),
	    intset_remove(Element, YAttr)
	)).



all_union([], Z) :- !, Z=[].
all_union([X], Z) :- !, Z=X.
all_union(Xs, Z) :-
	halve(Xs, Xs1, Xs2),
	all_union(Xs1, Z1),
	all_union(Xs2, Z2),
	union(Z1, Z2, Z).

all_union_lex([], Z) :- !, Z=[].
all_union_lex([X], Z) :- !, Z=X.
all_union_lex(Xs, Z) :-
	halve(Xs, Xs1, Xs2),
	all_union_lex(Xs1, Z1),
	all_union_lex(Xs2, Z2),
	union_lex(Z1, Z2, Z).


all_intersection([], Z) :- !,
	error(6, all_intersection([], Z)).
all_intersection([X], Z) :- !, Z=X.
all_intersection(Xs, Z) :-
	halve(Xs, Xs1, Xs2),
	all_intersection(Xs1, Z1),
	all_intersection(Xs2, Z2),
	intersection(Z1, Z2, Z).


%----------------------------------------------------------------------
% Symmetric difference constraint
%
% xyz action	propagator
% 10_ -> 101	1,4
% 11_ -> 110	1,3
% 1_0 -> 110	1,6
% 1_1 -> 101	1,5
% 00_ -> 000	2,4
% 01_ -> 011	2,3
% 0_0 -> 000	2,6
% 0_1 -> 011	2,5
% same for y and z trigger
%----------------------------------------------------------------------

symdiff(X0, Y0, Z0) :-
	seteval(X0, X), get_set_attribute(X, XAttr),
	seteval(Y0, Y), get_set_attribute(Y, YAttr),
	seteval(Z0, Z), get_new_set_attribute(Z, [XAttr,YAttr], ZAttr),

	suspend(symdiff_add_xyz_demon(YAttr, ZAttr, Events1), 4, X->add, Susp1),
	suspend(symdiff_rem_xyz_demon(YAttr, ZAttr, Events2), 4, X->rem, Susp2),
	suspend(symdiff_add_xyz_demon(XAttr, ZAttr, Events3), 4, Y->add, Susp3),
	suspend(symdiff_rem_xyz_demon(XAttr, ZAttr, Events4), 4, Y->rem, Susp4),
	suspend(symdiff_add_xyz_demon(XAttr, YAttr, Events5), 4, Z->add, Susp5),
	suspend(symdiff_rem_xyz_demon(XAttr, YAttr, Events6), 4, Z->rem, Susp6),

	initial_member_events(XAttr, Susp1, Events1),
	initial_nonmember_events(XAttr, YAttr, ZAttr, Susp2, Events2),
	initial_member_events(YAttr, Susp3, Events3),
	initial_nonmember_events(YAttr, XAttr, ZAttr, Susp4, Events4),
	initial_member_events(ZAttr, Susp5, Events5),
	initial_nonmember_events(ZAttr, XAttr, YAttr, Susp6, Events6),
	schedule_suspensions(1,s([Susp1,Susp2,Susp3,Susp4,Susp5,Susp6])), wake,

	verify check_instantiation(symdiff(X, Y, Z)).

:- demon symdiff_rem_xyz_demon/3.
symdiff_rem_xyz_demon(XYAttr, ZAttr, Receiver) :-
	foreachevent(symdiff_rem_xyz, Element, [XYAttr,ZAttr], Receiver, (
	    ( lset_member(Element, XYAttr) -> intset_add(Element, ZAttr)
	    ; uset_nonmember(Element, XYAttr) -> intset_remove(Element, ZAttr)
	    ; lset_member(Element, ZAttr) -> intset_add(Element, XYAttr)
	    ; uset_nonmember(Element, ZAttr) -> intset_remove(Element, XYAttr)
	    ; true
	    )
	)).

:- demon symdiff_add_xyz_demon/3.
symdiff_add_xyz_demon(XYAttr, ZAttr, Receiver) :-
	foreachevent(symdiff_add_xyz, Element, [XYAttr,ZAttr], Receiver, (
	    ( lset_member(Element, XYAttr) -> intset_remove(Element, ZAttr)
	    ; uset_nonmember(Element, XYAttr) -> intset_add(Element, ZAttr)
	    ; lset_member(Element, ZAttr) -> intset_remove(Element, XYAttr)
	    ; uset_nonmember(Element, ZAttr) -> intset_add(Element, XYAttr)
	    ; true
	    )
	)).


%----------------------------------------------------------------------
% Difference constraint
%
% xyz action	propagator
% 0__ -> 0_0	2
% 10_ -> 101	1,4
% _00 -> 000	4,6
% __1 -> 101	5
% _1_ -> _10	3
% 1_0 -> 110	1,6
%----------------------------------------------------------------------

difference(X0, Y0, Z0) :-
	seteval(X0, X), get_set_attribute(X, XAttr),
	seteval(Y0, Y), get_set_attribute(Y, YAttr),
	seteval(Z0, Z), get_new_set_attribute(Z, [XAttr,YAttr], ZAttr),

	suspend(difference_add_x_demon(YAttr, ZAttr, Events1), 4, X->add, Susp1),
	suspend(remove_demon(ZAttr, Events2), 3, X->rem, Susp2),
	suspend(remove_demon(ZAttr, Events3), 3, Y->add, Susp3),
	suspend(difference_rem_y_demon(XAttr, ZAttr, Events4), 4, Y->rem, Susp4),
	suspend(difference_add_z_demon(XAttr, YAttr, Events5), 4, Z->add, Susp5),
	suspend(difference_rem_z_demon(XAttr, YAttr, Events6), 4, Z->rem, Susp6),

	initial_member_events(XAttr, Susp1, Events1),
	initial_nonmember_events(XAttr, YAttr, ZAttr, Susp2, Events2),
	initial_member_events(YAttr, Susp3, Events3),
	initial_nonmember_events(YAttr, XAttr, ZAttr, Susp4, Events4),
	initial_member_events(ZAttr, Susp5, Events5),
	initial_nonmember_events(ZAttr, XAttr, YAttr, Susp6, Events6),
	schedule_suspensions(1,s([Susp1,Susp2,Susp3,Susp4,Susp5,Susp6])), wake,
	verify check_instantiation(difference(X, Y, Z)).

:- demon difference_add_x_demon/3.
difference_add_x_demon(YAttr, ZAttr, Receiver) :-
	foreachevent(difference_add_x, Element, [YAttr,ZAttr], Receiver, (
	    ( uset_nonmember(Element, YAttr) -> intset_add(Element, ZAttr)
	    ; uset_nonmember(Element, ZAttr) -> intset_add(Element, YAttr)
	    ; true )
	)).

:- demon difference_rem_y_demon/3.
difference_rem_y_demon(XAttr, ZAttr, Receiver) :-
	foreachevent(difference_rem_y, Element, [XAttr,ZAttr], Receiver, (
	    ( lset_member(Element, XAttr) -> intset_add(Element, ZAttr)
	    ; uset_nonmember(Element, ZAttr) -> intset_remove(Element, XAttr)
	    ; true )
	)).

:- demon difference_add_z_demon/3.
difference_add_z_demon(XAttr, YAttr, Receiver) :-
	foreachevent(difference_add_z, Element, [XAttr,YAttr], Receiver, (
	    intset_add(Element, XAttr),
	    intset_remove(Element, YAttr)
	)).

:- demon difference_rem_z_demon/3.
difference_rem_z_demon(XAttr, YAttr, Receiver) :-
	foreachevent(difference_rem_z, Element, [XAttr,YAttr], Receiver, (
	    ( lset_member(Element, XAttr) -> intset_add(Element, YAttr)
	    ; uset_nonmember(Element, YAttr) -> intset_remove(Element, XAttr)
	    ; true )
	)).


%----------------------------------------------------------------------
% Cardinality constraint
%----------------------------------------------------------------------

#(Set0, C) :-
	seteval(Set0, Set),
        card_aux(Set,C).
card_aux(Set,C):-
        nonvar(Set),!,
        length(Set,C).
card_aux(Set,C):-        
        get_set_attribute(Set, SetAttr),
	SetAttr = int_sets with [card:C],
	( var(C) ->
	    suspend(card_demon(SetAttr), 3, [C->inst]),
            recalc_lex_bounds(SetAttr),
            suspend(recalc_lex_bounds(SetAttr,Susp), 4,
                    [C->min, C->max], Susp)
	;
	    card_demon(SetAttr),
            recalc_lex_bounds(SetAttr)
	).

remove_larger(_Attr, _Largest, []):-!.
remove_larger(_Attr, Largest, [E|_Rest]):-
        E =< Largest,!.
remove_larger(Attr, Largest, [E|Rest]):-
        intset_remove(E,Attr),
        remove_larger(Attr, Largest, Rest).

old_set_lex_min(Attr, NewCMin, NewLexMin):-
	Attr = int_sets with [dom:Map, off:Offset,
                              lex_min:LexMin],
        ( lex_set:lex_less(LexMin,NewLexMin) ->
              setarg(lex_min of int_sets, Attr, NewLexMin),
              schedule_suspensions(min_susp of int_sets, Attr),
              Attr = int_sets with [lex_max:LexMax],
              lex_set:lex_common_prefix(NewLexMin,LexMax,Common),
              lex_glb(Attr,OldGlb),
              (lex_set:lex_subtract(OldGlb,Common,NewElements) ->
                   intset_union_lwb(NewElements, Attr), % Common subset S
                   lex_glb(Attr,Glb)
              ;
                   Glb = OldGlb,
                   true
              ),
              (LexMin==LexMax ->
                  % ground the set
                  Attr = int_sets with [card:C, lcard:LCard],
                  get_bounds(C,_CMin,CMax),
                  ImposeCMin is length(LexMin)
              ;
                  % TODO: ensure that the cardinality is strictly larger
                  % than the number of elemnts in the GLB if the min lex
                  % bound is not equal to the GLB
                  Attr = int_sets with [card:C, lcard:LCard],
                  get_bounds(C,CMin,CMax),
                  ((CMin =< LCard, NewLexMin \== Glb) ->
                      ImposeCMin is LCard+1
                  ;
                      ImposeCMin = CMin
                  )
              )
        ;
              Attr = int_sets with [card:C, lcard:LCard],
              get_bounds(C,_CMin,CMax),
              LexMin == NewLexMin,
              ImposeCMin = NewCMin
        ),
        % remove minimal elements from lub if possible
        remove_minimal_lub(Attr, CMax, LCard, NewLexMin, Map, Offset),
        Attr = int_sets with [value:S],
        (var(S) ->
            		%%% may wake
            impose_min(C, ImposeCMin),
            ( C == LCard ->
                C = LCard   % this will ground the set
            ;
                Attr = int_sets with [value:S],
                notify_constrained(S)
            )
        ;
            % set became ground
            true
        ).

set_lex_min(Attr, _NewCMin, NewLexMin):-
	Attr = int_sets with [dom:Map, off:Offset,
                              lex_min:LexMin],

        %incval(set_lex_min_count),
        ( lex_set:lex_less(LexMin,NewLexMin) ->
              setarg(lex_min of int_sets, Attr, NewLexMin),
              schedule_suspensions(min_susp of int_sets, Attr),
              Attr = int_sets with [lex_max:LexMax,value:S],
              lex_set:lex_common_prefix(NewLexMin,LexMax,Common),
              lex_glb(Attr,OldGlb),
              (lex_set:lex_subtract(OldGlb,Common,NewElements) ->
                  % Common subset S
                  intset_union_lwb(NewElements, Attr), % may ground S
                  lex_glb(Attr,Glb)
              ;
                  Glb = OldGlb,
                  true
              ),
              (NewLexMin==LexMax ->
                  % ground the set to LexMin
                  % remove all elements not in the Glb
                  intset_intersect_upb(NewLexMin,Attr)
              ;
                  % TODO: ensure that the cardinality is strictly larger
                  % than the number of elemnts in the GLB if the min lex
                  % bound is not equal to the GLB
                  Attr = int_sets with [card:C, lcard:LCard,
                                        lex_min:CurrentLexMin],
                  % NOTE: CurrentLexMin may be different to NewLexMin,
                  % because of earlier domain changes
                  get_bounds(C,CMin,CMax),
                  ((CMin =< LCard, CurrentLexMin \== Glb) ->
                      ImposeCMin is LCard+1,
                      impose_min(C, ImposeCMin),
                      remove_minimal_lub(Attr, CMax,
                                         LCard, CurrentLexMin,
                                         Map, Offset) 
                  ;
                      true
                  )
              ),
              notify_constrained(S)
        ;
              true
        ).

set_lex_max(Attr, NewCMax, NewLexMax):-
	Attr = int_sets with [lex_max:LexMax],
        %incval(set_lex_max_count),
        ( lex_set:lex_less(NewLexMax,LexMax) ->
              % check to see if largest element of Lub are valid
              lex_glb(Attr,Glb),
              lex_lub(Attr, OldLub),
              lex_set:lex_subtract(Glb,NewLexMax,LexMaxSignificant),
              (LexMaxSignificant=[Largest|_] ->
                  true
              ;
                  % remove all potential elements
                  Largest= -1.0Inf,
                  true
              ),
              lex_set:lex_subtract(Glb,OldLub,Potential),
              remove_larger(Attr, Largest, Potential),
              lex_lub(Attr, Lub),
              
              setarg(lex_max of int_sets, Attr, NewLexMax),
              schedule_suspensions(max_susp of int_sets, Attr),
              Attr = int_sets with [lex_min:LexMin],
              lex_set:lex_common_prefix(LexMin,NewLexMax,Common),
              (lex_set:lex_subtract(Glb,Common,NewElements) ->
                   intset_union_lwb(NewElements, Attr) % Common subset S
              ;
                   true
              ),
              (LexMax==LexMin ->
                  % ground the set
                  Attr = int_sets with [card:C,ucard:UCard],
                  ImposeCMax is length(LexMax)
              ;
                  % TODO: ensure that the cardinality is strictly less
                  % than the number of elemnts in the LUB if the max lex
                  % bound is not equal to the LUB
                  Attr = int_sets with [card:C,ucard:UCard],
                  get_bounds(C,_CMin,CMax),
                  (CMax >= UCard, NewLexMax \== Lub ->
                      ImposeCMax is UCard-1
                  ;
                      ImposeCMax = CMax
                  )
              )
        ;
              LexMax == NewLexMax,
              ImposeCMax = NewCMax,
              Attr = int_sets with [card:C,ucard:UCard]
        ),
        Attr = int_sets with [value:S],
        (var(S) ->
            % may wake
            impose_max(C, ImposeCMax),
            ( C == UCard ->
                C = UCard   % this will ground the set
            ;
                Attr = int_sets with [value:S],
                notify_constrained(S)
            )
        ;
            % set has become ground
            true
        ).
        
:-demon recalc_lex_bounds/2.
recalc_lex_bounds(Attr,Susp):-
	Attr = int_sets with [card:C],
        recalc_lex_bounds(Attr),
        (var(C) ->
            true
        ;
            kill_suspension(Susp) % set will become ground
        ).
recalc_lex_bounds(Attr):-
	Attr = int_sets with [card:C],
        attr_min_max(Attr,LexMin,LexMax),
        get_bounds(C,CMin,CMax),
        lex_glb_lub(Attr, Glb, Lub),
        length(LexMin,MinLen),
        (CMin =< MinLen, MinLen =< CMax ->
            true
        ;
            once(lex_set:lex_next_eq(LexMin, Glb, Lub, CMin, CMax,
                                     NewCMin, NewLexMin)),
            set_lex_min(Attr, NewCMin, NewLexMin)
        ),
        length(LexMax,MaxLen),
        (CMin =< MaxLen, MaxLen =< CMax ->
            true
        ;
            once(lex_set:lex_prev_eq(LexMax, Glb, Lub, CMin, CMax,
                                     NewCMax, NewLexMax)),
            set_lex_max(Attr, NewCMax, NewLexMax)
        ).

	% woken on C->inst
card_demon(SetAttr) :-
	SetAttr = int_sets with [card:C,value:Set,lcard:LCard,ucard:UCard],
	( C == LCard ->
	    lset_to_list(SetAttr, List),
	    Set = List
	; C == UCard ->
	    uset_to_list(SetAttr, List),
	    Set = List
	;
	    true
	).


%----------------------------------------------------------------------
% Weight constraint
%----------------------------------------------------------------------

weight(Set0, WeightArray, Weight) :-
	seteval(Set0, Set),
	functor(WeightArray, _, N),
	intset(Set, 1, N),
	integers([Weight]),
	get_set_attribute(Set, SetAttr),
	(
	    for(I,1,N),
	    fromto(0, MaxW0, MaxW1, MaxWeight),
	    fromto(0, MinW0, MinW1, MinWeight),
	    param(WeightArray,SetAttr)
	do
	    arg(I, WeightArray, WeightI),
	    ( integer(WeightI) ->
		( WeightI >= 0 ->
		    ( lset_member(I,SetAttr) ->
			MinW1 is MinW0+WeightI,
			MaxW1 is MaxW0+WeightI
		    ; uset_member(I,SetAttr) ->
			MinW1 = MinW0,
			MaxW1 is MaxW0+WeightI
		    ;
			MinW1 = MinW0,
			MaxW1 = MaxW0
		    )
		;
		    error(6, weight/3)
		)
	    ;
	    	error(5, weight/3)
	    )
	),
	init_event_receiver(added of int_sets, SetAttr, Susp, AddEvents),
	init_event_receiver(removed of int_sets, SetAttr, Susp, RemEvents),
	suspend(weight_demon(WeightArray, Weight,
			weight(MinWeight, MaxWeight), AddEvents, RemEvents), 4,
		[Set->add,Set->rem], Susp),
%		[Set->add,Set->rem,Weight->min,Weight->max], Susp),
	solver_module:(Weight :: MinWeight..MaxWeight),
%        suspend(weight_lex_max(Set, WeightArray, Weight, SuspMax),
%                5,
%                [Set->max_susp],SuspMax),
%        suspend(weight_lex_min(Set, WeightArray, Weight, SuspMin),
%                5,
%                [Set->min_susp],SuspMin),
%        suspend(weight_lex_weight(Set, WeightArray, Weight, SuspWeight),
%                5,
%                [Weight->min, Weight->max],SuspWeight),
	schedule_suspensions(1,s([Susp])), wake.

weighted_sum([], _WeightArray, Out, Out).
weighted_sum([H|T], WeightArray, In, Out):-
        W is WeightArray[H],
        In2 is In + W,
        weighted_sum(T, WeightArray, In2, Out).

:-demon weight_lex_min/4.
weight_lex_min(Set, WeightArray, Weight, _Susp):-
        get_bounds(Weight,WMin,WMax),
        lex_min(Set, Min),
        MinSum is weighted_sum(Min, WeightArray, 0),
        ((MinSum >= WMin, MinSum =< WMax) ->
            true
        ;
            get_set_attribute(Set, Attr),
            next_lex_min(Attr, NewLexMin, NewCMin),
            NewMinSum is weighted_sum(NewLexMin, WeightArray, 0),
            (NewMinSum >= WMin, NewMinSum =< WMax),
            !,
            set_lex_min(Attr, NewCMin, NewLexMin)
        ).

:-demon weight_lex_max/4.
weight_lex_max(Set, WeightArray, Weight, _Susp):-
        get_bounds(Weight,WMin,WMax),
        lex_max(Set, Max),
        MaxSum is weighted_sum(Max, WeightArray, 0),
        ((MaxSum >= WMin, MaxSum =< WMax) ->
            true
        ;
            get_set_attribute(Set, Attr),
            prev_lex_max(Attr, NewLexMax, NewCMax),
            NewMaxSum is weighted_sum(Max, WeightArray, 0),
            (NewMaxSum >= WMin, NewMaxSum =< WMax),
            !,
            set_lex_max(Attr, NewCMax, NewLexMax)
        ).

:-demon weight_lex_weight/4.
weight_lex_weight(Set, WeightArray, Weight, _Susp):-
        get_bounds(Weight,WMin,WMax),
        lex_min_max(Set, Min, Max),
        MinSum is weighted_sum(Min, WeightArray, 0),
        ((MinSum >= WMin, MinSum =< WMax) ->
            true
        ;
            get_set_attribute(Set, Attr),
            next_lex_min(Attr, NewLexMin, NewCMin),
            NewMinSum is weighted_sum(NewLexMin, WeightArray, 0),
            (NewMinSum >= WMin, NewMinSum =< WMax),
            !,
            set_lex_min(Attr, NewCMin, NewLexMin)
        ),
        MaxSum is weighted_sum(Max, WeightArray, 0),
        ((MaxSum >= WMin, MaxSum =< WMax) ->
            true
        ;
            get_set_attribute(Set, Attr),
            prev_lex_max(Attr, NewLexMax, NewCMax),
            NewMaxSum is weighted_sum(Max, WeightArray, 0),
            (NewMaxSum >= WMin, NewMaxSum =< WMax),
            !,
            set_lex_max(Attr, NewCMax, NewLexMax)
        ).

:- demon weight_demon/5.
weight_demon(WeightArray, Weight, SetWeight, AddReceiver, RemReceiver) :-
	receive_events(AddReceiver, AddEvents),
	receive_events(RemReceiver, RemEvents),
	SetWeight = weight(MinWeight0, MaxWeight0),
	(
	    foreach(Element,AddEvents),
	    fromto(MinWeight0, MinW0, MinW1, MinWeight1),
	    fromto(MaxWeight0, MaxW0, MaxW1, MaxWeight1),
	    param(WeightArray)
	do
	    MaxW1 = MaxW0,
	    MinW1 is MinW0 + WeightArray[Element]
	),
	(
	    foreach(Element,RemEvents),
	    fromto(MinWeight1, MinW0, MinW1, MinWeight),
	    fromto(MaxWeight1, MaxW0, MaxW1, MaxWeight),
	    param(WeightArray)
	do
	    MinW1 = MinW0,
	    MaxW1 is MaxW0 - WeightArray[Element]
	),
	setarg(1, SetWeight, MinWeight),
	setarg(2, SetWeight, MaxWeight),
	solver_module:(Weight :: MinWeight..MaxWeight).

% Improvements: many...


%----------------------------------------------------------------------
% Set partitioning
%----------------------------------------------------------------------

/*
partition([], Z) ?- !, Z=[].
partition([X], Z) ?- !, Z=X.
partition(Xs, Z) ?- Xs = [_|_],
	( foreach(X,Xs), foreach(XAttr, XAttrs) do
	    get_set_attribute(X, XAttr)
	),
	get_new_set_attribute(Z, XAttrs, ZAttr),

	% setup an array of maybe-counters for elements
	uset_min_max(ZAttr, Min, Max),
	N is Max-Min+1,
	Offset is Min-1,
	dim(Counters, [N]),
	( for(Elem,Min,Max), param(Counters,Offset) do
	    (
		foreach(XAttr, XAttrs),
	    	fromto(0, C0, C1, CountI),
		param(Elem)
	    do
	    	( uldiff_memberchk(Elem, XAttr) -> C1 is C0+1 ; C1=C0 )
	    ),
	    I is Elem - Offset,
	    arg(I, Counters, CountI)
	),

	(
	    foreach(XAttr, XAttrs),
	    param(ZAttr)
	do
	    XAttr = int_sets with value:X,
	    suspend(add_demon(ZAttr, AddEvents), 3, X->add, Susp1),
	    initial_member_events(XAttr, Susp1, AddEvents)
	),
	
	suspend(all_union_rem_z_demon(XAttrs, RemZReceiver), 3, Z->rem, Susp2),
	initial_nonmember_events(ZAttr, XAttrs, 

	



	    get_set_attribute(X, XAttr),
	    suspend(all_disjoint_demon(Head, Tail, AddEvents), 3, X->add, Susp),
	    initial_member_events(XAttr, Susp, AddEvents)
	),
	% don't schedule until all demons have been set up!
	schedule_suspensions(1,s(Susps)), wake.

:- demon all_union_rem_z_demon/2.
all_union_rem_z_demon(Attrs, Receiver) :-
	foreachevent(all_union_rem_z, Element, [Attrs], Receiver, (
	    ( foreach(Attr,Attrs), param(Element) do intset_remove(Element,Attr) )
	)).

:- demon all_union_add_x_demon/5.
all_union_add_x_demon(AttrZ, Receiver) :-
	foreachevent(all_union_add_x, Element, [AttrZ], Receiver, (
	    intset_add(Element, AttrZ)
	)).

all_union_rem_z_demon(Attrs, Counters, Offset, AddReceiver, RemReceiver) :-
	receive_events(AddReceiver, Added),	% to union
	receive_events(RemReceiver, Removed),	% to union
	call_priority((
	    ( foreach(Attr,Attrs), param(Counters,Offset,Added,Removed,Attrs) do
		( foreach(Element,Removed), param(Attr) do intset_remove(Element,Attr) )
		( foreach(Element,Added), param(Counters,Offset,Attr,Attrs)
		do
		    CountI is Counters[Element-Offset],
		    CountI > 0,		% else fail
		    ( CountI == 1 ->
			add_to_one(Element, Attrs)
			( foreach(Attr,Attrs), param(Element) do
			    ( uldiff_memberchk(Element,Attr) ->
				intset_add(Element,Attr)
			    ;
			    	true
			    )
			)
		)
	    )
	), 2).


    :- demon all_union_rhs_demon/5.
    all_union_rhs_demon(Attrs, Counters, Offset, AddReceiver, RemReceiver) :-
	receive_events(AddReceiver, Added),	% to union
	receive_events(RemReceiver, Removed),	% to union
	call_priority((
	    ( foreach(Attr,Attrs), param(Counters,Offset,Added,Removed,Attrs) do
		( foreach(Element,Removed), param(Attr) do intset_remove(Element,Attr) )
		( foreach(Element,Added), param(Counters,Offset,Attr,Attrs)
		do
		    CountI is Counters[Element-Offset],
		    CountI > 0,		% else fail
		    ( CountI == 1 ->
			add_to_one(Element, Attrs)
			( foreach(Attr,Attrs), param(Element) do
			    ( uldiff_memberchk(Element,Attr) ->
				intset_add(Element,Attr)
			    ;
			    	true
			    )
			)
		)
	    )
	), 2).


*/

%----------------------------------------------------------------------
% Booleans
%----------------------------------------------------------------------

membership_booleans(Set0, BoolArr) :-
	seteval(Set0, Set),
	( nonvar(BoolArr) ->
	    functor(BoolArr, _, Max),
	    intset(Set, 1, Max)
	; set_min_max(Set, MinS, MaxS) ->
	    MinS >= 1,
	    Max = MaxS,
	    functor(BoolArr, [], Max)
	;
	    error(4, membership_booleans(Set, BoolArr))
	),
	get_set_attribute(Set, SetAttr),
	SetAttr = int_sets with [booleans:Booleans],
	( nonvar(Booleans) ->
	    true
	;
	    init_event_receiver(added of int_sets, SetAttr, Susp, AddEvents),
	    init_event_receiver(removed of int_sets, SetAttr, Susp, RemEvents),
	    ( var(Set) ->
		suspend(boolean_forward_demon(BoolArr, AddEvents, RemEvents), 3,
			[Set->add,Set->rem], Susp)
	    ;
		true
	    ),
	    (
		for(I,1,Max),
		param(BoolArr,SetAttr)
	    do
		arg(I, BoolArr, Bool),
		( lset_member(I, SetAttr) ->
		    Bool = 1
		; uset_nonmember(I, SetAttr) ->
		    Bool = 0
		; Bool == 0 ->
		    intset_remove(I, SetAttr)
		; Bool == 1 ->
		    intset_add(I, SetAttr)
		;
		    arg(I, BoolArr, Bool),
		    solver_module:(Bool :: 0..1),
		    suspend(add_or_remove(Bool,I,SetAttr), 3, Bool->inst)
		)
	    )
	),
	Booleans = BoolArr.

    :- demon boolean_forward_demon/3.
    boolean_forward_demon(BoolArr, AddReceiver, RemReceiver) :-
	receive_events(AddReceiver, AddEvents),
	receive_events(RemReceiver, RemEvents),
	( foreach(Element,AddEvents), param(BoolArr) do
	    arg(Element, BoolArr, 1)
	),
	( foreach(Element,RemEvents), param(BoolArr) do
	    arg(Element, BoolArr, 0)
	).

    add_or_remove(0, I, SetAttr) ?-
    	intset_remove(I, SetAttr).
    add_or_remove(1, I, SetAttr) ?-
    	intset_add(I, SetAttr).


%----------------------------------------------------------------------
% Labeling
%----------------------------------------------------------------------

insetdomain(X, CardSel, ElemSel, Order) :-
	valid_card_sel(CardSel),
	valid_elem_sel(ElemSel),
	valid_order_sel(Order),
	!,
	insetdomain1(X, CardSel, ElemSel, Order).
insetdomain(X, CardSel, ElemSel, Order) :-
	error(6, insetdomain(X, CardSel, ElemSel, Order)).

insetdomain1(X, CardSel, ElemSel, Order) :-
	var(X), !,
	label_cardinality(X, CardSel),
	( var(X) ->	% label_cardinality/2 may instantiate
	    select_element(X, ElemSel, Elem),
	    search_node(label_element(X, Order, Elem)),
	    insetdomain1(X, CardSel, ElemSel, Order)
	;
	    true
	).
insetdomain1([], _CardSel, _ElemSel, _Order) ?- !.
insetdomain1([X|_], _CardSel, _ElemSel, _Order) ?-
	integer(X), !.	% only rudimentary check, to avoid complexity
insetdomain1(X, CardSel, ElemSel, Order) :-
	error(5, insetdomain(X, CardSel, ElemSel, Order)).

    valid_card_sel(any).		% default
    valid_card_sel(increasing).
    valid_card_sel(decreasing).

    valid_elem_sel(small_first).	% default
    valid_elem_sel(big_first).
    valid_elem_sel(random).
    valid_elem_sel(light_first(_Weights)).
    valid_elem_sel(heavy_first(_Weights)).

    valid_order_sel(in_notin).	% default
    valid_order_sel(notin_in).
    valid_order_sel(sbds).

label_cardinality(_X, any).
label_cardinality(X, increasing) :-
	#(X,C),
	indomain(C).
label_cardinality(X, decreasing) :-
	#(X,C),
	RC #= 1-C,
	indomain(RC).

label_element(X, in_notin, Elem) :- Elem in X.
label_element(X, in_notin, Elem) :- incval(bt_count),Elem notin X.
label_element(X, notin_in, Elem) :- Elem notin X.
label_element(X, notin_in, Elem) :- incval(bt_count),Elem in X.
label_element(X, sbds, Elem) :- sbds_module:sbds_try(X, Elem). % Uses order from SBDS.

select_element(X, small_first, Elem) :-
	get_set_attribute(X, XAttr),
	uldiff_smallest(XAttr, Elem).
select_element(X, big_first, Elem) :-
	get_set_attribute(X, XAttr),
	uldiff_biggest(XAttr, Elem).
select_element(X, random, Elem) :-
	get_set_attribute(X, XAttr),
	uldiff_random(XAttr, Elem).
select_element(X, light_first(Weights), Elem) :-
	get_set_attribute(X, XAttr),
	uldiff_lightest(XAttr, Weights, Elem).
select_element(X, heavy_first(Weights), Elem) :-
	get_set_attribute(X, XAttr),
	uldiff_heaviest(XAttr, Weights, Elem).


potential_members(_{int_sets:Attr}, Maybes) ?- nonvar(Attr), !,
	uldiff_to_list(Attr, Maybes).
potential_members(X, Maybes) :- var(X), !,
	error(4, potential_members(X, Maybes)).
potential_members(_, []).


%----------------------------------------------------------------------
% Set domain primitives
%----------------------------------------------------------------------


intset_union_lwb([], _SetAttr).
intset_union_lwb([E|Es], SetAttr) :-
	intset_add(E, SetAttr),
	intset_union_lwb(Es, SetAttr).

intset_intersect_upb(List, SetAttr) :-
	uset_to_list(SetAttr, UpbList),
	lex_diff(UpbList, List, RemoveList),
	( foreach(E,RemoveList), param(SetAttr) do
	    intset_remove(E,SetAttr)
	).

xremove_minimal_lub(_Attr, _CMax, _LCard1, _LexMin, _Map, _Offset):-
        true.
remove_minimal_lub(Attr, CMax, LCard1, LexMin, Map, Offset):-
        ( CMax - LCard1 =:= 1 ->
            % potentialy one more element to add to this set.
            % find the single undecidable element in LexMin
            (fromto(LexMin,[E|In],Out,[]),
             param(Map,Offset,MinUndecidedI) do
                I is E - Offset,
                arg(I, Map, Bit),
                (var(Bit) ->
                    Out=[],
                    MinUndecidedI=I
                ;
                    Out=In
                )
            ),
           (var(MinUndecidedI) ->
               true
           ;
               % remove all elements smalled than MinUndecided
               (for(I,MinUndecidedI-1,1,-1),
                param(Map,Offset,Attr) do
                   arg(I,Map,Bit),
                   (var(Bit) ->
                       E is Offset+I,
                       intset_remove(E,Attr)
                   ;
                       true
                   )
               )
           ),
            true
        ;
            % more than one element to be added, so nothing
            % can be done
            true
        ).


intset_add(Elem, Attr) :-
	integer(Elem),
	Attr = int_sets with [dom:Map,off:Offset,lcard:LCard,value:X,
                              card:C],
	I is Elem - Offset,
	I >= 1, I =< functor(Map, _),
	arg(I, Map, Bit),
	( var(Bit) ->
            attr_min_max(Attr,LexMin,LexMax),
	    Bit = 1,
	    LCard1 is LCard+1,
	    setarg(lcard of int_sets, Attr, LCard1),
	    trace_set('ADD_ELEM', Elem, X),
            % mark the lex_glb and lex_lub as dirty
            setarg(lex_dirty of int_sets, Attr, 1),
	    send_notification(added of int_sets, Attr, Elem),
	    schedule_suspensions(add of int_sets, Attr),
	    impose_min(C, LCard1),		%%% may wake
	    ( C == LCard1 ->       % set is ground
	    	lset_to_list(Attr, List),
		X = List,			% may wake
                % ensure the instantiation does not violate lex bounds
                lex_set:lex_leq(LexMin,X),
                lex_set:lex_leq(X,LexMax),
                % set the lex bounds explicitly
                set_lex_min(Attr, C, X),
                set_lex_max(Attr, C, X)
	    ;
                % remove minimal elements from lub if possible
                get_bounds(C,CMin,CMax),
                % update the lex bounds if necessary
                ( lex_set:lex_memberchk(Elem,LexMin) ->
                    % no change to LexMin
                    NewLexMin=LexMin
                ;
                    lex_glb_lub(Attr, RevGlb, RevLub),
                    once(lex_set:lex_next_eq(LexMin, RevGlb, RevLub, CMin,
                                             CMax, NewCMin, NewLexMin)),
                    set_lex_min(Attr, NewCMin, NewLexMin)
                ),
                ( lex_set:lex_memberchk(Elem,LexMax) ->
                    % no change to LexMax
                    true
                ;
                    (var(RevGlb) ->
                        lex_glb_lub(Attr, RevGlb, RevLub)
                    ;
                        % already created RevGlb and RevLub
                        true
                    ),
                    once(lex_set:lex_prev_eq(LexMax, RevGlb, RevLub,
                                             CMin, CMax,NewCMax, NewLexMax)),
                    set_lex_max(Attr, NewCMax, NewLexMax)
                ),
		notify_constrained(X),
                remove_minimal_lub(Attr, CMax, LCard1, NewLexMin, Map,
                                    Offset)
	    ),
	    wake
	;
	    Bit == 1
	).

intset_remove(Elem, Attr) :-
	integer(Elem),
	Attr = int_sets with [dom:Map,off:Offset,ucard:UCard,value:X,
                              card:C],
	I is Elem - Offset,
	functor(Map, _, Max),
	( I < 1 ->
	    true
	; I > Max ->
	    true
	;
	    arg(I, Map, Bit),
	    ( var(Bit) ->
                attr_min_max(Attr,LexMin,LexMax),
		Bit = 0,
		UCard1 is UCard-1,
		setarg(ucard of int_sets, Attr, UCard1),
		trace_set('REM_ELEM', Elem, X),
                % mark the lex_glb and lex_lub as dirty
                setarg(lex_dirty of int_sets, Attr, 1),
		send_notification(removed of int_sets, Attr, Elem),
		schedule_suspensions(rem of int_sets, Attr),
		impose_max(C, UCard1),		%%% may wake
		( C == UCard1 ->
		    uset_to_list(Attr, List),
		    X = List			% may wake
		;
                    ( lex_set:lex_memberchk(Elem,LexMin) ->
                        get_bounds(C,CMin,CMax),
                        lex_glb_lub(Attr, RevGlb, RevLub),
                        once(lex_set:lex_next_eq(LexMin, RevGlb, RevLub,
                                                 CMin, CMax,NewCMin,
                                                 NewLexMin)),
                        set_lex_min(Attr,NewCMin, NewLexMin)
                    ;
                        % no change to LexMin
                        true
                    ),
                    ( lex_set:lex_memberchk(Elem,LexMax) ->
                        (var(RevGlb) ->
                            get_bounds(C,CMin,CMax),
                            lex_glb_lub(Attr, RevGlb, RevLub)
                        ;
                            % already created RevGlb and RevLub
                            true
                        ),
                        once(lex_set:lex_prev_eq(LexMax, RevGlb, RevLub,
                                                 CMin, CMax, NewCMax, NewLexMax)),
                        set_lex_max(Attr, NewCMax, NewLexMax)
                    ;
                        % no change to LexMax
                        true
                    ),
		    notify_constrained(X)
		),
		wake
	    ;
		Bit == 0
	    )
	).


intset_domain(Attr, L, U) :-
	Attr = int_sets with [dom:Map,off:Offset,lcard:0,ucard:N,card:
                                                                     C,
                              value:V],
        attr_min_max(Attr,LexMin,LexMax),
	N is max(0,U-L+1),
	functor(Map, [], N),
	( N = 0 ->
	    C = 0,
	    V = [],
            LexMin = [],
            LexMax = [],
	    close_sender(added of int_sets, Attr),
	    close_sender(removed of int_sets, Attr)
	;
	    solver_module:(C::0..N),	%%%
            LexMin = [],
            (for(I,U,L,-1), foreach(I,LexMax) do true),  % default domain
	    open_sender(added of int_sets, Attr),
	    open_sender(removed of int_sets, Attr)
	),
	init_suspension_list(add of int_sets, Attr),
	init_suspension_list(rem of int_sets, Attr),
	init_suspension_list(min_susp of int_sets, Attr),
	init_suspension_list(max_susp of int_sets, Attr),
	Offset is L-1.

intset_domain_from_lists(Attr, [], []) :- !,
	intset_domain(Attr, 1, 0).
intset_domain_from_lists(Attr, LowerList, UpperList) :-
	sort(0,'>',UpperList, UpperSorted),
	sort(0,'>',LowerList, LowerSorted),
	lex_subseteq(LowerSorted, UpperSorted),
	create_lset_uset(LowerSorted, UpperSorted, Attr).

    create_lset_uset(LowerSorted, UpperSorted, Attr) :-
	UpperSorted = [MaxUniv|_], once append(_, [MinUniv], UpperSorted),
	intset_domain(Attr, MinUniv, MaxUniv),
	(
	    for(I,MaxUniv,MinUniv,-1),
	    fromto(UpperSorted, Upper0, Upper1, []),
	    param(Attr)
	do
	    ( Upper0 = [I|Upper1] ->
	        true
	    ;
		Upper0 = Upper1,
		uset_remove_member(I, Attr)
	    )
	),
	( foreach(LwbElem,LowerSorted), param(Attr) do
	    lset_add_nonmember(LwbElem, Attr)
	),
	Attr = int_sets with [lcard:LCard,ucard:UCard,card:C,value:Value],
        %attr_min_max(Attr,LexMin,LexMax),
	( LCard == UCard ->
	    C = LCard,
	    close_sender(added of int_sets, Attr),
	    close_sender(removed of int_sets, Attr),
            setarg(lex_min of int_sets, Attr, LowerSorted),
            setarg(lex_max of int_sets, Attr, LowerSorted),
	    Value = LowerSorted
	; 
	    solver_module:(C :: LCard..UCard),
            % find initial lex min and lex max values
            lex_set:lex_subtract(LowerSorted,UpperSorted,Remaining),
            get_bounds(C,CMin,CMax),
            MinReqCard is CMin - length(LowerSorted),
            MaxReqCard is CMax - length(LowerSorted),
            length(MinReq,MinReqCard),
            length(MaxReq,MaxReqCard),
            once(append(_,MinReq,Remaining)),  % smallest
            once(append(MaxReq,_,Remaining)),  % largest
            lex_set:lex_union(LowerSorted,MinReq,NewLexMin),
            lex_set:lex_union(LowerSorted,MaxReq,NewLexMax),
            setarg(lex_min of int_sets, Attr, NewLexMin),
            setarg(lex_max of int_sets, Attr, NewLexMax)
	).

lset_member(Elem, Attr) :- var(Elem), !,
	Attr = int_sets with [dom:Map,off:Offset],
	functor(Map, _, Arity),
	between(1, Arity, 1, I),
	arg(I, Map, Bit),
	Bit == 1,
	Elem is I + Offset.
lset_member(Elem, Attr) :- integer(Elem), !,
	Attr = int_sets with [dom:Map,off:Offset],
	I is Elem - Offset,
	I >= 1, I =< functor(Map, _),
	arg(I, Map, Bit),
	Bit == 1.
lset_member(Elem, Attr) :-
	error(5, lset_member(Elem, Attr)).

uldiff_memberchk(Elem, Attr) :- integer(Elem),
	Attr = int_sets with [dom:Map,off:Offset],
	I is Elem - Offset,
	I >= 1, I =< functor(Map, _),
	arg(I, Map, Bit),
	var(Bit).

uset_member(Elem, Attr) :- var(Elem), !,
	Attr = int_sets with [dom:Map,off:Offset],
	functor(Map, _, Arity),
	between(1, Arity, 1, I),
	arg(I, Map, Bit),
	Bit \== 0,
	Elem is I + Offset.
uset_member(Elem, Attr) :- integer(Elem), !,
	Attr = int_sets with [dom:Map,off:Offset],
	I is Elem - Offset,
	I >= 1, I =< functor(Map, _),
	arg(I, Map, Bit),
	Bit \== 0.
uset_member(Elem, Attr) :-
	error(5, uset_member(Elem, Attr)).

lset_nonmember(Elem, Attr) :-
	Attr = int_sets with [dom:Map,off:Offset],
	I is Elem - Offset,
	( I < 1 -> true
	; I > functor(Map, _) -> true
	;
	    arg(I, Map, Bit),
	    Bit \== 1
	).

uset_nonmember(Elem, Attr) :-
	Attr = int_sets with [dom:Map,off:Offset],
	I is Elem - Offset,
	( I < 1 -> true
	; I > functor(Map, _) -> true
	;
	    arg(I, Map, Bit),
	    Bit == 0
	).

uset_remove_member(Elem, Attr) :-
	Attr = int_sets with [dom:Map,off:Offset,ucard:UCard],
	I is Elem - Offset,
	arg(I, Map, 0),
	UCard1 is UCard-1,
	setarg(ucard of int_sets, Attr, UCard1).

lset_add_nonmember(Elem, Attr) :-
	Attr = int_sets with [dom:Map,off:Offset,lcard:LCard],
	I is Elem - Offset,
	arg(I, Map, 1),
	LCard1 is LCard+1,
	setarg(lcard of int_sets, Attr, LCard1).


% get min/max element in set representation (not necessarily in the domain)
% fail for non-set variables and []
set_min_max(_{int_sets:Attr}, Min, Max) ?-
	uset_min_max(Attr, Min, Max).
set_min_max([First|Rest], Min, Max) ?-
	Min = First,
	once append(_, [Max], Rest).

uset_min_max(int_sets with [dom:Map,off:Offset], Min, Max) ?-
	Min is Offset+1,
	Max is Offset+functor(Map,_).


lset_card(int_sets with lcard:LCard, Card) ?- Card=LCard.

uset_card(int_sets with ucard:UCard, Card) ?- Card=UCard.

uset_to_list(int_sets with [dom:Map,off:Offset], Elements) :-
	functor(Map, _, Arity),
	( for(I,Arity,1,-1), fromto(Elements,E1,E0,[]),
          loop_name(uset_to_list_loop),
          param(Map,Offset)
        do
	    arg(I, Map, Bit),
	    ( Bit == 0 -> E1=E0 ; Elem is I+Offset, E1=[Elem|E0] )
	).

lset_to_list(Attr, Elements) :-
	lset_to_list(Attr, Elements, []).

lset_to_list(int_sets with [dom:Map,off:Offset], Elements, Elements0) :-
	functor(Map, _, Arity),
	( for(I,Arity,1,-1), fromto(Elements,E1,E0,Elements0),
          loop_name(lset_to_list_loop),
          param(Map,Offset) do
	    arg(I, Map, Bit),
	    ( Bit == 1 -> Elem is I+Offset, E1=[Elem|E0] ; E1=E0 )
	).

lset_uset_to_lists_reverse(int_sets with [dom:Map,off:Offset], GlbElements, LubElements) :-
	functor(Map, _, Arity),
	( for(I,1,Arity),
          fromto(GlbElements,LE1,LE0,[]),
          fromto(LubElements, UE1, UE0, []),
          loop_name(lset_uset_to_lists_reverse_loop),
          param(Map,Offset) do
	    arg(I, Map, Bit),
            ( var(Bit) ->
                  Elem is I+Offset,
                  UE1=[Elem|UE0],
                  LE1=LE0
            ; Bit == 1 ->
                  Elem is I+Offset,
                  UE1=[Elem|UE0],
                  LE1=[Elem|LE0]
            ; % Bit == 0
                  UE1=UE0,
                  LE1=LE0
            )
	).

lset_uset_to_lists(int_sets with [dom:Map,off:Offset], GlbElements, LubElements) :-
	functor(Map, _, Arity),
	( for(I,Arity,1,-1),
          fromto(GlbElements,LE1,LE0,[]),
          fromto(LubElements, UE1, UE0, []),
          loop_name(lset_uset_to_lists_loop),
          param(Map,Offset) do
	    arg(I, Map, Bit),
            ( var(Bit) ->
                  Elem is I+Offset,
                  UE1=[Elem|UE0],
                  LE1=LE0
            ; Bit == 1 ->
                  Elem is I+Offset,
                  UE1=[Elem|UE0],
                  LE1=[Elem|LE0]
            ; % Bit == 0
                  UE1=UE0,
                  LE1=LE0
            )
	).


uldiff_to_list(int_sets with [dom:Map,off:Offset], Elements) :-
	functor(Map, _, Arity),
	( for(I,Arity,1,-1), fromto(Elements,E1,E0,[]),
        loop_name(uldiff_to_list_loop),
          param(Map,Offset) do
	    arg(I, Map, Bit),
	    ( var(Bit) -> Elem is I+Offset, E1=[Elem|E0] ; E1=E0 )
	).

uldiff_smallest(Attr, Elem) :-
	Attr = int_sets with [dom:Map,off:Offset],
	functor(Map, _, Arity),
	between(1, Arity, 1, I),
	arg(I, Map, Bit),
	var(Bit),
	!,
	Elem is I + Offset.

uldiff_biggest(Attr, Elem) :-
	Attr = int_sets with [dom:Map,off:Offset],
	functor(Map, _, Arity),
	between(Arity, 1 ,-1, I),
	arg(I, Map, Bit),
	var(Bit),
	!,
	Elem is I + Offset.

uldiff_random(Attr, Elem) :-
	Attr = int_sets with [dom:Map,off:Offset,ucard:UCard,lcard:Lcard],
	Rand is 1 + (random mod (UCard-Lcard)),
	( fromto(Rand, R0, R1, 0), count(I,1,RandI), param(Map) do
	    arg(I, Map, Bit),
	    ( var(Bit) -> R1 is R0-1 ; R1=R0 )
	),
	Elem is RandI + Offset.

uldiff_lightest(Attr, Weights, Elem) :-
	Attr = int_sets with [dom:Map,off:Offset,ucard:UCard,lcard:Lcard],
	DiffCard is UCard-Lcard,
	(
	    fromto(DiffCard, R0, R1, 0),
	    fromto(+1.0Inf, MinW0, MinW1, _),
	    fromto(_, Best0, Best1, LightestI),
	    count(I,1,_),
	    param(Map,Weights,Offset)
	do
	    arg(I, Map, Bit),
	    ( var(Bit) ->
		R1 is R0-1,
		WeightI is Weights[I+Offset],
		( WeightI < MinW0 ->
		    MinW1 is WeightI, Best1 = I
		;
		    MinW1 = MinW0, Best1 = Best0
		)
	    ;
		R1=R0, MinW1 = MinW0, Best1 = Best0
	    )
	),
	Elem is LightestI + Offset.

uldiff_heaviest(Attr, Weights, Elem) :-
	Attr = int_sets with [dom:Map,off:Offset,ucard:UCard,lcard:Lcard],
	DiffCard is UCard-Lcard,
	(
	    fromto(DiffCard, R0, R1, 0),
	    fromto(-1.0Inf, MaxW0, MaxW1, _),
	    fromto(_, Best0, Best1, HeaviestI),
	    count(I,1,_),
	    param(Map,Weights,Offset)
	do
	    arg(I, Map, Bit),
	    ( var(Bit) ->
		R1 is R0-1,
		WeightI is Weights[I+Offset],
		( WeightI > MaxW0 ->
		    MaxW1 is WeightI, Best1 = I
		;
		    MaxW1 = MaxW0, Best1 = Best0
		)
	    ;
		R1=R0, MaxW1 = MaxW0, Best1 = Best0
	    )
	),
	Elem is HeaviestI + Offset.


significant_nonmembers(Attr, Attr1, Elements) :-
	significant_nonmembers1(Attr, Attr1, Elements, []).

significant_nonmembers1(Attr, Attr1, Elements, Elements0) :-
	uldiff_to_list(Attr1, SignificantElements),
	( foreach(E,SignificantElements),
	  fromto(Elements,E1,E0,Elements0),
	  param(Attr)
	do
	    ( uset_nonmember(E,Attr) -> E1=[E|E0] ; E1=E0 )
	).

significant_nonmembers(Attr, Attr1, Attr2, Elements) :-
	significant_nonmembers1(Attr, Attr1, Attr2, Elements, []).

significant_nonmembers1(Attr, Attr1, Attr2, Elements, Elements0) :-
	uldiff_to_list(Attr1, SignificantElements1),
	uldiff_to_list(Attr2, SignificantElements2),
	merge(0, >, SignificantElements1, SignificantElements2, SignificantElements),
	( foreach(E,SignificantElements),
	  fromto(Elements,E1,E0,Elements0),
	  param(Attr)
	do
	    ( uset_nonmember(E,Attr) -> E1=[E|E0] ; E1=E0 )
	).


	% send add-events for all current members of X
initial_member_events(XAttr, Susp, Receiver) :-
	lset_to_list(XAttr, AllEvents, FutureEvents),
	init_event_receiver(added of int_sets, XAttr, Susp,
		AllEvents, FutureEvents, Receiver).

	% send rem-events for all nonmembers of X which may
	% be significant with respect to Z
initial_nonmember_events(XAttr, ZAttr, Susp, Receiver) :-
	significant_nonmembers1(XAttr, ZAttr, AllEvents, FutureEvents),
	init_event_receiver(removed of int_sets, XAttr, Susp,
		AllEvents, FutureEvents, Receiver).

	% send rem-events for all nonmembers of X which may
	% be significant with respect to Y or Z
initial_nonmember_events(XAttr, YAttr, ZAttr, Susp, Receiver) :-
	significant_nonmembers1(XAttr, YAttr, ZAttr, AllEvents, FutureEvents),
	init_event_receiver(removed of int_sets, XAttr, Susp,
		AllEvents, FutureEvents, Receiver).



% -------------------------
% Hybrid stuff
% -------------------------


% S3 is the intsersection of S1 and S2, plus lex_bounds of S1 and S2
% are updated to ensure that they are consistent with the cardinality
% of S3
intersect_lex(S1,S2,S3):-
        intersection(S1,S2,S3),
        S3 leq S1,
        S3 leq S2,
        #(S3,C3),
        suspend(intersect_lex_max(S1,S2,C3,Susp1),5,
                [S1->inst, S1->add, S2->min_susp, S2->max_susp, C3->max],Susp1),
        suspend(intersect_lex_min(S1,S2,C3,Susp2),5,
                [S1->inst, S1->rem, S2->min_susp, S2->max_susp, C3->min],Susp2),
        suspend(intersect_lex_max(S2,S1,C3,Susp3),5,
                [S2->inst, S2->add, S1->min_susp, S1->max_susp, C3->max],Susp3),
        suspend(intersect_lex_min(S2,S1,C3,Susp4),5,
                [S2->inst, S2->rem, S1->min_susp, S1->max_susp, C3->min],Susp4),
        call(C3 #=< min(#(S1),#(S2))),
        true.


% ensure that the lex bound of S2, intersects the glb(S1) in no
% more than max(N) elements
:-demon(intersect_lex_max/4).
intersect_lex_max(_S1,S2,_N,Susp):-
        nonvar(S2),!,kill_suspension(Susp).
intersect_lex_max(S1,S2,N,_Susp):-
        get_bounds(N,_,NMax),
        ((var(S1),
          get_set_attribute(S1,int_sets with [lcard:LCard]),
          LCard =< NMax
         ) ->
            % the Glb does not contain more that NMax elements so
            % neither can the intersection
            true
        ;
            lex_set_range(S1,Glb,_),
            lex_min_max(S2,Min,Max),
            (lex_set:lex_int_card(Glb,Min) =< NMax ->
                ( lex_set:lex_int_card(Glb,Max) =< NMax ->
                    true
                ;
                    get_set_attribute(S2, Attr),
                    prev_lex_max(Attr,NewLexMax,NewCMax,(ic_bound_sets:int_leq_than(A,B,C,D,E,F,G,H,Glb, NMax))-[](A,B,C,D,E,F,G,H)),
                    lex_set:lex_int_card(Glb,NewLexMax) =< NMax,
                    !,
                    set_lex_max(Attr,NewCMax,NewLexMax),  % this will re-
                                                          % schedule the
                                                          % constraint
                    true
                )
            ;
                get_set_attribute(S2, Attr),
                next_lex_min(Attr, NewLexMin, NewCMin,(ic_bound_sets:int_leq_than(A,B,C,D,E,F,G,H,Glb,NMax))-[](A,B,C,D,E,F,G,H)),
                lex_set:lex_int_card(Glb, NewLexMin) =< NMax,
                !,
                set_lex_min(Attr,NewCMin,NewLexMin), % this will re-
                                                     % schedule the constraint
                true
            )
        ).

:-export int_leq_than/10.
% succeed if the partial set S1, (with associated remaining glb and
% lub elements and cardinality ranges) can be extended to a full set
% that intersects S2 in less than (or exactly) NMax elements
int_leq_than(Result,S1,S1Len,X,S1GlbRemain,S1LubRemain,S1CMin,_S1CMax,S2,NMax):-
        (lex_set:lex_memberchk(X,S2) ->
            % X will effect the size of the intersection
            lex_set:lex_int_card(S1, S2, LenS1S2Int),
            (LenS1S2Int == NMax ->
                % can NEVER add X
                Result=exclude
            ; LenS1S2Int > NMax ->
                fail
            ;
                % adding X will not violate intersection size
                true
            )
        ;
            % X cannot effect the size of the intersection directly,
            % only indirectly
            MaxSafe is length(S1LubRemain) - (lex_set:lex_int_card(S1LubRemain,S2)),
            (MaxSafe >= S1CMin ->
                % there are easily enough safe elements to allow X to
                % be excluded
                true
            ;
                MinUnsafeRequired is S1CMin - S1Len - MaxSafe,
                (MinUnsafeRequired + (lex_set:lex_int_card(S1GlbRemain,S2)) + (lex_set:lex_int_card(S1,S2)) > NMax ->
                    % X must be included, cause if it isn't then the
                    % rest of the set must be made up of too many
                    % unsafe elements
                    Result = include,
                    true
                ;
                    true
                )
            )
        ).

:-export int_geq_than/10.
% succeed if the partial set S1, (with associated remaining glb and
% lub elements and cardinality ranges) can be extended to a full set
% that intersects S2 in more than (or exactly) NMin elements
int_geq_than(_Result,S1,S1Len,_X,_S1GlbRemain,S1LubRemain,_S1CMin,
             S1CMax,S2,NMin):-
        lex_set:lex_int_card(S1, S2, LenS1S2Int),
        (LenS1S2Int >= NMin ->
            true
        ;
            RequiredNum is NMin - LenS1S2Int,
            lex_set:lex_int_card(S1LubRemain,S2) >= RequiredNum,
            RequiredNum =< S1CMax-S1Len,
            true
        ),
        !.
int_geq_than(include,_S1,_S1Len,_X,_S1GlbRemain,_S1LubRemain,_S1CMin,
             _S1CMax,_S2,_NMin).


% ensure that the lex bound of S2, intersects the lub(S1) in no
% less than min(N) elements
:-demon(intersect_lex_min/4).
intersect_lex_min(_S1,S2,_N,Susp):-
        nonvar(S2),!,kill_suspension(Susp).
intersect_lex_min(S1,S2,N,Susp):-
        get_bounds(N,NMin,_),
        (NMin == 0 ->
            (nonvar(N) -> % N=0
                kill_suspension(Susp)
            ;
                %writeln(a),
                true
            )
        ;
            lex_set_range(S1,_,Lub),
            lex_min_max(S2,Min,Max),
            (lex_set:lex_int_card(Lub,Min) >= NMin ->
                ( lex_set:lex_int_card(Lub,Max) >= NMin ->
                    true
                ;
                    get_set_attribute(S2, Attr),
                    prev_lex_max(Attr,NewLexMax,NewCMax,(ic_bound_sets:int_geq_than(A,B,C,D,E,F,G,H,Lub, NMin))-[](A,B,C,D,E,F,G,H)),
                    lex_set:lex_int_card(Lub,NewLexMax) >= NMin,
                    !,
                    set_lex_max(Attr,NewCMax,NewLexMax),
                    true
                )
            ;
                get_set_attribute(S2, Attr),
                next_lex_min(Attr, NewLexMin, NewCMin, (ic_bound_sets:int_geq_than(A,B,C,D,E,F,G,H,Lub, NMin))-[](A,B,C,D,E,F,G,H)),
                lex_set:lex_int_card(Lub, NewLexMin) >= NMin,
                !,
                set_lex_min(Attr,NewCMin,NewLexMin),
                true
            )
        ).


local_intersect_atmost_n(S1,S2,Int,N):-
        S1 /\ S2 sameset Int,
        #(Int, C),		% constrain the cardinality
        C #=< N. 		% of pairwise intersection

intersect_atmost_n(S1,S2,Int,N):-
        intersect_lex(S1,S2,Int),
        #(Int, C),		% constrain the cardinality
        C #=< N. 		% of pairwise intersection




% S3 is the union of S1 and S2, plus lex_bounds of S1 and S2
% are updated to ensure that they are consistent with the cardinality
% of S3
union_lex(S1,S2,S3):-
        union(S1,S2,S3),
        S1 leq S3,
        S2 leq S3,
        #(S3,C3),
        suspend(union_lex_max(S1,S2,C3,Susp1),5,
                [S1->inst, S1->add, S2->min_susp, S2->max_susp, C3->max],Susp1),
        suspend(union_lex_min(S1,S2,C3,Susp2),5,
                [S1->inst, S1->rem, S2->min_susp, S2->max_susp, C3->min],Susp2),
        suspend(union_lex_max(S2,S1,C3,Susp3),5,
                [S2->inst, S2->add, S1->min_susp, S1->max_susp, C3->max],Susp3),
        suspend(union_lex_min(S2,S1,C3,Susp4),5,
                [S2->inst, S2->rem, S1->min_susp, S1->max_susp, C3->min],Susp4),
        call(C3 #>= max(#(S1),#(S2))),
        true.


% ensure that the lex bound of S2, unions the glb(S1) in no
% more than max(N) elements
:-demon(union_lex_max/4).
union_lex_max(S1,S2,N,_Susp):-
        get_bounds(N,_,NMax),
        lex_set_range(S1,Glb,_),
        lex_min_max(S2,Min,Max),
        lex_set:lex_union(Glb,Min,MinUnion),
        (length(MinUnion) =< NMax ->
            lex_set:lex_union(Glb,Max,MaxUnion),
            ( length(MaxUnion) =< NMax ->
                true
            ;
                get_set_attribute(S2, Attr),
                prev_lex_max(Attr,NewLexMax,NewCMax),
                lex_set:lex_union(Glb,NewLexMax,NewMaxUnion),
                length(NewMaxUnion) =< NMax,
                !,
                set_lex_max(Attr,NewCMax,NewLexMax),  % this will re-
                                                      % schedule the
                                                      % constraint
                true
            )
        ;
            get_set_attribute(S2, Attr),
            next_lex_min(Attr, NewLexMin, NewCMin),
            lex_set:lex_union(Glb, NewLexMin, NewMinUnion),
            length(NewMinUnion) =< NMax,
            !,
            set_lex_min(Attr,NewCMin,NewLexMin), % this will re-
                                                 % schedule the constraint
            true
        ).
% ensure that the lex bound of S2, unions the lub(S1) in no
% less than min(N) elements
:-demon(union_lex_min/4).
union_lex_min(S1,S2,N,_Susp):-
        get_bounds(N,NMin,_),
        lex_set_range(S1,_,Lub),
        (length(Lub) >= NMin ->
            % quick termination
            true
        ;
            lex_min_max(S2,Min,Max),
            lex_set:lex_union(Lub,Min,MinUnion),
            (length(MinUnion) >= NMin ->
                lex_set:lex_union(Lub,Max,MaxUnion),
                ( length(MaxUnion) >= NMin ->
                    true
                ;
                    get_set_attribute(S2, Attr),
                    prev_lex_max(Attr,NewLexMax,NewCMax),
                    lex_set:lex_union(Lub,NewLexMax,NewMaxUnion),
                    length(NewMaxUnion) >= NMin,
                    !,
                    set_lex_max(Attr,NewCMax,NewLexMax),
                    true
                )
            ;
                get_set_attribute(S2, Attr),
                next_lex_min(Attr, NewLexMin, NewCMin),
                lex_set:lex_union(Lub, NewLexMin, NewMinUnion),
                length(NewMinUnion) >= NMin,
                !,
                set_lex_min(Attr,NewCMin,NewLexMin),
                true
            )
        ).

% S3 is the difference of S1 and S2, plus lex_bounds of S1 and S2
% are updated to ensure that they are consistent with the cardinality
% of S3
difference_lex(S1,S2,S3):-
        difference(S1,S2,S3),
        S3 leq S1,
        %S2 leq S3,
        #(S3,C3),
        suspend(diff_lex_max_1(S1,S2,C3,Susp1),5,
                [S2->rem, S1->min_susp, S1->max_susp, C3->max],Susp1),
        suspend(diff_lex_min_1(S1,S2,C3,Susp2),5,
                [S2->add, S1->min_susp, S1->max_susp, C3->min],Susp2),
        suspend(diff_lex_max_2(S1,S2,C3,Susp3),5,
                [S2->add, S1->min_susp, S1->max_susp, C3->max],Susp3),
        suspend(diff_lex_min_2(S1,S2,C3,Susp4),5,
                [S2->rem, S1->min_susp, S1->max_susp, C3->min],Susp4),
        call(C3 #=< #(S1)),
        true.


% ensure that the lex bound of S1, diffs the lub(S2) in no
% more than max(N) elements
:-demon(diff_lex_max_1/4).
diff_lex_max_1(S1,S2,N,_Susp):-
        get_bounds(N,_,NMax),
        lex_set_range(S2,_,Lub),
        lex_min_max(S1,Min,Max),
        lex_set:lex_diff(Min,Lub,MinDiff),
        (length(MinDiff) =< NMax ->
            lex_set:lex_diff(Max, Lub, MaxDiff),
            ( length(MaxDiff) =< NMax ->
                true
            ;
                get_set_attribute(S1, Attr),
                prev_lex_max(Attr,NewLexMax,NewCMax),
                lex_set:lex_diff(NewLexMax, Lub, NewMaxDiff),
                length(NewMaxDiff) =< NMax,
                !,
                set_lex_max(Attr,NewCMax,NewLexMax),  % this will re-
                                                      % schedule the
                                                      % constraint
                true
            )
        ;
            get_set_attribute(S1, Attr),
            next_lex_min(Attr, NewLexMin, NewCMin),
            lex_set:lex_diff(NewLexMin, Lub, NewMinDiff),
            length(NewMinDiff) =< NMax,
            !,
            set_lex_min(Attr,NewCMin,NewLexMin), % this will re-
                                                 % schedule the constraint
            true
        ).
% ensure that the lex bound of S1, diffs the glb(S2) in no
% less than min(N) elements
:-demon(diff_lex_min_1/4).
diff_lex_min_1(S1,S2,N,_Susp):-
        get_bounds(N,NMin,_),
        lex_set_range(S2,Glb,_),
        lex_min_max(S1,Min,Max),
        lex_set:lex_diff(Min,Glb,MinDiff),
        (length(MinDiff) >= NMin ->
            lex_set:lex_diff(Max,Glb,MaxDiff),
            ( length(MaxDiff) >= NMin ->
                true
            ;
                get_set_attribute(S1, Attr),
                prev_lex_max(Attr,NewLexMax,NewCMax),
                lex_set:lex_diff(NewLexMax,Glb,NewMaxDiff),
                length(NewMaxDiff) >= NMin,
                !,
                set_lex_max(Attr,NewCMax,NewLexMax),
                true
            )
        ;
            get_set_attribute(S1, Attr),
            next_lex_min(Attr, NewLexMin, NewCMin),
            lex_set:lex_diff(NewLexMin, Glb, NewMinDiff),
            length(NewMinDiff) >= NMin,
            !,
            set_lex_min(Attr,NewCMin,NewLexMin),
            true
        ).
% ensure that the lex bound of S2 when diffed from glb(S1) is no
% more than max(N) elements
:-demon(diff_lex_max_2/4).
diff_lex_max_2(S1,S2,N,_Susp):-
        get_bounds(N,_,NMax),
        lex_set_range(S1,Glb,_),
        lex_min_max(S2,Min,Max),
        lex_set:lex_diff(Glb,Min,MinDiff),
        (length(MinDiff) =< NMax ->
            lex_set:lex_diff(Glb, Max, MaxDiff),
            ( length(MaxDiff) =< NMax ->
                true
            ;
                get_set_attribute(S2, Attr),
                prev_lex_max(Attr,NewLexMax,NewCMax),
                lex_set:lex_diff(Glb, NewLexMax, NewMaxDiff),
                length(NewMaxDiff) =< NMax,
                !,
                set_lex_max(Attr,NewCMax,NewLexMax),  % this will re-
                                                      % schedule the
                                                      % constraint
                true
            )
        ;
            get_set_attribute(S2, Attr),
            next_lex_min(Attr, NewLexMin, NewCMin),
            lex_set:lex_diff(Glb, NewLexMin, NewMinDiff),
            length(NewMinDiff) =< NMax,
            !,
            set_lex_min(Attr,NewCMin,NewLexMin), % this will re-
                                                 % schedule the constraint
            true
        ).
% ensure that the lex bound of S2, when diffed from lub(S1) is no
% less than min(N) elements
:-demon(diff_lex_min_2/4).
diff_lex_min_2(S1,S2,N,_Susp):-
        get_bounds(N,NMin,_),
        lex_set_range(S1,_,Lub),
        lex_min_max(S2,Min,Max),
        lex_set:lex_diff(Lub, Min, MinDiff),
        (length(MinDiff) >= NMin ->
            lex_set:lex_diff(Lub, Max, MaxDiff),
            ( length(MaxDiff) >= NMin ->
                true
            ;
                get_set_attribute(S2, Attr),
                prev_lex_max(Attr,NewLexMax,NewCMax),
                lex_set:lex_diff(Lub, NewLexMax, NewMaxDiff),
                length(NewMaxDiff) >= NMin,
                !,
                set_lex_max(Attr,NewCMax,NewLexMax),
                true
            )
        ;
            get_set_attribute(S2, Attr),
            next_lex_min(Attr, NewLexMin, NewCMin),
            lex_set:lex_diff(Lub, NewLexMin, NewMinDiff),
            length(NewMinDiff) >= NMin,
            !,
            set_lex_min(Attr,NewCMin,NewLexMin),
            true
        ).


%
% all_ordered(Op, SetExpressions)
%
% Equivalent to psoting numerous binary leq/2 or less/2 constraints
% between adjacent pairs of Sets.
%
:-export all_ordered/2.
all_ordered(Op0,SExprs):-
        (foreach(SExpr,SExprs),
         foreach(S,Ss0),
         foreach(_Var,[_|Flags0]),
         % one flag per adjacent pair of setsset, will become ground
         % when both sets are ground AND have been processed atleast once
         fromto(Conditions, In, Out, [])
        do
            seteval(SExpr,S),
            #(S,C),
            In = [S->min_susp, S->max_susp, S->inst, C->min, C->max|Out]
        ),
        reverse(Ss0,RevSs0),
        reverse(Flags0,RevFlags0),
        (Op0==(<) ->
            Ss1=RevSs0,
            Ss2=Ss0,
            Flags1=RevFlags0,
            Flags2=Flags0,
            Op=(>)
        ;Op0==(=<) ->
            Ss1=RevSs0,
            Ss2=Ss0,
            Flags1=RevFlags0,
            Flags2=Flags0,
            Op=(>=)
        ;
            Ss1=Ss0,
            Ss2=RevSs0,
            Flags1=Flags0,
            Flags2=RevFlags0,
            Op=Op0
        ),
        suspend(all_ordered_demon(Op,Ss1,Flags1,Ss2,Flags2,Susp),
                6, Conditions, Susp),
        schedule_suspensions(1,s([Susp])), wake.

:-demon all_ordered_demon/6.
all_ordered_demon((>), SsDesc, FlagsDesc, SsAsc, FlagsAsc, Susp):-
        get_priority(Pri),
        sepia_kernel:set_priority(2),
        (fromto(SsDesc,[S1,S2|SRest],[S2|SRest],[_]),
         foreach(Flag,FlagsDesc)
        do
            (var(Flag) ->
                % process upperbounds
                (var(S1) ->
                    get_set_attribute(S1, Attr1),
                    attr_min_max(Attr1, _LexMin1, LexMax1)
                ;
                    LexMax1=S1
                ),
                (var(S2) ->
                    get_set_attribute(S2, Attr2),
                    Attr2 = int_sets with [card:C2],
                    attr_min_max(Attr2, _LexMin2, LexMax2),
                    (lex_set:lex_less(LexMax2,LexMax1) ->
                        % nothing to do
                        true
                    ;
                        lex_glb_lub(Attr2, Glb2, Lub2),
                        get_bounds(C2,CMin2,CMax2),
                        once(lex_set:lex_prev(LexMax1,Glb2,Lub2,
                                              CMin2,CMax2,NewCMax2,
                                              NewLexMax2)),
                        set_lex_max(Attr2, NewCMax2, NewLexMax2)
                    )
                ;
                    lex_set:lex_less(S2,LexMax1)
                            % donot set flag here, it will be done in the
                            % lowerbound loop below
                )
            ;
                % this ground pair has been processed already
                true
            )
        ),
        (fromto(SsAsc,[S1,S2|SRest],[S2|SRest],[_]),
         foreach(Flag,FlagsAsc), param(FoundVar)
        do
            (var(Flag) ->
                (var(S1) ->
                    FoundVar=1,
                    get_set_attribute(S1, Attr1),
                    attr_min_max(Attr1, LexMin1, _LexMax1)
                ;
                    LexMin1=S1
                ),
                (var(S2) ->
                    FoundVar=1,
                    get_set_attribute(S2,Attr2),
                    Attr2 = int_sets with [card:C2],
                    attr_min_max(Attr2, LexMin2, _LexMax2),
                    (lex_set:lex_less(LexMin1,LexMin2) ->
                        % nothing to do
                        true
                    ;
                        lex_glb_lub(Attr2, Glb2, Lub2),
                        get_bounds(C2,CMin2,CMax2),
                        once(lex_set:lex_next(LexMin1,Glb2,Lub2,
                                              CMin2,CMax2,NewCMin2,
                                              NewLexMin2)),
                        set_lex_min(Attr2, NewCMin2, NewLexMin2)
                    )
                ;
                    lex_set:lex_less(LexMin1,S2),
                    (var(S1) ->
                        true
                    ;
                        Flag=1  % both S1 and S2 are ground
                    )
                )
            ;
                true
            )
        ),
        sepia_kernel:set_priority(Pri),
        (nonvar(FoundVar)->
            true
        ;
            % all sets are ground
            kill_suspension(Susp)
        ),
        wake.
all_ordered_demon((>=), SsDesc, FlagsDesc, SsAsc, FlagsAsc, Susp):-
        get_priority(Pri),
        sepia_kernel:set_priority(2),
        (fromto(SsDesc,[S1,S2|SRest],[S2|SRest],[_]),
         foreach(Flag,FlagsDesc)
        do
            (var(Flag) ->
                % process upperbounds
                (var(S1) ->
                    get_set_attribute(S1, Attr1),
                    attr_min_max(Attr1, _LexMin1, LexMax1)
                ;
                    LexMax1=S1
                ),
                (var(S2) ->
                    get_set_attribute(S2, Attr2),
                    Attr2 = int_sets with [card:C2],
                    attr_min_max(Attr2, _LexMin2, LexMax2),
                    (lex_set:lex_leq(LexMax2,LexMax1) ->
                        % nothing to do
                        true
                    ;
                        lex_glb_lub(Attr2, Glb2, Lub2),
                        get_bounds(C2,CMin2,CMax2),
                        once(lex_set:lex_prev_eq(LexMax1,Glb2,Lub2,
                                              CMin2,CMax2,NewCMax2,
                                              NewLexMax2)),
                        set_lex_max(Attr2, NewCMax2, NewLexMax2)
                    )
                ;
                    lex_set:lex_leq(S2,LexMax1)
                            % donot set flag here, it will be done in the
                            % lowerbound loop below
                )
            ;
                % this ground pair has been processed already
                true
            )
        ),
        (fromto(SsAsc,[S1,S2|SRest],[S2|SRest],[_]),
         foreach(Flag,FlagsAsc), param(FoundVar)
        do
            (var(Flag) ->
                (var(S1) ->
                    FoundVar=1,
                    get_set_attribute(S1, Attr1),
                    attr_min_max(Attr1, LexMin1, _LexMax1)
                ;
                    LexMin1=S1
                ),
                (var(S2) ->
                    FoundVar=1,
                    get_set_attribute(S2,Attr2),
                    Attr2 = int_sets with [card:C2],
                    attr_min_max(Attr2, LexMin2, _LexMax2),
                    (lex_set:lex_leq(LexMin1,LexMin2) ->
                        % nothing to do
                        true
                    ;
                        lex_glb_lub(Attr2, Glb2, Lub2),
                        get_bounds(C2,CMin2,CMax2),
                        once(lex_set:lex_next_eq(LexMin1,Glb2,Lub2,
                                              CMin2,CMax2,NewCMin2,
                                              NewLexMin2)),
                        set_lex_min(Attr2, NewCMin2, NewLexMin2)
                    )
                ;
                    lex_set:lex_leq(LexMin1,S2),
                    (var(S1) ->
                        true
                    ;
                        Flag=1  % both S1 and S2 are ground
                    )
                )
            ;
                true
            )
        ),
        sepia_kernel:set_priority(Pri),
        (nonvar(FoundVar)->
            true
        ;
            % all sets are ground
            kill_suspension(Susp)
        ),
        wake.


%----------------------------------------------------------------------
% User documentation
%----------------------------------------------------------------------

:- comment(author, "Joachim Schimpf, Neng-Fa Zhou, Andrew Sadler").
:- comment(date, "$Date: 2010/07/25 13:29:05 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(desc, html("
    This is a solver for constraints over the domain of finite integer sets.
    </P><P>
    (Ground) integer sets are represented simply as sorted, duplicate-free
    lists of integers e.g.
    <PRE>
    	SetOfThree = [1,3,7]
    	EmptySet = []
    </PRE>

<H3>Set Variables</H3>
    Set variables are variables which can eventually take a ground integer
    set as their value. They are characterized by a lower bound (the set 
    of elements that are definitely in the set) and an upper bound (the
    set of elements that may be in the set). A set variable can be declared
    as follows:
    <PRE>
    	SetVar :: []..[1,2,3,4,5,6,7],
    </PRE>
    Since the lower bound is the empty set, this can be written as
    <PRE>
    	SetVar subset [1,2,3,4,5,6,7],
    </PRE>
    If the lower bound is the empty set and the upper bound is a set
    of consecutive integers, you can also write
    <PRE>
    	intset(SetVar, 1, 7)
    </PRE>

<H3>Set Constraints</H3>
    Most of the usual set operations/relations are provided as constraints:
    <UL>
    <LI>membership
    <LI>non-membership
    <LI>inclusion (subset)
    <LI>equality
    <LI>intersection
    <LI>union
    <LI>difference
    <LI>symmetric difference
    <LI>disjointness
    <LI>cardinality
    </UL>
    as well as a constraint on set weight.  Note that there is no
    complement-constraint because the library has no concept of a set
    universe and cannot represent infinite sets.
    </P><P>
    On most argument positions where sets are expected, set expressions
    are allowed, e.g.
    <PRE>
    Set1 /\\ Set2       % intersection
    Set1 \\/ Set2       % union
    Set1 \\ Set2        % difference
    </PRE>
    as well as references to array elements like Set[3].
    </P>

<H3>Search</H3>
    The insetdomain/4 predicate can be used to enumerate all ground
    instantiations of a set variable, much like indomain/1 in the
    finite-domain case.

<H3>Cooperation with a finite domain solver</H3>
    This library comes in two flavours: lib(fd_sets) which cooperates with
    lib(fd), and lib(ic_sets) which cooperates with lib(ic). This is relevant
    only for those constraints which involve integer variables, e.g. the
    cardinality argument in #/2, the weight argument in weight/3 and the
    booleans in membership_booleans/2. These will be represented as fd-
    or ic-variables respectively.
")).

:- comment(intset/3, [
    summary:"Set is a set containing numbers between Min and Max",
    exceptions:[4:"Min or Max are uninstantiated",
	5:"Set is not a variable or list"],
    args:["Set":"A free variable, set variable or an integer list",
    	"Min":"integer",
    	"Max":"integer"],
    see_also:[intsets/4,(::)/2],
    amode:intset(?,+,+),
    desc:html("<P>
    Unifies Set with a set variable whose lower bound is the empty set
    and whose upper bound is the set of all integers between Min and Max.
    Equivalent to Set::[]..[Min,Min+1,..,Max].
</P>")
]).
:- comment(intsets/4, [
    summary:"Sets is a list of N sets containing numbers between Min and Max",
    exceptions:[4:"Min or Max are uninstantiated"],
    see_also:[intset/3,(::)/2],
    template:"intsets(?Sets, ?N, +Min, +Max)"
]).
:- comment((in)/2, [
    summary:"The integer X is member of the integer set Set",
    args:["X":"an integer or an (integer) variable",
    	"Set":"a set, set variable, or set expression"],
    template:"?X in ?Set"
]).
:- comment((in)/3, [
    summary:"Reified version of the set membership constraint",
    args:["X":"an integer or an (integer) variable",
    	"Set":"a set, set variable, or set expression",
        "Bool":"0, 1 or a boolean variable"
    ],
    see_also:[(in)/2,(notin)/2,membership_booleans/2],
    exceptions:[4:"Set is a free variable without set domain"],
    desc:html("<P>
    This is the reified version of the in/2 constraint. This means that
    the boolean variable Bool reflects the truth of the relation <EM>X in Set</EM>.
    If Bool is 1, then in(X,Set,Bool) is the same as <EM>X in Set</EM>.
    If Bool is 0, then in(X,Set,Bool) is the same as <EM>X notin Set</EM>.
    Otherwise, Bool will be bound to 0 or 1 when <EM>X in Set</EM>
    is known to be false or true, respectively. The latter is only
    guaranteed to be detected after X has become instantiated.
</P><P>
    Note that if one wants to have booleans corresponding to all or most
    of the set domain elements, it will be more efficient to use the
    membership_booleans/2 constraint in place of many in/3 constraints.
</P>"),
    eg:"
    ?- intset(S, 1, 9), in(E, S, B).
    E = E{[-10000000 .. 10000000]}
    S = S{([] .. [1, 2, 3, 4, 5, 6, 7, 8, 9]) : C{[0 .. 9]}}
    B = B{[0, 1]}
    There is 1 delayed goal.

    ?- intset(S, 1, 9), in(E, S, B), B = 1.
    E = E{[1 .. 9]}
    S = S{([] .. [1, 2, 3, 4, 5, 6, 7, 8, 9]) : C{[0 .. 9]}}
    B = 1
    There is 1 delayed goal.

    ?- intset(S, 1, 9), in(E, S, B), E = 3, B = 1.
    S = S{[3] \\/ ([] .. [1, 2, 4, 5, 6, 7, 8, 9]) : C{[1 .. 9]}}
    E = 3
    B = 1

    ?- intset(S, 1, 9), in(E, S, B), E = 3, B = 0.
    S = S{([] .. [1, 2, 4, 5, 6, 7, 8, 9]) : C{[0 .. 8]}}
    E = 3
    B = 0
    Yes (0.00s cpu)

    ?- intset(S, 1, 9), in(E, S, B), E = 3, S includes [2, 3, 4].
    B = 1
    E = 3
    S = S{[2, 3, 4] \\/ ([] .. [1, 5, 6, 7, 8, 9]) : C{[3 .. 9]}}
    There is 1 delayed goal.

    ?- intset(S, 1, 9), in(E, S, B), E = 3, S disjoint [2, 3, 4].
    B = 0
    E = 3
    S = S{([] .. [1, 5, 6, 7, 8, 9]) : C{[0 .. 6]}}
    There is 1 delayed goal.

    ?- in(3, S, 1).
    instantiation fault
    "
]).
:- comment((notin)/2, [
    summary:"The integer X is not a member of the integer set Set",
    args:["X":"an integer or an (integer) variable",
    	"Set":"a set, set variable, or set expression"],
    template:"?X notin ?Set"
]).
:- comment((disjoint)/2, [
    summary:"The integer sets Set1 and Set2 are disjoint",
    args:["Set1":"a set, set variable or set expression",
	"Set2":"a set, set variable or set expression"],
    template:"?Set1 disjoint ?Set2"
]).
:- comment((sameset)/2, [
    summary:"The sets Set1 and Set2 are equal",
    args:["Set1":"a set, set variable or set expression",
	"Set2":"a set, set variable or set expression"],
    template:"?Set1 sameset ?Set2"
]).
:- comment(all_disjoint/1, [
    summary:"Sets is a list of integers sets which are all disjoint",
    template:"all_disjoint(+Sets)"
]).
:- comment((subset)/2, [
    summary:"Set1 is a (non-strict) subset of the integer set Set2",
    args:["Set1":"a set, set variable, free variable or set expression",
	"Set2":"a set, set variable or set expression"],
    see_also:[subsetof/2],
    template:"?Set1 subset ?Set2"
]).
:- comment((subsetof)/2, [
    summary:"Set1 is a (non-strict) subset of the integer set Set2",
    args:["Set1":"a set, set variable, free variable or set expression",
	"Set2":"a set, set variable or set expression"],
    template:"?Set1 subsetof ?Set2",
    eg:"
    	?- X subset [1,2,3].
	X = X{([] .. [1, 2, 3]) : _398{0 .. 3}}

	?- X subsetof [1,2,3], Y subsetof X.
	X = X{([] .. [1, 2, 3]) : _398{0 .. 3}}
	Y = Y{([] .. [1, 2, 3]) : _531{0 .. 3}}

	?- [1,2,3] subsetof Y.
	instantiation fault
    "
]).
:- comment((includes)/2, [
    summary:"Set1 includes (is a superset of) the integer set Set2",
    args:["Set1":"a set, set variable or set expression",
	"Set2":"a set, set variable, free variable or set expression"],
    template:"?Set1 includes ?Set2"
]).
:- comment(union/3, [
    summary:"Set3 is the union of the integer sets Set1 and Set2",
    args:["Set1":"a set, set variable or set expression",
	"Set2":"a set, set variable or set expression",
	"Set3":"a set, set variable, free variable or set expression"],
    template:"union(?Set1, ?Set2, ?Set3)"
]).
:- comment(all_union/2, [
    summary:"SetUnion is the union of all the sets in the list Sets",
    amode:all_union(+,?),
    args:["Sets":"a list of sets, set variables or set expressions",
	"SetUnion":"a set, set variable, free variable or set expression"]
]).
:- comment(intersection/3, [
    summary:"Set3 is the intersection of the integer sets Set1 and Set2",
    args:["Set1":"a set, set variable or set expression",
	"Set2":"a set, set variable or set expression",
	"Set3":"a set, set variable, free variable or set expression"],
    template:"intersection(?Set1, ?Set2, ?Set3)"
]).
:- comment(all_intersection/2, [
    summary:"Intersection is the intersection of all the sets in the list Sets",
    amode:all_intersection(+,?),
    args:["Sets":"a non-empty list of sets, set variables or set expressions",
	"Intersection":"a set, set variable, free variable or set expression"]
]).
:- comment(difference/3, [
    summary:"Set3 is the difference of the integer sets Set1 and Set2",
    args:["Set1":"a set, set variable or set expression",
	"Set2":"a set, set variable or set expression",
	"Set3":"a set, set variable, free variable or set expression"],
    template:"difference(?Set1, ?Set2, ?Set3)"
]).
:- comment(symdiff/3, [
    summary:"Set3 is the symmetric difference of the integer sets Set1 and Set2",
    args:["Set1":"a set, set variable or set expression",
	"Set2":"a set, set variable or set expression",
	"Set3":"a set, set variable, free variable or set expression"],
    template:"symdiff(?Set1, ?Set2, ?Set3)"
]).
:- comment((#)/2, [
    summary:"Card is the cardinality of the integer set Set",
    exceptions:[4:"Set is a variable, but not a set variable"],
    amode:(#(?,?)),
    args:["Set":"a set, set variable or set expression",
	"Card":"a variable, finite domain variable, or integer"],
    desc:html("<P>
    This is a constraint for the cardinality of the set Set.
    Any changes in the potential cardinality of Set will be reflected
    in the finite domain of Card. Restricting the domain of Card will
    restrict the potential cardinality of Set.
</P>")
]).
:- comment(weight/3, [
    summary:"According to the array of element weights, the weight of set Set is Weight",
    amode:weight(?,++,?),
    args:["Set":"a set, set variable or set expression",
	"ElementWeights":"an array (structure) of non-negative integers",
	"Weight":"an integer variable or integer"
    ],
    desc:html("<P>
    Set is constrained to be a set of integers between 1 and the size of
    the array ElementWeights. Weight is an integer domain variable which
    is constrained to the sum of the weights of Set's elements. The weight
    of a set element is determined by the array element in ElementWeights
    whose index corresponds to the set element.
</P><P>
    Changes to the set domain will affect the domain of Weight. There is currently
    no reverse propagation. This may change in a future release.
</P>")
]).
:- comment(potential_members/2, [
    summary:"List is the list of elements of whose membership in Set is currently uncertain",
    exceptions:[4:"Set is a variable, but not a set variable"],
    amode:potential_members(?,-),
    args:[
	"Set":"a set or set variable",
	"List":"variable, will be unified with a list"
    ],
    eg:"
    	?- S :: [2,4]..[1,2,3,4,5], potential_members(S, P).
	S = S{[2, 4] \\/ ([] .. [1, 3, 5]) : _{[2 .. 5]}}
	P = [1, 3, 5]
    "
]).
:- comment(insetdomain/4, [
    summary:"Instantiate Set to a possible value",
    amode:insetdomain(?,?,?,?),
    args:[
	"Set":"a set or set variable",
	"CardSel":"atom or variable",
	"ElemSel":"atom, structure or variable",
	"Order":"atom or variable"
    ],
    resat:yes,
    exceptions:[4:"Set is a variable, but not a set variable"],
    see_also:[refine/1,potential_members/2,
	    sbds_try/2,sbds_initialise/4,sbds_initialise/5],
    desc:html("<P>
    This predicate instantiates a set variable to a possible value,
    according to its domain.  The predicate backtracks over all
    possible set instantiations.  The three option arguments allow to
    choose between a number of different enumeration orders. Giving a
    variable as option argument will select the default.
</P><P>
    The <B>CardSel</B> argument determines whether the sets are enumerated
    according to their cardinality. It can take the following values:
<DL>
<DT><B>any</B> (default)</DT>
    <DD>the sets are not enumerated in a particular cardinality order</DD>
<DT><B>increasing</B></DT>
    <DD>the sets are enumerated with increasing cardinality, ie. small
    sets are tried first</DD>
<DT><B>decreasing</B></DT>
    <DD>the sets are enumerated with decreasing cardinality, ie. large
    sets are tried first</DD>
</DL>
</P><P>
    The <B>ElemSel</B> argument determines which potential set elements
    are considered first for inclusion or exclusion. It can take the
    following values:
<DL>
<DT><B>small_first</B> (default)</DT>
    <DD>small set elements (small numbers) are considered first</DD>
<DT><B>big_first</B></DT>
    <DD>big set elements (big numbers) are considered first</DD>
<DT><B>random</B></DT>
    <DD>potential set elements are considered in random order</DD>
<DT><B>heavy_first(Weights)</B></DT>
    <DD>heavy set elements (according to Weight array) are considered first</DD>
<DT><B>light_first(Weights)</B></DT>
    <DD>light set elements (according to Weight array) are considered first</DD>
</DL>
</P><P>
    The <B>Order</B> argument determines whether it is first tried to make
    the selected potential element a set member, or whether to exclude it
    first.  The argument can take the following values:
<DL>
<DT><B>in_notin</B> (default)</DT>
    <DD>try inclusion first, then exclusion</DD>
<DT><B>notin_in</B></DT>
    <DD>try exclusion first, then inclusion</DD>
<DT><B>sbds</B></DT>
    <DD>uses sbds_try/2 to include or exclude an element in or from a set
    (this is for use in conjunction with the SBDS library, and whether
    inclusion or exclusion is tried first depends on the \"fix pred\"
    specified in the prior call to sbds_initialise/4 or sbds_initialise/5)</DD>
</DL>
</P><P>
    Note that there are many different enumeration strategies for a set
    variable, and insetdomain/4 only allows a limited number of them.
    For an actual application, it might be more appropriate to choose a
    problem-specific enumeration order. This can be programmed easily.
    As a guideline, here is the code for insetdomain with the default
    options:
<PRE>
    insetdomain(Set, _, _, _) :-
    	nonvar(Set).
    insetdomain(Set, any, small_first, in_notin) :-
    	var(Set),
    	potential_members(Set, PotentialElements),
	PotentialElements = [Element|_],
	(
	    Element in Set
	;
	    Element notin Set
	),
	insetdomain(Set, any, small_first, in_notin).
</PRE>
"),
eg:"
?-  X::[]..[1,2,3], insetdomain(X,_,_,_), writeln(X), fail.
[1, 2, 3]
[1, 2]
[1, 3]
[1]
[2, 3]
[2]
[3]
[]

no (more) solution.
?-  X::[]..[1,2,3], insetdomain(X,increasing,_,_), writeln(X), fail.
[]
[1]
[2]
[3]
[1, 2]
[1, 3]
[2, 3]
[1, 2, 3]

no (more) solution.
?-  X::[]..[1,2,3], insetdomain(X,_,big_first,_), writeln(X), fail.
[1, 2, 3]
[2, 3]
[1, 3]
[3]
[1, 2]
[2]
[1]
[]

no (more) solution.
?-  X::[]..[1,2,3], insetdomain(X,_,_,notin_in), writeln(X), fail.
[]
[3]
[2]
[2, 3]
[1]
[1, 3]
[1, 2]
[1, 2, 3]

no (more) solution.
?-  X::[]..[1,2,3],
	insetdomain(X, increasing, heavy_first([](2,9,4,7)), _),
	writeln(X), fail.
[]
[2]
[3]
[1]
[2, 3]
[1, 2]
[1, 3]
[1, 2, 3]

no (more) solution.
"
]).
:- comment((::)/2, [
    summary:"Set is an integer set within the given bounds",
    template:"?Set :: ++Lwb..++Upb",
    args:["Set":"A free variable, set variable or an integer list",
    	"Lwb..Upb":"Structure holding two lists of integers"],
    fail_if:"Lwb is not a sublist of Upb",
    exceptions:[5:"Set is not a variable or list, or Lwb..Upb is not a ../2 structure"],
    see_also:[in_set_range/2,intset/3,intsets/4],
    desc:html("<P>
    Lwb and Upb are two lists of integers. Lwb must be a sublist of
    Upb. Set is unified with a set variable whose lower bound is the
    set of list elements of Lwb, and whose upper bound is the set of
    list elements of Upb.
</P>")
]).
:- comment((in_set_range)/2, [
    summary:"Set is an integer set within the given bounds",
    template:"?Set in_set_range ++Lwb..++Upb",
    args:["Set":"A free variable, set variable, array reference, or an integer list",
    	"Lwb..Upb":"Structure holding two lists of integers"],
    fail_if:"Lwb is not a sublist of Upb",
    exceptions:[5:"Set is not a variable, array reference, or list, or Lwb..Upb is not a ../2 structure"],
    see_also:[intset/3,intsets/4],
    desc:html("<P>
    Lwb and Upb are two lists of integers. Lwb must be a sublist of
    Upb. Set is unified with a set variable whose lower bound is the
    set of list elements of Lwb, and whose upper bound is the set of
    list elements of Upb.
</P>")
]).
:- comment(set_range/3, [
    summary:"Lwb and Upb are the current lower and upper bounds on Set",
    amode:set_range(?,-,-),
    args:["Set":"A variable or an integer list",
    	"Lwb":"List of integers",
    	"Upb":"List of integers"],
    fail_if:"Set is a variable, but not a set variable",
    desc:html("<P>
    Returns two sorted, duplicate-free lists of integers which represent
    the lower and upper bound of the set variable (or the ground set) Set.
    The predicate can also be used as a test for set-variables, since it
    fails for any other (in particular domain-less) variables.
</P>")
]).
:- comment(is_solver_var/1, [
    summary:"Succeeds if Term is a set variable",
    args:["Term":"A term"],
    fail_if:"Set is not a variable, or a non-set variable"
]).
:- comment(is_solver_type/1, [
    summary:"Succeeds if Term is a ground set or a set variable",
    args:["Term":"A term"],
    fail_if:"Set is neither a set nor a set variable",
    desc:html("<P>
    Succeeds if Term is either a set (represented as a list) or a set
    variable. For efficiency reasons, lists are not checked for being
    valid set representations (i.e. strictly sorted lists of integers),
    the predicate succeeds for any list.
</P>")
]).
:- comment(get_set_attribute/2, [
    summary:"Get the set-attribute corresponding to Set",
    args:["Set":"A ground set or a set variable",
    	"Attr":"A variable, will be bound to a set-attribute structure"
    ],
    amode:get_set_attribute(?,-),
    desc:html("<P>
    Gets the set-attribute of a set-variable (or computes an attribute
    structure describing a ground set as if it was a variable).
</P>")
]).
:- comment(membership_booleans/2, [
    summary:"BoolArr is an array of booleans describing Set",
    amode:membership_booleans(?,?),
    args:["Set":"a set, set variable, free variable or set expression",
    	"BoolArr":"A variable or an array (structure) of boolean variables"],
    desc:html("<P>
    This constraint maintains the correspondence between a finite set and
    an array of booleans (0/1 variables). The set is constrained to contain
    those (and only those) integers I between 1 and N (the size of the array)
    where the corresponding array element is 1.
</P><P>
    Operationally, setting a array element I to 0 will exclude the element
    I from Set's domain. Setting I to 1 will include I in Set's lower bound.
    Adding and removing elements from Set will be reflected in the
    corresponding boolean being instantiated to 1 or 0 respectively.
</P><P>
    When called with BoolArr being a free variable, an array will be created
    whose arity is at least as big as the largest potential set member.
    Set members smaller than 1 will be excluded from the set.
    When called with BoolArr being instantiated to an array, set members
    smaller than 1 or bigger than the array size will be excluded from
    the set.
</P>"),
    eg:"
    ?- S::[2,4]..[1,2,3,4,5], membership_booleans(S,B).

    S = S{[2, 4] \\/ ([] .. [1, 3, 5]) : _{[2 .. 5]}}
    B = [](_{[0, 1]}, 1, _{[0, 1]}, 1, _{[0, 1]})

    Delayed goals:
	...


    ?- membership_booleans(S, [](0,1,0,1,B5)).

    S = S{[2, 4] \\/ ([] .. [5]) : _{[2, 3]}}
    B5 = B5{[0, 1]}

    Delayed goals:
	...
    "
]).

