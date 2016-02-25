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
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Solver for constraints over finite sets of integers
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: generic_sets.ecl,v 1.6 2010/04/04 14:05:09 jschimpf Exp $
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
	op(700, xfx, subsetof).

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
	all_disjoint/1,		% all_disjoint(+Sets)
	all_union/2,		% all_union(+Sets, +Union)
	all_intersection/2,	% all_intersection(+Sets, +Intersection)
	(includes)/2,		% ?Set1 includes Set2	superset
	(subset)/2,		% ?Set1 subset Set2	subset
	(subsetof)/2,		% ?Set1 subsetof Set2	subset
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
	is_exact_solver_var/1,	% is_exact_solver_var(?Thing)
	is_solver_type/1,	% is_solver_type?Thing)
	msg/3,			% msg(?Set, ?Set, -Set)
	potential_members/2,	% potential_members(?Set, -UpbMinusLwb)
	set_range/3.		% set_range(?Set, -Lwb, -Upb) [conjunto compat]


:- lib(ordset).

:- meta_attribute(int_sets, [
	print:print_setvar/2,
	unify:unify_sets/3,
	test_unify:test_unify_sets/2,
	suspensions:suspensions_set/3,
	delayed_goals_number:delayed_goals_number_set/2,
	copy_term:copy_term_set/2,
	compare_instances:compare_instances_set/3
    ]).

:- export struct(int_sets(
	    dom,	% set domain representation
	    off,	% integer
	    lcard,	% integer
	    ucard,	% integer
	    added,	% notification send-port
	    removed,	% notification send-port
	    add,	% suspension list
	    rem,	% suspension list
	    card,	% fd/ic variable
	    booleans,	% vector of fd/ic variables
%	    invoc,	% integer
	    value	% set variable
    	)).

:- comment(struct(int_sets), [
    summary:"Attribute structure for set variables (and constants)",
    fields:[
	    "dom":"set domain representation (array of booleans)",
	    "off":"offset between set element and corresponding array index (integer)",
	    "lcard":"lower bound cardinality (integer)",
	    "ucard":"upper bound cardinality (integer)",
	    "added":"notification send-port for lower bound increases",
	    "removed":"notification send-port for upper bound decreases",
	    "add":"suspension list woken when lower bound increases",
	    "rem":"suspension list woken when upper bound decreases",
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
%trace_set(Port, Elem, X{int_sets:int_sets{invoc:I}}) ?-
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
	Receiver = rec{susp:Susp},
	open_receiver(Pos, Attr, port of rec, Receiver).

init_event_receiver(Pos, Attr, Susp, InitialEvents, InitialTail, Receiver) :-
	Receiver = rec{susp:Susp},
	open_receiver_init(Pos, Attr, InitialEvents, InitialTail, port of rec, Receiver).

receive_events(Receiver, Events) :-
	nonvar(Receiver),
	receive_notifications(port of rec, Receiver, Events, Status),
	kill_receiver_suspension(Status, Receiver).

kill_receiver_suspension(closed, rec{susp:Susp}) :-
    	kill_suspension(Susp).
kill_receiver_suspension(open, _).

kill_receiver_suspension(rec{susp:Susp}) :-
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

intset(_{int_sets:Attr}, MinUniv, MaxUniv) ?- nonvar(Attr), !,
	( foreach(I,UnivList), for(I,MinUniv,MaxUniv) do true ),
	intset_intersect_upb(UnivList, Attr, _SuspAttr).
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
	intset_union_lwb(Lwb, SetAttr, _SuspAttr),
	intset_intersect_upb(Upb, SetAttr, _SuspAttr).
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
	SetAttr = int_sets{value:X},
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
	Attr = int_sets{card:C},
	lset_to_list(Attr, LwbList),
	( LwbList = [] ->
	    uset_to_list(Attr, UpbList),
	    Pretty = (LwbList..UpbList):C
	;
	    uldiff_to_list(Attr, Maybe),
	    Pretty = (LwbList \/ ([]..Maybe)):C
	).

% fail for non-set variable (so it can be used to test for
% set variables, compatible with conjunto)
set_range(_{int_sets:Attr}, LwbList, UpbList) ?-
	nonvar(Attr),
	!,
	lset_to_list(Attr, LwbList),
	uset_to_list(Attr, UpbList).
set_range(Set, LwbList, UpbList) :-
	nonvar(Set),
	get_set_attribute(Set, Attr),
	lset_to_list(Attr, LwbList),
	uset_to_list(Attr, UpbList).

set_card(_{int_sets:int_sets{card:C}}, Card) ?- Card=C.
set_card(List, Card) :- nonvar(List), length(List, Card).

is_solver_var(_{int_sets:int_sets{}}) ?- true.

is_exact_solver_var(_{int_sets:int_sets{}}) ?- true.

is_solver_type(_{int_sets:int_sets{}}) ?- true.
is_solver_type([]) ?- true.
is_solver_type([_|_]) ?- true.

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
	SetAttr = int_sets{value:Set},
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
		SetAttr = int_sets{value:Set},
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
	Y0 subsetof X0.

X0 subset Y0 :-
	X0 subsetof Y0.

X0 subsetof Y0 :-
	seteval(Y0, Y),
	seteval(X0, X),
	( nonvar(Y) ->
	    % special case: subset of ground set (avoid delayed goals)
	    impose_set_bounds(X, [], Y)
	;
	    get_set_attribute(Y, YAttr),
	    get_new_set_attribute(X, [YAttr], XAttr),

	    suspend(add_demon(YAttr, Events1), 3, X->add, Susp1),
	    suspend(remove_demon(XAttr, Events2), 3, Y->rem, Susp2),

	    initial_member_events(XAttr, Susp1, Events1),
	    initial_nonmember_events(YAttr, XAttr, Susp2, Events2),
	    schedule_suspensions(1,s([Susp1,Susp2])), wake,

	    verify check_instantiation((X subset Y))
	).

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
% Unification handler (Y gets bound)
%----------------------------------------------------------------------

unify_sets(X, YAttr, YSuspAttr) :- var(YAttr),
	unify_any_other(X, YAttr, YSuspAttr).
unify_sets(X, YAttr, YSuspAttr) :- nonvar(YAttr),
	unify_any_set(X, YAttr, YSuspAttr).

unify_any_other(_X{int_sets:XAttr}, YAttr, YSuspAttr) ?- !,
	unify_meta_other(XAttr, YAttr, YSuspAttr).
unify_any_other(_X, _YAttr, _YSuspAttr).			% nonvar_other

unify_meta_other(XAttr, _YAttr, _YSuspAttr) :- var(XAttr).	% other_other
unify_meta_other(XAttr, _YAttr, YSuspAttr) :- nonvar(XAttr),	% set_other
	( nonvar(YSuspAttr) -> 
	    schedule_suspensions(constrained of suspend, YSuspAttr)
	;
	    true
	).

unify_any_set(_X{int_sets:XAttr}, YSetAttr, YSuspAttr) ?-
	unify_meta_set(XAttr, YSetAttr, YSuspAttr).
unify_any_set(X, YSetAttr, _YSuspAttr) :- nonvar(X),
	call_priority((
		% complete the bounds and make them equal
	    intset_union_lwb(X, YSetAttr, _),
	    intset_intersect_upb(X, YSetAttr, _),
		% wake both bounds-lists, even if one bound was not touched.
		% this gives the goals a chance to kill themselves
	    close_sender(added of int_sets, YSetAttr),
	    close_sender(removed of int_sets, YSetAttr),
	    schedule_suspensions(add of int_sets, YSetAttr),
	    schedule_suspensions(rem of int_sets, YSetAttr)
	), 2).

unify_meta_set(XAttr, YSetAttr, _YSuspAttr) :- var(XAttr), XAttr=YSetAttr.
unify_meta_set(XSetAttr, YSetAttr, YSuspAttr) :- nonvar(XSetAttr),
	XSetAttr = int_sets{card:Card,value:X},
	YSetAttr = int_sets{card:Card},

	% make the lwbs and upbs identical and wake the necessary
	% suspensions on X and Y
	lset_to_list(XSetAttr, XLwbList),
	lset_to_list(YSetAttr, YLwbList),
	uset_to_list(XSetAttr, XUpbList),
	uset_to_list(YSetAttr, YUpbList),
	call_priority((
	    intset_union_lwb(YLwbList, XSetAttr, _),	% make the lwbs equal
	    intset_union_lwb(XLwbList, YSetAttr, YSuspAttr),
	    intset_intersect_upb(YUpbList, XSetAttr, _), % make the upbs equal
	    intset_intersect_upb(XUpbList, YSetAttr, YSuspAttr),

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
	    init_event_receiver(added of int_sets, YSetAttr, Susp2, YAddEvents),
	    init_event_receiver(removed of int_sets, YSetAttr, Susp2, YRemEvents),
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
	YSetAttr = int_sets{card:YCard},
	lset_to_list(YSetAttr, YLwbList),
	ord_subset(YLwbList, X),
	uset_to_list(YSetAttr, YUpbList),
	ord_subset(X, YUpbList),
	length(X, XCard),
	not not_unify(XCard, YCard).

test_unify_meta_set(XSetAttr, _YSetAttr) :- var(XSetAttr).
test_unify_meta_set(XSetAttr, YSetAttr) :- nonvar(XSetAttr),
	XSetAttr = int_sets{card:XCard},
	YSetAttr = int_sets{card:YCard},
	not not_unify(XCard, YCard),
	lset_to_list(XSetAttr, XLwbList),
	uset_to_list(YSetAttr, YUpbList),
	ord_subset(XLwbList, YUpbList),
	lset_to_list(YSetAttr, YLwbList),
	uset_to_list(XSetAttr, XUpbList),
	ord_subset(YLwbList, XUpbList).


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
% Most Specific Generalisation
%----------------------------------------------------------------------

msg(X, Y, G) :-
	set_range(X, XL, XU),
	set_range(Y, YL, YU),
	ord_intersection(XL, YL, L),
	ord_union(XU, YU, U),
	set_card(X, XCard),
	set_card(Y, YCard),
	solver_module:msg(XCard, YCard, Card),
	SetAttr = int_sets{card:Card},
	intset_domain_from_lists(SetAttr, L, U),
	add_set_attribute(G, SetAttr).


%----------------------------------------------------------------------
% Other handlers
%----------------------------------------------------------------------

suspensions_set(_{int_sets:Attr}, Susps, Susps0) ?-
	( var(Attr) ->
	    Susps=Susps0
	;
	    Attr = int_sets{add:Add, rem:Rem},
	    Susps = [Add,Rem|Susps0]
	).


delayed_goals_number_set(_{int_sets:int_sets{add:Add,rem:Rem}}, N) ?- !,
	count_active_suspensions(Add, 0, N1),
	count_active_suspensions(Rem, N1, N).
delayed_goals_number_set(_, 0).
	
    count_active_suspensions([Susp|Susps], N0, N) ?- !,
	( is_suspension(Susp) -> N1 is N0 + 1 ; N1 = N0 ),
	count_active_suspensions(Susps, N1, N).
    count_active_suspensions(_, N, N).


copy_term_set(_{int_sets:int_sets{dom:Dom,off:Off,lcard:LC,ucard:UC,card:Card}}, Copy) ?- !,
	copy_term(Dom-Card, DomCopy-CardCopy),
	Attr = int_sets{dom:DomCopy,off:Off,lcard:LC,ucard:UC,
				card:CardCopy,value:Copy},
	open_sender(added of int_sets, Attr),
	open_sender(removed of int_sets, Attr),
	init_suspension_list(add of int_sets, Attr),
	init_suspension_list(rem of int_sets, Attr),
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
	seteval(Set0, Set), get_set_attribute(Set, SetAttr),
	SetAttr = int_sets{card:C},
	( var(C) ->
	    suspend(card_demon(SetAttr), 3, [C->inst])
	;
	    card_demon(SetAttr)
	).

	% woken on C->inst
card_demon(SetAttr) :-
	SetAttr = int_sets{card:C,value:Set,lcard:LCard,ucard:UCard},
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
	arity(WeightArray, N),
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
	schedule_suspensions(1,s([Susp])), wake.

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
	    XAttr = int_sets{value:X},
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
	    arity(BoolArr, Max),
	    intset(Set, 1, Max)
	; set_min_max(Set, MinS, MaxS) ->
	    MinS >= 1,
	    Max = MaxS,
	    functor(BoolArr, [], Max)
	;
	    error(4, membership_booleans(Set, BoolArr))
	),
	get_set_attribute(Set, SetAttr),
	SetAttr = int_sets{booleans:Booleans},
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
	    label_element(X, Order, Elem),
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
    valid_order_sel(gap_sbds_in_notin).
    valid_order_sel(gap_sbds_notin_in).
    valid_order_sel(gap_sbdd_in_notin).
    valid_order_sel(gap_sbdd_notin_in).

label_cardinality(_X, any).
label_cardinality(X, increasing) :-
	#(X,C),
	indomain(C).
label_cardinality(X, decreasing) :-
	#(X,C),
	RC #= 1-C,
	indomain(RC).

label_element(X, in_notin, Elem) :- Elem in X.
label_element(X, in_notin, Elem) :- Elem notin X.
label_element(X, notin_in, Elem) :- Elem notin X.
label_element(X, notin_in, Elem) :- Elem in X.
label_element(X, sbds, Elem) :- sbds_module:sbds_try(X, Elem). % Uses order from SBDS.
label_element(X, gap_sbds_in_notin, Elem) :- ic_gap_sbds:sbds_try_set(X, Elem, 1).
label_element(X, gap_sbds_notin_in, Elem) :- ic_gap_sbds:sbds_try_set(X, Elem, 0).
label_element(X, gap_sbdd_in_notin, Elem) :- ic_gap_sbdd:sbdd_try_set(X, Elem, 1).
label_element(X, gap_sbdd_notin_in, Elem) :- ic_gap_sbdd:sbdd_try_set(X, Elem, 0).

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


intset_union_lwb(Elems, SetAttr, SuspAttr) :-
	( foreach(E,Elems), param(SetAttr,DomainChanged) do
	    intset_add_nowake(E, SetAttr, DomainChanged)
	),
	wake_if_domain_changed(SetAttr, SuspAttr, add of int_sets, DomainChanged).

intset_intersect_upb(List, SetAttr, SuspAttr) :-
	uset_to_list(SetAttr, UpbList),
	ord_subtract(UpbList, List, RemoveList),
	( foreach(E,RemoveList), param(SetAttr,DomainChanged) do
	    intset_remove_nowake(E, SetAttr, DomainChanged)
	),
	wake_if_domain_changed(SetAttr, SuspAttr, rem of int_sets, DomainChanged).

    % Wake Which-list and constrained-list if DomainChanged.
    % If a SuspAttr argument is given, that means the value-field
    % in the intset-attribute is invalid and should not be used!
    wake_if_domain_changed(_Attr, _SuspAttr, _Which, DomainChanged) :-
	var(DomainChanged).
    wake_if_domain_changed(Attr, SuspAttr, Which, DomainChanged) :-
	nonvar(DomainChanged),
	schedule_suspensions(Which, Attr),
	( var(SuspAttr) ->
	    Attr = int_sets{value:X},
	    notify_constrained(X)
	;
	    schedule_suspensions(constrained of suspend, SuspAttr)
	),
	wake.



intset_add(Elem, Attr) :-
	intset_add_nowake(Elem, Attr, DomainChanged),
	wake_if_domain_changed(Attr, add of int_sets, DomainChanged).

intset_add_nowake(Elem, Attr, DomainChanged) :-
	integer(Elem),
	Attr = int_sets{dom:Map,off:Offset,lcard:LCard,value:X,card:C},
	I is Elem - Offset,
	I >= 1, I =< arity(Map),
	arg(I, Map, Bit),
	( var(Bit) ->
	    Bit = 1,
	    DomainChanged = 1,
	    LCard1 is LCard+1,
	    setarg(lcard of int_sets, Attr, LCard1),
	    trace_set('ADD_ELEM', Elem, X),
	    send_notification(added of int_sets, Attr, Elem),
	    impose_min(C, LCard1),		%%% may wake
	    ( C == LCard1 ->
	    	lset_to_list(Attr, List),
		X = List			% may wake
	    ;
		true
	    )
	;
	    Bit == 1
	).


intset_remove(Elem, Attr) :-
	intset_remove_nowake(Elem, Attr, DomainChanged),
	wake_if_domain_changed(Attr, rem of int_sets, DomainChanged).

intset_remove_nowake(Elem, Attr, DomainChanged) :-
	integer(Elem),
	Attr = int_sets{dom:Map,off:Offset,ucard:UCard,value:X,card:C},
	I is Elem - Offset,
	arity(Map, Max),
	( I < 1 ->
	    true
	; I > Max ->
	    true
	;
	    arg(I, Map, Bit),
	    ( var(Bit) ->
		Bit = 0,
		DomainChanged = 1,
		UCard1 is UCard-1,
		setarg(ucard of int_sets, Attr, UCard1),
		trace_set('REM_ELEM', Elem, X),
		send_notification(removed of int_sets, Attr, Elem),
		impose_max(C, UCard1),		%%% may wake
		( C == UCard1 ->
		    uset_to_list(Attr, List),
		    X = List			% may wake
		;
		    true
		)
	    ;
		Bit == 0
	    )
	).


% Wake Which-list and constrained-list if DomainChanged.
wake_if_domain_changed(_Attr, _Which, DomainChanged) :-
	var(DomainChanged).
wake_if_domain_changed(Attr, Which, DomainChanged) :-
	nonvar(DomainChanged),
	Attr = int_sets{value:X},
	( var(X) ->
	    schedule_suspensions(Which, Attr),
	    notify_constrained(X),
	    wake
	;
	    true	% X instantiated: unify handler has done the waking
	).


% intset_domain(?,+,+)
intset_domain(Attr, L, U) :-
	Attr = int_sets{dom:Map,off:Offset,lcard:0,ucard:N,card:C,value:V},
	N is max(0,U-L+1),
	functor(Map, [], N),
	( N = 0 ->
	    C = 0,
	    V = [],
	    close_sender(added of int_sets, Attr),
	    close_sender(removed of int_sets, Attr)
	;
	    solver_module:(C::0..N),	%%%
	    open_sender(added of int_sets, Attr),
	    open_sender(removed of int_sets, Attr)
	),
	init_suspension_list(add of int_sets, Attr),
	init_suspension_list(rem of int_sets, Attr),
	Offset is L-1.

% intset_domain_from_lists(?,+,+)
% (may be invoked with an Attr structure that already has a card-variable!)
intset_domain_from_lists(Attr, [], []) :- !,
	intset_domain(Attr, 1, 0).
intset_domain_from_lists(Attr, LowerList, UpperList) :-
	sort(UpperList, UpperSorted),
	sort(LowerList, LowerSorted),
	ord_subset(LowerSorted, UpperSorted),
	create_lset_uset(LowerSorted, UpperSorted, Attr).

    create_lset_uset(LowerSorted, UpperSorted, Attr) :-
	UpperSorted = [MinUniv|_], once append(_, [MaxUniv], UpperSorted),
	intset_domain(Attr, MinUniv, MaxUniv),
	(
	    for(I,MinUniv,MaxUniv),
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
	Attr = int_sets{lcard:LCard,ucard:UCard,card:C,value:Value},
	( LCard == UCard ->
	    C = LCard,
	    close_sender(added of int_sets, Attr),
	    close_sender(removed of int_sets, Attr),
	    Value = LowerSorted
	; 
	    solver_module:(C :: LCard..UCard)
	).

lset_member(Elem, Attr) :- var(Elem), !,
	Attr = int_sets{dom:Map,off:Offset},
	arity(Map, Arity),
	between(1, Arity, 1, I),
	arg(I, Map, Bit),
	Bit == 1,
	Elem is I + Offset.
lset_member(Elem, Attr) :- integer(Elem), !,
	Attr = int_sets{dom:Map,off:Offset},
	I is Elem - Offset,
	I >= 1, I =< arity(Map),
	arg(I, Map, Bit),
	Bit == 1.
lset_member(Elem, Attr) :-
	error(5, lset_member(Elem, Attr)).

uldiff_memberchk(Elem, Attr) :- integer(Elem),
	Attr = int_sets{dom:Map,off:Offset},
	I is Elem - Offset,
	I >= 1, I =< arity(Map),
	arg(I, Map, Bit),
	var(Bit).

uset_member(Elem, Attr) :- var(Elem), !,
	Attr = int_sets{dom:Map,off:Offset},
	arity(Map, Arity),
	between(1, Arity, 1, I),
	arg(I, Map, Bit),
	Bit \== 0,
	Elem is I + Offset.
uset_member(Elem, Attr) :- integer(Elem), !,
	Attr = int_sets{dom:Map,off:Offset},
	I is Elem - Offset,
	I >= 1, I =< arity(Map),
	arg(I, Map, Bit),
	Bit \== 0.
uset_member(Elem, Attr) :-
	error(5, uset_member(Elem, Attr)).

lset_nonmember(Elem, Attr) :-
	Attr = int_sets{dom:Map,off:Offset},
	I is Elem - Offset,
	( I < 1 -> true
	; I > arity(Map) -> true
	;
	    arg(I, Map, Bit),
	    Bit \== 1
	).

uset_nonmember(Elem, Attr) :-
	Attr = int_sets{dom:Map,off:Offset},
	I is Elem - Offset,
	( I < 1 -> true
	; I > arity(Map) -> true
	;
	    arg(I, Map, Bit),
	    Bit == 0
	).

uset_remove_member(Elem, Attr) :-
	Attr = int_sets{dom:Map,off:Offset,ucard:UCard},
	I is Elem - Offset,
	arg(I, Map, 0),
	UCard1 is UCard-1,
	setarg(ucard of int_sets, Attr, UCard1).

lset_add_nonmember(Elem, Attr) :-
	Attr = int_sets{dom:Map,off:Offset,lcard:LCard},
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

uset_min_max(int_sets{dom:Map,off:Offset}, Min, Max) ?-
	Min is Offset+1,
	Max is Offset+arity(Map).


lset_card(int_sets{lcard:LCard}, Card) ?- Card=LCard.

uset_card(int_sets{ucard:UCard}, Card) ?- Card=UCard.

uset_to_list(int_sets{dom:Map,off:Offset}, Elements) :-
	arity(Map, Arity),
	( for(I,1,Arity), fromto(Elements,E1,E0,[]), param(Map,Offset) do
	    arg(I, Map, Bit),
	    ( Bit == 0 -> E1=E0 ; Elem is I+Offset, E1=[Elem|E0] )
	).

lset_to_list(Attr, Elements) :-
	lset_to_list(Attr, Elements, []).

lset_to_list(int_sets{dom:Map,off:Offset}, Elements, Elements0) :-
	arity(Map, Arity),
	( for(I,1,Arity), fromto(Elements,E1,E0,Elements0), param(Map,Offset) do
	    arg(I, Map, Bit),
	    ( Bit == 1 -> Elem is I+Offset, E1=[Elem|E0] ; E1=E0 )
	).

uldiff_to_list(int_sets{dom:Map,off:Offset}, Elements) :-
	arity(Map, Arity),
	( for(I,1,Arity), fromto(Elements,E1,E0,[]), param(Map,Offset) do
	    arg(I, Map, Bit),
	    ( var(Bit) -> Elem is I+Offset, E1=[Elem|E0] ; E1=E0 )
	).

uldiff_smallest(Attr, Elem) :-
	Attr = int_sets{dom:Map,off:Offset},
	arity(Map, Arity),
	between(1, Arity, 1, I),
	arg(I, Map, Bit),
	var(Bit),
	!,
	Elem is I + Offset.

uldiff_biggest(Attr, Elem) :-
	Attr = int_sets{dom:Map,off:Offset},
	arity(Map, Arity),
	between(Arity, 1, -1, I),
	arg(I, Map, Bit),
	var(Bit),
	!,
	Elem is I + Offset.

uldiff_random(Attr, Elem) :-
	Attr = int_sets{dom:Map,off:Offset,ucard:UCard,lcard:Lcard},
	Rand is 1 + (random mod (UCard-Lcard)),
	( fromto(Rand, R0, R1, 0), count(I,1,RandI), param(Map) do
	    arg(I, Map, Bit),
	    ( var(Bit) -> R1 is R0-1 ; R1=R0 )
	),
	Elem is RandI + Offset.

uldiff_lightest(Attr, Weights, Elem) :-
	Attr = int_sets{dom:Map,off:Offset,ucard:UCard,lcard:Lcard},
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
	Attr = int_sets{dom:Map,off:Offset,ucard:UCard,lcard:Lcard},
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
	merge(0, <, SignificantElements1, SignificantElements2, SignificantElements),
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


%----------------------------------------------------------------------
% User documentation
%----------------------------------------------------------------------

:- comment(author, "Joachim Schimpf, Neng-Fa Zhou").
:- comment(date, "$Date: 2010/04/04 14:05:09 $").
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
	    sbds_try/2,sbds_initialise/4,sbds_initialise/5,
	    sbds_try_set/3, library(ic_gap_sbds),
	    sbdd_try_set/3, library(ic_gap_sbdd)],
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
    (this is for use in conjunction with the classic SBDS library
    (lib(ic_sbds) or lib(fd_sbds)), and whether inclusion or exclusion is
    tried first depends on the \"fix pred\" specified in the prior call to
    sbds_initialise/4 or sbds_initialise/5)</DD>
<DT><B>gap_sbds_in_notin</B></DT>
    <DD>try inclusion first, then exclusion, using sbds_try_set/3 (this is
    for use in conjunction with the GAP-based SBDS library lib(ic_gap_sbds))</DD>
<DT><B>gap_sbds_notin_in</B></DT>
    <DD>try exclusion first, then inclusion, using sbds_try_set/3 (this is
    for use in conjunction with the GAP-based SBDS library lib(ic_gap_sbds))</DD>
<DT><B>gap_sbdd_in_notin</B></DT>
    <DD>try inclusion first, then exclusion, using sbdd_try_set/3 (this is
    for use in conjunction with the GAP-based SBDD library lib(ic_gap_sbdd))</DD>
<DT><B>gap_sbdd_notin_in</B></DT>
    <DD>try exclusion first, then inclusion, using sbdd_try_set/3 (this is
    for use in conjunction with the GAP-based SBDD library lib(ic_gap_sbdd))</DD>
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

