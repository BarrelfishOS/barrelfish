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
% Copyright (C) 1996 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Hani El-Sakkout, Stefano Novello and Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% CLP Repair Library
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Hani El-Sakkout, Stefano Novello, Joachim Schimpf, IC-Parc
% Version:	$Id: repair.pl,v 1.4 2012/10/25 13:14:34 jschimpf Exp $
%
%
% EXTENSIONS
% We consider variables with variable tentative value tenable. That's
% because intially when the problem is being set up we don't want to
% see constraints that are in conflict just because the tentative
% value hasn't been set yet.
% 
% Later, we may want to consider these constraints as being in conflict
% since they have to be fixed either by labelling of by giving tentative
% values.
%
%
% CHANGES
% Allowed constraints where not all variables have a tentative value
% Such variables are considered tenable.
% It means that tentative_ground may not ground completely if such
% variables are present.
% when testing, call such an almost ground constraint inside a not not
% so that it does no binding.
% this may cause unncessary propagations though.
%
% ----------------------------------------------------------------------

:- module(repair).

% The repair handler should execute after the others, since it
% wants to know the new domain of a variable to determine tenablity
% at that point. This is useful in var var unification to choose
% which tentative value to throw away, but not essential.
% lib(repair) should occur first to achieve this !

:- meta_attribute(repair,[
	unify:repair_unify_handler/2,
	print:print_repair/2,
	suspensions:repair_suspensions_handler/3
    ]).

:- export op(900,xf,r).
:- export op(900,xf,r_no_prop).
:- export op(900,xf,r_prop).
:- export op(900,xfx,(r_conflict)).
:- export op(900,xfx,(r_conflict_prop)).
:- export op(700,xfx,[tent_set,tent_get,tent_is]).

:- export
    (tent_set)/2,
    (tent_get)/2,
    (tent_call)/3,(tent_call)/4,
    (tent_is)/2,(tent_is)/3,
    tenable/1,
    (r)/1,
    (r_no_prop)/1,
    (r_prop)/1,
    (r_conflict)/2,
    (r_conflict_prop)/2,
    poss_conflict_vars/1,
    conflict_constraints/1,
    poss_conflict_vars/2,
    conflict_constraints/2,
    conflict_vars/1,
    tentative_ground/2,
    tr_monitors/2,
    call_satisfied_constraints/0,
    repair_stat/1.


% Attribute

:- export struct(repair(
    	tent,		% tentative value
	mon,		% set element with suspension of monitor_tenable goal
%	to_unten,	% suspensions to wake on becoming untenable
	ga_chg		% suspensions to wake on global asignment changes
    )).

% global_assignent(Term) = if var(Term) tentative_value(Term) else Term
% conflict_vars		= monitor_tenable suspensions of untenable vars
% conflict_constraints	= suspensions of repair constraints in conflict
% satisfied		= suspension of satisfied repair constraints.


:- local struct(repair_state(
	conflict_vars,
	conflict_hash_constraints,
	conflict_constraints)).
:- local struct(monitor_conflict(constraint,annotation,conflict,prop,module)).
:- local struct(tent_is_conflict(expr,annotation,sum,outsum,conflict,susp,module)).


:- export portray(monitor_tenable/3, tr_monitors/2, [goal]).
:- export portray(monitor_conflict/(property(arity) of monitor_conflict), tr_monitors/2, [goal]).

tr_monitors(monitor_tenable(V,_,T), monitor_tenable(V,T)).
tr_monitors(monitor_conflict{constraint:C}, monitor_conflict(C)).


:- comment(categories, ["Constraints","Techniques"]).
:- comment(summary, "Repair library: support for local search via tentative assignments and repair constraints").
:- comment(author, "Hani El-Sakkout, Stefano Novello, Joachim Schimpf").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2012/10/25 13:14:34 $").

:- comment(desc, html("\
The repair library provides a framework for the integration of repair-based
search with the constraint consistency checking techniques of ECLiPSe. It
allows the implementation of classical local search methods within a CLP
environment.

It provides two facilities:

<UL>

<LI> The maintenance of tentative values for problem variables. These
    tentative values may together form a partial or even inconsistent 
    tentative assignment.  Modifications to, or extensions of this
    assignment may be applied until a correct solution is found.

<LI> The monitoring of constraints (the so called repair constraints)
    for being either satisfied or violated under the current tentative
    assignment.  Search algorithms can then access the set of
    constraints that are violated at any point in the search,
    and perform repairs by changing the tentative assignment
    of the problem variables.

</UL><P>
Normally, the repair library communicates with another solver (such as fd or 
ria) to check for constraint violations.</P> 
<P>
Tentative values can be visualised using the ECLiPSe visualisations tools.
To do so, set up a viewable using viewable:viewable_create/3,4 and specify
changeable(repair,Type) as its element type.
</P> 
")).

:- comment(index, ["local search", "tentative assignments"]).

:- comment((r)/1, [
see_also: [(r_conflict)/2, (r_conflict_prop)/2],
summary: "Obsolete: use r_conflict/2 and r_conflict_prop/2 instead."
]).
:- comment((r_no_prop)/1, [
see_also: [(r_conflict)/2, (r_conflict_prop)/2],
summary: "Obsolete: use r_conflict/2 and r_conflict_prop/2 instead."
]).
:- comment((r_prop)/1, [
see_also: [(r_conflict)/2, (r_conflict_prop)/2],
summary: "Obsolete: use r_conflict/2 and r_conflict_prop/2 instead."
]).
:- comment(conflict_constraints/1, [
see_also: [conflict_constraints/2],
summary: "Obsolete: use conflict_constraints/2 instead."
]).


% ----------------------------------------------------------------------

:- pragma(nodebug).

:- use_module(library(linearize)).
:- use_module(library(hash)).

:- import copy_term/3 from sepia_kernel.

% DEBUG -------------------------------------------------------

  'ASSERT'(_).
% 'ASSERT'(G) :- call(G).

% Attribute -------------------------------------------------------

% :- meta_attribute(...)  see above

print_repair(X, TVal) :-
	get_repair_attr(X, repair{tent:TVal}),
	nonvar(TVal).

repair_suspensions_handler(_{Attr}, Susps, Susps0) ?-
	( var(Attr) ->
	    Susps=Susps0
	;
	    Attr = repair{ga_chg:S},
	    Susps = [S|Susps0]
	).


repair_unify_handler(_, Attr) :-
	var(Attr).

repair_unify_handler(Term, Attr) :-
	compound(Attr),
	repair_unify_handler1(Term, Attr).

repair_unify_handler1(Var{Attr1}, Attr2) :-
	-?->
	!,
	( var(Attr1) ->
	    Attr1 = Attr2	% transfer the whole attribute
	;
	    Attr1 = repair{tent:TV1,mon:M1},
	    Attr2 = repair{tent:TV2,mon:M2},
	    'ASSERT'(writeln(unify_var_var(TV1,TV2))),
	    inc(var_var_unify),
	    ( TV1 == TV2 ->
		    kill_monitor(M2)
	    ; not_unify(Var , TV2) ->
		% TV2 is untenable
		kill_monitor(M2),
		schedule_suspensions(ga_chg of repair,Attr2)
	    ;
		( not_unify(Var , TV1) ->
		    kill_monitor(M1),
		    setarg(tent of repair, Attr1, TV2),
		    setarg(mon of repair, Attr1, M2),
		    schedule_suspensions(ga_chg of repair,Attr1)
		;
		    kill_monitor(M2),
		    schedule_suspensions(ga_chg of repair,Attr2)
		)
	    ),
%	    merge_suspension_lists(to_unten of repair,Attr2,to_unten of repair,Attr1),
	    merge_suspension_lists(ga_chg of repair,Attr2,ga_chg of repair,Attr1)
	).
repair_unify_handler1(Nonvar, Attr) :-
	Attr = repair{tent:TVal,mon:M},
	'ASSERT'(writeln(unify_nonvar_var(Nonvar,TVal))),
	inc(nonvar_var_unify),
	(var(TVal) ->
	    schedule_suspensions(ga_chg of repair,Attr)
	;
	    kill_monitor(M),
	    ( Nonvar == TVal ->
		    true
	    ;
		% instantiated but not to tentative value
		schedule_suspensions(ga_chg of repair,Attr)
	    )
	).

kill_monitor(EM) :-
	    elem_term(EM,M),
	    elem_del(EM),
	    kill_suspension(M).

% Global state -------------------------------------------------------

% The conflict vars set is those variables that are untenable. All of these
% must get labelled.
%
% The conflict constraints set contains constraints that are untenable.
% All these must be fixed, by moving at least one variable of each
% constraint into the conflict vars set.
% an untenable constraint is one which is violated if all its
% variables are set to their tentative values.
% The global value conflict_constraints is a free variable. It is ensured
% that its delayed goals list is exactly the conflict constraint set.

:- local reference(repair_state).

get_repair_state(S) :-
	getval(repair_state, RepairState),
	( compound(RepairState) ->
		S = RepairState
	; % needs initialisation
		S = repair_state{
		    conflict_vars:CVs,
		    conflict_constraints:CCs,
		    conflict_hash_constraints:H
		},
		set_new(CVs),
		set_new(CCs),
		hash_create(H),
		setval(repair_state, S)
	).

get_repair_state(Field, F) :-
	get_repair_state(S),
	arg(Field, S, F).

get_hashed_set(Key,Set) :-
	( var(Key) ->
	    set_new(Set), Key = Set
	; atom(Key) ->
	    get_repair_state(conflict_hash_constraints of repair_state, H),
	    ( hash_find(H,Key,Set) ->
		true
	    ;
		set_new(Set),
		hash_add(H,Key,Set)
	    )
	;
	    Key = Set
	).

% ----------------------------------------------------------------------
% The general repair annotation:
%
%	Goal r_conflict SetName
%	Goal r_conflict SetName-ConflictInfo
%
%	Goal r_conflict_prop SetName
%	Goal r_conflict_prop SetName-ConflictInfo
%		like r_conflict but calls Goal when it goes into conflict
%
% Backward compatibility:
%	Goal r_no_prop
%		like r_conflict but using a global, unnamed conflict set
%	Goal r
%		like r_no_prop but calls Goal when it goes into conflict
%	Goal r_prop
%		like r_no_prop but eagerly calls Goal
% ----------------------------------------------------------------------

% PRIORITIES
% Make this low so that propagation comes first
% make the almost ground check at a priority just higher than
% monitor
% Highest is tenability monitor since it is cheap and others
% use the tenability flag that it sets.
% Lower than propagation to avoid redoing it unnecessarily.
%
% All monitor_conflict does is collect the possible untenable variables
% used for labelling so it can be done last.
%
% The disjunct needs a priority inbetween.
%


:- comment((r_conflict)/2, [
amode: r_conflict(+,?),
template: "+Constraint r_conflict ?ConflictSet",
args: ["Constraint":"Constraint to be monitored for conflict (Goal)",
"ConflictSet": "Handle for the conflict set (atom or varibale)
                argument can alternatively be ConflictSet-ConflictData"
      ],
summary: "Annotate Constraint as a repair constraint and monitor it for conflicts.",
see_also: [conflict_constraints/2, (r_conflict_prop)/2],
resat: no,
eg:  "\
% lib(fd) is loaded
[eclipse 17]:  A #= B r_conflict c , B tent_set 11, A tent_set 5,  conflict_constraints(c, X).

B = B{11}
A = A{5}
X = [A{5} #= B{11}]  % the constraint is in conflict due to tentative values

[eclipse 18]: A #= B r_conflict c , B = 11, A = 5, conflict_constraints(c, X).

B = 11
A = 5
X = [5#=11] % the constraint is in conflict due to the values of the variables

 A #= B r_conflict c, B tent_set 11, conflict_constraints(c, X).

A = A
B = B{11}
X = []  % the constraint is not in conflict

 A::[1..10],  A #= B r_conflict c, B tent_set 11, conflict_constraints(c, X).

A = A{[1..10]}
B = B{11}
X = [A{[1..10]} #= B{11}]

[eclipse 26]:  A::[1..10],  A #= B r_conflict c, A #= B, B = 11, conflict_constraints(c, X).

no (more) solution. 
% fails because A #= B is also set up as a normal constraint

[eclipse 23]: A::[1..10],  A #= B r_conflict c, A #= B, B tent_set 11, conflict_constraints(c, X).

A = A{fd:[1..10], repair:11}
B = A{fd:[1..10], repair:11}
X = [A{fd:[1..10], repair:11} #= A]
% does not fail because the normal A #= B does not consider tenative values
",

desc: html("\
<P>
Repair constraints are constraints that are monitored by the repair library 
for conflicts caused by the tentative values of variables in the constraints. 
r_conflict/2 annotates a constraint to be a repair constraint, and performs
the simplest form of monitoring for violation: the repair constraint is 
passive in that it simply waits for constraint to become violated due to 
bindings to its variables or their tentative values. In such a case, the 
constraint will show up in the ConflictSet, from where it can be
retrieved using conflict_constraints/2.

</P><P>
Note that setting up a repair constraint does <EM>not</EM> propagate the 
constraint as a normal constraint as well. Call the constraint again
without the annotation to propagate the constraint.

</P><P>
Constraint can be any goal that works logically, it should be useable
as a ground check, and work on any instantiation pattern. Typically,
it will be a constraint from some solver library.

</P><P>
ConflictSet can be a user-defined name (an atom) or it can be
a variable in which case the system returns a conflict set handle that can
later be passed to conflict_constraints/2. 

</P><P>
Note that using different conflict sets for different groups of constraints
will often make the search algorithm easier and more efficient.
A second allowed form of the r_conflict annotation is
Constraint r_conflict ConflictSet-ConflictData.
If this is used, \bf ConflictData will appear in the conflict
set instead of the Constraint itself.
This feature can be used to pass additional information to the
search algorithm.
</P>")
]).

:- comment((r_conflict_prop)/2, [
amode: r_conflict_prop(+,?),
template: "+Constraint r_conflict_prop ?ConflictSet",
args: ["Constraint":"Constraint to be monitored for conflict (Goal)",
"ConflictSet": "Handle for the conflict set (atom or varibale)
                argument can alternatively be ConflictSet-ConflictData"
      ],
summary: "Annotate Constraint as a repair constraint and monitor it for conflicts. It is propagated when it goes into conflict.",
see_also: [conflict_constraints/2, (r_conflict)/2],
resat: no,
eg: "\
 A #= B r_conflict_prop c, A = 5, writeln(1), B = 11, write(2), conflict_constraints(c, X).

1

no (more) solution.
% fails because A #= B was propagated when a conflict was detected 
",
desc: html("\
<P>
Repair constraints are constraints that are monitored by the repair library 
for conflicts caused by the tentative values of variables in the constraints. 
r_conflict_prop/2 annotates a constraint to be a repair constraint, and 
as with r_conflict/2, monitors the constraint for conflicts. The difference
is that when a violation is first detected and the Constraint enters the 
ConflictSet, it is actually propagated at that point by calling the constraint.

</P><P>
Note that if you want constraint propagation from the very beginning,
you should simply write the constraint twice, once without and once
with annotation.

</P><P>
Constraint can be any goal that works logically, it should be useable
as a ground check, and work on any instantiation pattern. Typically,
it will be a constraint from some solver library.

</P><P>
ConflictSet can be a user-defined name (an atom) or it can be
a variable in which case the system returns a conflict set handle that can
later be passed to conflict_constraints/2. 

</P><P>
Note that using different conflict sets for different groups of constraints
will often make the search algorithm easier and more efficient.
A second allowed form of the r_conflict annotation is
Constraint r_conflict ConflictSet-ConflictData.
If this is used, \bf ConflictData will appear in the conflict
set instead of the Constraint itself.
This feature can be used to pass additional information to the
search algorithm.
</P>")
]).


:- tool((r_conflict)/2, (r_conflict)/3).
r_conflict(Constraint,SetName-Annotation,Module) ?- !,
	get_hashed_set(SetName,Set),
	r_conflict(Constraint,Set,Annotation,1,Module).
r_conflict(Constraint,SetName,Module) :- 
	get_hashed_set(SetName,Set),
	r_conflict(Constraint,Set,Constraint,1,Module).

:- tool((r_conflict_prop)/2, (r_conflict_prop)/3).
r_conflict_prop(Constraint,SetName-Annotation,Module) ?- !,
	get_hashed_set(SetName,Set),
	r_conflict(Constraint,Set,Annotation,0,Module).
r_conflict_prop(Constraint,SetName,Module) :- 
	get_hashed_set(SetName,Set),
	r_conflict(Constraint,Set,Constraint,0,Module).


% We treat the arithmetic constraints specially, using tent_is/2 to
% evaluate the arithmetic expressions efficiently. Note that the auxiliary
% result variable of the tent_is cannot be accessed by the user and can
% therefore be assumed to remain a variable.

%r_conflict((Val #:= Expr),Set,Annotation,Prop,Module) ?-
%	!,
%	tent_is_(Set,Val,Expr,Annotation,Module).
%r_conflict(#?(Goal,B),Set,Annotation,Prop,Module) ?-
%	!,
%	tent_call(Goal,B,#?(Goal,B),Module).
%	tent_isd_(Set,B,Goal,Annotation,Module).
r_conflict(Goal, Set, Annotation, Prop, Module) :-
	arith_constraint(Goal, LeftExpr, RightExpr, NewGoal, Left, Right),
	!,
	tent_is(Left,LeftExpr,Module),
	unify_to_tent_if_ground_args(Left, LeftExpr),
	tent_is(Right,RightExpr,Module),
	unify_to_tent_if_ground_args(Right, RightExpr),
	setup_conflict_monitor(Set,NewGoal,Annotation,Prop,Module).
r_conflict(Goal,Set,Annotation,Prop,Module) :-
	setup_conflict_monitor(Set,Goal,Annotation,Prop,Module).

    :- mode arith_constraint(?,-,-,-,-,-).
    arith_constraint(Cstr, _, _, _, _, _) :- var(Cstr), !, fail.
    arith_constraint(X < Y, X, Y, suspend:(X1 < Y1), X1, Y1) :- !.
    arith_constraint(X > Y, X, Y, suspend:(X1 > Y1), X1, Y1) :- !.
    arith_constraint(X =< Y, X, Y, suspend:(X1 =< Y1), X1, Y1) :- !.
    arith_constraint(X >= Y, X, Y, suspend:(X1 >= Y1), X1, Y1) :- !.
    arith_constraint(X =:= Y, X, Y, suspend:(X1 =:= Y1), X1, Y1) :- !.
    arith_constraint(X =\= Y, X, Y, suspend:(X1 =\= Y1), X1, Y1) :- !.
    arith_constraint(M : Goal, X, Y, M : Goal1, X1, Y1) :- !,
	arith_constraint1(Goal, X, Y, Goal1, X1, Y1).
    arith_constraint(Goal, X, Y, Goal1, X1, Y1) :-
	arith_constraint1(Goal, X, Y, Goal1, X1, Y1).

    :- mode arith_constraint1(?,-,-,-,-,-).
    arith_constraint1(Cstr, _, _, _, _, _) :- var(Cstr), !, fail.
    arith_constraint1(X < Y, X, Y,	X1 < Y1, X1, Y1).
    arith_constraint1(X > Y, X, Y,	X1 > Y1, X1, Y1).
    arith_constraint1(X =< Y, X, Y,	X1 =< Y1, X1, Y1).
    arith_constraint1(X >= Y, X, Y,	X1 >= Y1, X1, Y1).
    arith_constraint1(X =:= Y, X, Y,	X1 =:= Y1, X1, Y1).
    arith_constraint1(X =\= Y, X, Y,	X1 =\= Y1, X1, Y1).
    arith_constraint1(<(X,Y,B), X, Y,	<(X1,Y1,B), X1, Y1).
    arith_constraint1(>(X,Y,B), X, Y,	>(X1,Y1,B), X1, Y1).
    arith_constraint1(=<(X,Y,B), X, Y,	=<(X1,Y1,B), X1, Y1).
    arith_constraint1(>=(X,Y,B), X, Y,	>=(X1,Y1,B), X1, Y1).
    arith_constraint1(=:=(X,Y,B), X, Y,	=:=(X1,Y1,B), X1, Y1).
    arith_constraint1(=\=(X,Y,B), X, Y,	=\=(X1,Y1,B), X1, Y1).
    arith_constraint1($<(X,Y), X, Y,	$<(X1,Y1), X1, Y1).
    arith_constraint1($>(X,Y), X, Y,	$>(X1,Y1), X1, Y1).
    arith_constraint1($=<(X,Y), X, Y,	$=<(X1,Y1), X1, Y1).
    arith_constraint1($>=(X,Y), X, Y,	$>=(X1,Y1), X1, Y1).
    arith_constraint1($=(X,Y), X, Y,	$=(X1,Y1), X1, Y1).
    arith_constraint1($\=(X,Y), X, Y,	$\=(X1,Y1), X1, Y1).
    arith_constraint1($<(X,Y,B), X, Y,	$<(X1,Y1,B), X1, Y1).
    arith_constraint1($>(X,Y,B), X, Y,	$>(X1,Y1,B), X1, Y1).
    arith_constraint1($=<(X,Y,B), X, Y,	$=<(X1,Y1,B), X1, Y1).
    arith_constraint1($>=(X,Y,B), X, Y,	$>=(X1,Y1,B), X1, Y1).
    arith_constraint1($=(X,Y,B), X, Y,	$=(X1,Y1,B), X1, Y1).
    arith_constraint1($\=(X,Y,B), X, Y,	$\=(X1,Y1,B), X1, Y1).
    arith_constraint1(X #< Y, X, Y,	X1 #< Y1, X1, Y1).
    arith_constraint1(X #> Y, X, Y,	X1 #> Y1, X1, Y1).
    arith_constraint1(X #=< Y, X, Y,	X1 #=< Y1, X1, Y1).
    arith_constraint1(X #<= Y, X, Y,	X1 #<= Y1, X1, Y1).
    arith_constraint1(X #>= Y, X, Y,	X1 #>= Y1, X1, Y1).
    arith_constraint1(X #= Y, X, Y,	X1 #= Y1, X1, Y1).
    arith_constraint1(X #\= Y, X, Y,	X1 #\= Y1, X1, Y1).
    arith_constraint1(#<(X,Y,B), X, Y,	#<(X1,Y1,B), X1, Y1).
    arith_constraint1(#>(X,Y,B), X, Y,	#>(X1,Y1,B), X1, Y1).
    arith_constraint1(#=<(X,Y,B), X, Y,	#=<(X1,Y1,B), X1, Y1).
    arith_constraint1(#<=(X,Y,B), X, Y,	#<=(X1,Y1,B), X1, Y1).
    arith_constraint1(#>=(X,Y,B), X, Y,	#>=(X1,Y1,B), X1, Y1).
    arith_constraint1(#=(X,Y,B), X, Y,	#=(X1,Y1,B), X1, Y1).
    arith_constraint1(#\=(X,Y,B), X, Y,	#\=(X1,Y1,B), X1, Y1).


:- tool((r)/1, (r)/2).
r(Constraint,Module) :-
	get_repair_state(conflict_constraints of repair_state,ConfSet),
	setup_conflict_monitor(ConfSet,Constraint,Constraint,0,Module).

:- tool((r_no_prop)/1, (r_no_prop)/2).
r_no_prop(Constraint,Module) :-
	get_repair_state(conflict_constraints of repair_state,ConfSet),
	setup_conflict_monitor(ConfSet,Constraint,Constraint,1,Module).

:- tool((r_prop)/1, (r_prop)/2).
r_prop(Constraint,Module) :-
	get_repair_state(conflict_constraints of repair_state,ConfSet),
	call(Constraint)@Module,
	setup_conflict_monitor(ConfSet,Constraint,Constraint,1,Module).


setup_conflict_monitor(ConfSet,Constraint,Annotation,PropFlag,Module) :-
	term_variables(Constraint,Vars),
	add_repair_attrs(Vars),
	elem_new(Susp,ConfSet,ConfElem),
	suspend(
	    monitor_conflict{
		constraint:Constraint,
	    	annotation:Annotation,
		conflict:ConfElem,
		prop:PropFlag,
		module:Module},
	    8,
	    [Vars->constrained, Vars->ga_chg],
	    Susp),
	schedule_woken([Susp]),
	wake.

% monitor_conflict/? keeps testing whether the constraint would be satisfied
% when using the tentative values of its variables. It can be in three states:
%	- satisfied
%	- unsatisfied				\ conflict
%	- unknown (contains untenable vars)	/  constraints
% When the constraint is unsatisfied or unknown, it goes into the conflict
% constraint set. Otherwise it goes into the satisfied constraint set.
% A constraint can make many transitions between these states:
% 	sat	   -untenable instantiation->	unsat
% 	sat		-untenability->		unknown
% 	sat		-tentative val change->	unsat/unknown
% 	unsat		-instantiation->	sat
% 	unsat		-untenability->		unknown
% 	unsat		-tentative val change->	sat/unknown
% 	unknown		-instantiation->	sat/unsat
% 	unknown		-tentative val change->	sat/unsat
%       

:- demon(property(functor) of monitor_conflict).
monitor_conflict{constraint:C,annotation:_Annotation,
			conflict:ConfElem,prop:PropFlag,module:Module} :-
	( tentative_ground(C ,AlmostGroundConstraint,Vars),
	  not not call(AlmostGroundConstraint)@Module ->
	    elem_del(ConfElem),
	    ( novars == Vars ->
		inc(wake_ground),
		'ASSERT'(writeln(ground(C,PropFlag))),
		% the constraint is ground so remove it
		propagate(PropFlag,C,Module,ConfElem),
		elem_term(ConfElem,Susp),
		kill_suspension(Susp)
	    ; % ground check succeeds 
		inc(wake_satisfied),
		'ASSERT'(writeln(satisfied(C,PropFlag)))
	    )
	;
	    % ground check failed or no pssible global assignment
	    % the first time it finds a constraint is unsatisfiable
	    % it asserts the constraint.
	    inc(wake_conflict),
	    'ASSERT'(writeln(conflict(C,PropFlag))),
	    elem_add(ConfElem),
	    propagate(PropFlag,C,Module,ConfElem)
	).


propagate(1,_,_,_).
propagate(0,C,M,Elem):-
	inc(propagate),
	call(C)@M,
	elem_term(Elem,Susp),
	get_suspension_data(Susp, goal, Rep),
	setarg(prop of monitor_conflict,Rep,1).


% WasTenable = { yes,no }
% keeps tracking the tenability of a variable. If a variable
% becomes untenable it wakes the goals waiting for this
% condition.

% a suspension on a global ref is used get the untenable
% variables

% this may get killed it's
% variable is unified with another variable.

% the goal is only created for variables with a ground tentative value

:- demon(monitor_tenable/3).
monitor_tenable(Var, Attr, S) :-
	S=s(WasTenable),
	Attr = repair{tent:TVal,mon:EM},
	'ASSERT'((var(Var),writeln(mon(Var,TVal,WasTenable)))),
	( not_unify(Var , TVal) ->
	    ( WasTenable == yes ->
		elem_add(EM),
		setarg(1,S,no),
	    	inc(monitor_to_untenable),
		schedule_suspensions(ga_chg of repair,Attr),
%		schedule_suspensions(to_unten of repair,Attr),
		wake
	    ;
	    	inc(monitor_no_change)
	    )
	;
	    (WasTenable == no ->
		elem_del(EM),
		inc(monitor_to_tenable),
		setarg(1,S,yes)
	    ;
	    	inc(monitor_no_change)
	    )
	).

    :- mode extract_variables(+,?).
    extract_variables([],[]).
    extract_variables([Susp|Susps],[V|Vs]) :-
	    get_suspension_data(Susp, goal, monitor_tenable(V,_,_)),
	    extract_variables(Susps,Vs).

:- comment(tenable/1, [
amode: tenable(?),
summary: "Check if Var is tenable.",
args: ["Var":"Term"],
fail_if: "Fails if Var is non-tenable.",
see_also: [(tent_set)/2, conflict_vars/1],
resat: no,
eg: "\
% lib(fd) is loaded

[eclipse 3]: X::1..5, X tent_set 3, tenable(X).  % suceeds

[eclipse 3]: X::1..5, X tent_set 7, tenable(X).  % fails

", 

desc: html("\
<P>
Succeeds if Term is tenable. A Term is tenable if it does not contain any
variables with tentative values which are inconsistent with any constraints
involving thevariable. Note that variables with no tentative values are
considered tenable.</P>")
]).

tenable(X{repair{tent:TVal}}) :-
	-?->
	!,
	not not_unify(X , TVal).
tenable(_).

% this construct the global assignment for Original in Copy.
% AllVars = all variables in term
% NoTenVars = all variables in term with no tentative value
% Untenable = an untenable variable or []
% In the case that Untenable is a variable, no global assignment
% was constructed and the Copy AllVars and NoTenVars parameters are
% invalid.
:- mode tentative_ground(?,?,-).
tentative_ground(Original,Copy,Vars) :-
	copy_term(Original,Copy,Pairs),
	tentative_ground_pairs(Pairs,Vars).

	:- mode tentative_ground_pairs(+,-).
	tentative_ground_pairs([],novars).
	tentative_ground_pairs([[Original|Copy]|Pairs],vars) :-
	    get_repair_attr(Original, Attr),
	    Attr = repair{tent:TVal},
	    ( var(TVal) ->
		copy_term(Original,Copy)
	    ; not_unify(Original , TVal) ->
	    	fail
	    ;
		TVal=Copy
	    ),
	    tentative_ground_pairs(Pairs,_).

% This finds the global assignment for Term. If due to the
% presence of a non-tenable variable there was no global assignment
% it fails.
% The global assignment is :
% Replace tenable variables with their tentative values
% Rename variables that have no tentative value (i.e. they keep their
% domains and any important properties but are new variables with no
% attached goals.
tentative_ground(Term,GlobalAssignment) :-
	tentative_ground(Term,GlobalAssignment,_).

% tent_get/2 is like tentative_ground/2 but will not fail in the case of
% presence of an untenable variable. Useful to know the tentative value
% of untenable variables.
:- comment((tent_get)/2, [
amode: tent_get(?,?),
template: "?Vars tent_get ?Values",
summary: "Query the tentative values of variables in Vars.",
args: ["Vars": "Term typically with variables with tentative values",
       "Values": "Term to receive tentative values of Vars."
      ],
fail_if: "Values does not unify with Vars with the tentative values filled in.",
resat: no,
see_also: [(tent_set)/2],
desc: html("\
<P>
Values is a copy of the term Vars with the tentative values filled in
place of the variables.   If a variable has no tentative value 
a variable is returned in its place.
</P>
<P>
CAUTION: If a variable has no tentative value, it is not possible to
give it a tentative value by binding that returned variable.
tent_set/2 must be used instead.
</P>")
]).


Var tent_get TVal :-
	var(Var),
	!,
	get_repair_attr(Var,repair{tent:TVal0}),
	( var(TVal0) ->
	    true
	;
	    TVal = TVal0
	).

Term tent_get TValTerm :-
	compound(Term),
	!,
	functor(Term,F,A),
	functor(TValTerm,F,A),
	( for(I,1,A),
	  param(Term,TValTerm)
	do
	    arg(I,Term,Termi),
	    arg(I,TValTerm,TValTermi),
	    Termi tent_get TValTermi
	).
Atomic tent_get Atomic.

:- comment((tent_set)/2, [
amode: tent_set(?,++),
template: "?Vars tent_set ++Values",
args:  ["Vars":"Term with variables (non-ground term)",
        "Values":"Tentative values for variables in Vars (ground term)"
       ],
summary: "Assigns tentative values for the variables in a term.",
see_also: [(tent_get)/2, tenable/1 
          ],
fail_if: "Vars is non-unifiable with Values",
resat: no,
eg: "
% lib(fd) is loaded

[eclipse 3]: X::1..5, X tent_set 3.
X = X{fd:[1..5], repair:3} % X is tenable 

[eclipse 3]: X::1..5, X tent_set 7.
X = X{fd:[1..5], repair:7} % X is non-tenable 
", 

desc:  html("\
<P>
Associate tentative values with variables. Vars can be any non-ground term,
and Values the corresponding ground term. The tentative values of the
variables are set to the ground values in Values. Typically Var is a
variable or a list of variables.

</P></P>
A tentative value is generally used to record preferred or previous
assigments to this variable. It does not actually bind the variable to the
value.  It can be changed through later calls to tent_set. Together with
other tentative values and actual values for the problem variables in a
program, they can form a tentative assignment which may be a partial or
inconsistent solution to the problem. Variables with inconsistent tentative
values are known as non-tenable.
</P>
")
]).


% Set the tentative values in the left hand side term. Rhs must be ground.
% enclose in call_priority to limit unnecessary work while doing a large
% tent_set/2 with several variables

Term tent_set GroundTerm :-
	nonground(GroundTerm), !,
	error(4, Term tent_set GroundTerm).
Term tent_set GroundTerm :-
	call_priority(was0(Term, GroundTerm),1).

    was0(Var, NewTVal) :-
	    var(Var),
	    % nonvar(NewTVal), is guaranteed by calling via tent_set/2.
	    !,
	    get_repair_attr(Var, Attr),
	    Attr = repair{tent:OldTVal,mon:Mon},
	    ( var(OldTVal) ->
		'ASSERT'(var(Mon)),
		NewTVal = OldTVal,
		get_repair_state(conflict_vars of repair_state, ConfSet),
		elem_new(MonSusp,ConfSet,Mon),
		% no tenable value treated as if it had been tenable
		suspend(
			monitor_tenable(Var,Attr,s(WasTenable)),
			6,
			Var->constrained,
			MonSusp),
		( not_unify(Var , NewTVal) ->
		    WasTenable=no,
		    elem_add(Mon)
%		    , schedule_suspensions(to_unten of repair,Attr)
		;
		    WasTenable=yes
		),
		schedule_suspensions(ga_chg of repair,Attr),
		wake
	    ; OldTVal == NewTVal ->
		true
	    ; % change the tentative value
		setarg(tent of repair,Attr,NewTVal),
		( not_unify(Var , OldTVal) ->
		    ( not_unify(Var , NewTVal) ->
		    	true
		    ;
			elem_del(Mon),
			modify_tent(Mon,yes)
		    )
		;
		    ( not_unify(Var , NewTVal) ->
			elem_add(Mon),
			modify_tent(Mon,no)
%			, schedule_suspensions(to_unten of repair,Attr)
		    ;
		    	true
		    )
		),
		schedule_suspensions(ga_chg of repair,Attr),
		wake
	    ).
    was0(C,G) :-
	    compound(C),
	    !,
	    functor(C,F,A),
	    functor(G,F,A),
	    ( for(I,1,A),
	      param(C,G)
	    do
	    	arg(I,C,Ci),
	    	arg(I,G,Gi),
		was0(Ci,Gi)
	    ).
    was0(C,C).

    modify_tent(Elem,WasTenable) :-
    	elem_term(Elem,Susp),
	get_suspension_data(Susp, goal, Goal),
	Goal = monitor_tenable(_,_,S),
	setarg(1,S,WasTenable).

add_repair_attrs([]).
add_repair_attrs([X|Xs]) :-
	get_repair_attr(X, _),		% will actually add if not there yet
	add_repair_attrs(Xs).


get_repair_attr(X{A}, Attr) :-		% access attribute, create if none
	-?->
	get_repair_attr1(X, Attr, A).
get_repair_attr(X, Attr) :-
	free(X),
	new_repair_attr(X, Attr).

    get_repair_attr1(X, Attr, A) :-
	var(A), new_repair_attr(X, Attr).
    get_repair_attr1(_, Attr, A) :-
	nonvar(A), Attr=A.

    new_repair_attr(X, Attr) :-		% make a new repair-variable
	Attr = repair{},
%	init_suspension_list(to_unten of repair,Attr),
	init_suspension_list(ga_chg of repair,Attr),
	add_attribute(X, Attr).


% ----------------------------------------------------------------------
% Invariants
% ----------------------------------------------------------------------

% Precondition: Val is a variable (otherwise it can fail in tent_set)

:- comment((tent_is)/2, [
amode: tent_is(-,+),
template: "-Result tent_is +Expression",
args: ["Result":"Variable", "Expression":"Arithematic Expression"],
summary: "Eagerly evaulate Expression using tentative assignments.",
see_also: [is/2, (tent_set)/2, tent_call/3],
desc: html("\
<P>
This is similar to the normal arithmetic <TT>is/2</TT> predicate, but 
evaluates the expression based on the tentative
assignment of its variables. The result is delivered as (an update to)
the tentative value of the Result variable.
Once initiated, tent_is will stay active and keep updating Result's
tentative value eagerly whenever the tentative assignment of any
variable in Expression changes.
</P>
")
]).

:- tool((tent_is)/2, (tent_is)/3).
tent_is(Val,Expr,_Module) :- var(Expr), !,
	Val = Expr,
	get_repair_attr(Expr, _).	% will actually add if not there yet
tent_is(Val,Expr,_Module) :- number(Expr), !,
	Val = Expr.
tent_is(Sum, Expr, Module) :-
	linearize(Expr, [Cst*1 | Terms], NonLin),
	(
	    foreach(C*V, Terms),
	    fromto(Cst, In, Out, TentSum),
	    param(Sum)
	do
	    get_repair_attr(V, repair{tent:TV}),	% or make attr
	    ( var(TV) -> T=0 ; T=TV ),	% no tent value treated as zero
	    Out is In + T * C,
	    suspend(sum_update(C*V,T,Sum,Susp), 2, [V->inst,V->ga_chg], Susp)
	),
	Sum tent_set TentSum,

	% treat the nonlinear components
	( foreach(V = NonLinExpr, NonLin), param(Module) do
	    update_expr(V, NonLinExpr,Module)
	).


% Out is guaranteed to be a var.
update_expr(Out, Expr, Module) :-
	term_variables(Expr, In),
	tent_call(In, Out, Out is Expr, Module).
	

:- comment(tent_call/3, [
amode: tent_call(+,+,+),
args: ["In":"List of variables (subset of variables in Goal)",
       "Out":"List of variables (subset of variables in Goal)",
       "Goal": "Goal to be called"
      ],
see_also: [(tent_set)/2, (tent_is)/2],
summary: "Eagerly call Goal whenever tentative values of variables in In changes.",
desc: html("\
<P>
This is a completely general meta-predicate to support computations
with tentative values. Goal is a general goal, and In and Out are
lists (or other terms) containing subsets of Goal's variables.
A copy of Goal is called, with the In-variables replaced by their
tentative values and the Out-variables replaced by fresh variables.
Goal is expected to return values for the Out variables. These values
are then used to update the tentative values of the original Out variables.
This process repeats whenever the tentative value of any In-variable
changes.
</P>
")
]).


% Out is guaranteed to be a var.
:- tool(tent_call/3, tent_call/4).
tent_call(In, Out, Goal, Module) :-	% General predicate
	tent_call(In, Out, Goal, Module, _Susp).

:- demon tent_call/5.
tent_call(In, Out, Goal, Module, Susp) :-
	% tent_ground without Out being grounded
	copy_term((In, Goal, Out), (In0, Goal0, Out0)),
	In tent_get In0,
	once(Goal0)@Module,
	Out tent_set Out0,
	( nonground(In) ->
	    ( var(Susp) ->
	    	suspend(tent_call(In, Out, Goal, Module, Susp),
			2, [In->ga_chg], Susp)
	    ;
		true
	    )
	;
	    kill_suspension(Susp)
	).


% priority has to be less than that of sum_update and tent_call
unify_to_tent_if_ground_args(X, Args) :-
	( nonground(Args, SomeVar) ->
	    suspend(unify_to_tent_if_ground_args(X, Args), 3, SomeVar->inst)
	;
	    X tent_get X
	).

% Sum is guaranteed to be a var.
:- demon sum_update/4.
sum_update(CV, Previous, Sum, Susp) :-
	CV = C*V,
	V tent_get Current,
	Sum tent_get PreviousSum,
	CurrentSum is PreviousSum + (Current - Previous) * C,
	( var(V) ->
	    get_suspension_data(Susp,goal,Goal), setarg(2,Goal,Current)
	;
	    kill_suspension(Susp)
	),
	Sum tent_set CurrentSum.


/***
tent_is_conflict(ConfSet,Annotation,Sum,OutSum,Expr,Module) :-
	elem_new(Susp,ConfSet,Elem),
	suspend(tent_is_conflict(Expr,Annotation,Sum,OutSum,Elem,Susp,Module),
		7,
		[OutSum->constrained,OutSum-Sum->ga_chg],
		Susp),
	schedule_woken([Susp]),
	wake.

:- demon tent_is_conflict/7.
% Sum generated by invariant code.
% groundness signals invariant also ground.
% Outsum is user variable.
tent_is_conflict(_Expr,_Annotation,Sum,OutSum,Elem,Susp,_Module) :-
	( nonvar(Sum) ->
	    kill_suspension(Susp),
	    elem_del(Elem),
	    Sum = OutSum
	;
	    Sum tent_get TentSum,
	    ( var(OutSum) ->
		    OutSum tent_set TentSum,
		    ( tenable(Outsum) ->
			elem_del(Elem)
		    ;
			elem_add(Elem)
		    )
	    ; OutSum == TentSum ->
		    elem_del(Elem)
	    ;
		    elem_add(Elem)
	    )
	).
	
% ----------------------------------------------------------------------
% #?/2
% #?(Goal,B)
% ----------------------------------------------------------------------
tent_isd_(ConfSet,Bool,Goal,Annotation,Module) :-
	term_variables(Bool-Goal,Vars),
	add_repair_attrs(Vars),
	elem_new(Susp,ConfSet,ConfElem),
	suspend(
	    repair_isd(Goal,Annotation,ConfElem,Bool,Module),
	    7,
	    [ Vars->ga_chg],
	    Susp),
	schedule_woken([Susp]),
	wake.

:- demon repair_isd/5.
repair_isd(Goal,_Annotation,ConfElem,Bool,Module) :-
	( tentative_ground(Goal, G1,Vars) ->
	    ( novars == Vars ->
		elem_term(ConfElem,Susp),
		kill_suspension(Susp),
		( call(Goal)@Module -> Bool = 1 ; Bool = 0 )
	    ; not not call(G1)@Module ->
	  	TentBool = 1
	    ;
	  	TentBool = 0
	    ),
	    ( var(Bool) ->
	    	Bool tent_set TentBool,
		elem_del(ConfElem)
	    ; Bool == TentBool ->
		elem_del(ConfElem)
	    ;
		elem_add(ConfElem)
	    )
	;
	    elem_add(ConfElem)
	).
***/

% ----------------------------------------------------------------------
% Labeling interface
% ----------------------------------------------------------------------

:- comment(conflict_constraints/2, [
amode:conflict_constraints(+,-),
args: ["ConflictSet":"name or handle of a conflict set",
       "Constraints":"List of conflicting constraints in ConflictSet (variable)"
      ],
summary: "Retreive the set of conflicting constraints monitored in ConflictSet.",
resat: no,
desc: html("\
<P>
When a repair constraint goes into conflict (i.e. when it does not satisfy
the tentative assignment of its variables), it appears in a conflict set,
once it satisfies the tentative assignment, it disappears.
This primitive returns the list of all current conflict constraints
in the given conflict set.

</P><P>
ConflictSet is the conflict set name (or handle) which has
been used in the corresponding constraint annotation.  For example

<TT>
conflict_constraints(cap_cstr, Conflicts)
</TT>

would retrieve all constraints that were annotated with <TT>cap_cstr</TT>
and are currently in conflict.
</P>")
]).


conflict_constraints(Cs) :-
	get_repair_state(conflict_constraints of repair_state,Set),
	conflict_constraints(Set,Cs).

conflict_constraints(SetName, Cs) :-
	var(SetName), !,
	error(4, conflict_constraints(SetName, Cs)).
conflict_constraints(SetName, Cs) :-
	conflict_constraints1(SetName, Cs).

    conflict_constraints1(SetName,[]) ?- !,	% make this case more efficient
	get_hashed_set(SetName,Set),
	set_empty(Set).
    conflict_constraints1(SetName,Cs) :-
	get_hashed_set(SetName,Set),
	set_list(Set,List),
	extract_annotations(List,Cs).

:- mode extract_annotations(+,?).
extract_annotations([],[]).
extract_annotations([Susp|Susps],[A|As]) :-
	get_suspension_data(Susp, goal, Goal),
	Goal = monitor_conflict{annotation:A},
	extract_annotations(Susps,As).

:- comment(poss_conflict_vars/2, [
amode: poss_conflict_vars(+,-),
args: ["ConflictSet":"name or handle of a conflict set",
       "Vars": "Variables within conflict constraints"
      ],
summary: "Returns the set of variables within the conflict constraints in ConflictSet.",
desc: "\
The set of variables within the conflict constraints.
This is generally a mixture of tenable and untenable variables."
]).


poss_conflict_vars(Vs) :-
	get_repair_state(conflict_constraints of repair_state,Set),
	poss_conflict_vars(Set,Vs).

poss_conflict_vars(SetName,Vs) :-
	conflict_constraints(SetName,Cs),
	term_variables(Cs,Vs).

:- comment(conflict_vars/1, [
amode: conflict_vars(-),
args:["Vars":"List of variables that are currently non-tenable"],
summary: "Returns the list of variables which are currently non-tenable.",
see_also: [tenable/1, (tent_set)/2],
desc: html("\
<P>
When a variable becomes untenable, it appears in the set of conflict
variable, when it becomes tenable, is disappears.
This primitive returns the list of all currently untenable variables.
Note that all these variables must be reassigned in any solution
(there is no other way to repair untenability).
Variable reassignment can be achieved
by changing the variable's tentative value with tent_set/2,
or by instantiating the variable.
Care should be taken whilst implementing repairs through tentative
value changes since this is a non-monotonic operation: conflicting repairs
may lead to cycles and the computation may not terminate.  
</P>
")
]).

conflict_vars(Vs) :-
	get_repair_state(conflict_vars of repair_state,ConfSet),
	set_list(ConfSet,ConfList),
	extract_variables(ConfList,Vs).

call_satisfied_constraints :-
	suspensions(Susps),
	call_satisfied_suspensions(Susps).

    call_satisfied_suspensions([]).
    call_satisfied_suspensions([S|Ss]):-
	( get_suspension_data(S, goal, G),
	  G = monitor_conflict{constraint:Constraint,prop:0,module:Module} ->
	    call(Constraint)@Module,
	    setarg(prop of monitor_conflict,G,1) % set flag to say goal called
	;
	    true
	),
	call_satisfied_suspensions(Ss).

kill_monitor_conflict :-
	suspensions(Susps),
	kill_suspensions(Susps).

    kill_suspensions([]).
    kill_suspensions([S|Ss]):-
	( get_suspension_data(S, goal, G),
	  G = monitor_conflict{constraint:Constraint,prop:PropFlag,module:Module} ->
	    ( PropFlag == 0 ->
		call(Constraint)@Module
	    ;
	    	true
	    ),
	    kill_suspension(S)
	;
	    true
	),
	kill_suspensions(Ss).
		

% Statistics -----------------------------------------------------------

repair_counter(wake_satisfied).
repair_counter(wake_ground).
repair_counter(wake_conflict).
repair_counter(var_var_unify).
repair_counter(monitor_no_change).
repair_counter(monitor_to_tenable).
repair_counter(monitor_to_untenable).
repair_counter(nonvar_var_unify).
repair_counter(propagate).


inc_code_off(inc(_Counter)).

inc_code_on((
    inc(Counter) :-
	    incval(Counter)
)).

repair_stat(Stats):-
	var(Stats),
	!,
	bagof(Counter=Value,
		(repair_counter(Counter),
		 getval(Counter,Value)),
		Stats).
repair_stat(on) :-
	inc_code_on(Code),
	compile_term(Code).
repair_stat(off) :-
	inc_code_off(Code),
	compile_term(Code).
repair_stat(reset) :-
	not((
	    repair_counter(Counter),
	    setval(Counter,0),
	    fail
	)).

:- repair_stat(reset),repair_stat(off).

% Constant time sets ----------------------------------------------------
% elem(Term,InSet,OnList,Set).
% The list s([...]) maintains elements possibly in the set.
% it is flushed on readiing with set_list/2
%  elements maintain whether they are in the list and whether they are
% in the set. If you are in the set you are guaranteed to be in the list
% but you may remove yourself from the set, but not have been flushed out
% of the list yet.

elem_in(elem(_,1,_,_)) :-
	-?-> true.

elem_new(Term,Set,elem(Term,0,0,Set)).

elem_term(elem(Term,_,_,_),Term).

elem_add(Elem):-
	Elem=elem(_,In,OnList,Set),
	(In == 0 ->
	    setarg(2,Elem,1),
	    (OnList == 0 ->
		Set=s(Elems),
		setarg(1,Set,[Elem|Elems]),
		setarg(3,Elem,1)
	    ;
		true
	    )
	;
	    true
	).

elem_del(Elem) :-
	Elem=elem(_,In,_,_),
	(In == 1 ->
	    setarg(2,Elem,0)
	;
	    true
	).


set_new(s([])).

set_list(Set,Terms) :-
	Set=s(Elems0),
	set_list_(Elems0,Elems,Terms),
	setarg(1,Set,Elems).

	:- mode set_list_(+,-,-).
	set_list_([],[],[]).
	set_list_([Elem|Elems0],Elems,Terms) :-
	    Elem=elem(Term,In,_,_),
	    set_list_(In,Term,Elem,Elems0,Elems,Terms).

	:- mode set_list_(++,?,?,?,-,-).
	set_list_(0,_,Elem,Elems0,Elems,Terms) :-
	    setarg(3,Elem,0),
	    set_list_(Elems0,Elems,Terms).
	set_list_(1,Term,Elem,Elems0,[Elem|Elems],[Term|Terms]) :-
	    set_list_(Elems0,Elems,Terms).

set_empty(Set) :-
	Set=s(Elems0),
	set_empty_(Elems0,Elems),
	setarg(1,Set,Elems).

	:- mode set_empty_(+,-).
	set_empty_([],[]).
	set_empty_([Elem|Elems0],Elems) :-
	    Elem=elem(_,0,_,_),		% fail here if not empty
	    setarg(3,Elem,0),
	    set_empty_(Elems0,Elems).



%----------------------------------------------------------------------
% Changeable value interface for Visualisation
%----------------------------------------------------------------------

:- export suspend_on_change/2.
suspend_on_change(X, Susp) :-
	insert_suspension(X, Susp, ga_chg of repair).

:- export get_changeable_value/2.
get_changeable_value(X, V) :-
	tent_get(X, V).

