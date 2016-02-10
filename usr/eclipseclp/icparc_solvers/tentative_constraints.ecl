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
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Tentative value implementations for some basic constraints
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: tentative_constraints.ecl,v 1.2 2009/07/16 09:11:27 jschimpf Exp $
%
% ----------------------------------------------------------------------


:- module(tentative_constraints).

:- comment(categories, ["Algorithms","Techniques","Constraints"]).
:- comment(summary, "Tentative value implementations for some basic constraints").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2009/07/16 09:11:27 $").
:- comment(copyright, "Cisco Systems").

:- comment(desc, html("
    This library contains tentative value implementations for some basic
    constraints.  It is intended to be used together with lib(tentative),
    which provides the underlying primitives and facilities to create
    tentative variables, manage constraint sets, and to do search.
")).

:- comment(see_also, [library(tentative)]).

:- comment(eg, "
    %
    % The following code implements a solution to the N-queens problem,
    % using a steepest-ascent hill-climbing heuristic.
    %

    :- lib(tentative).
    :- lib(tentative_constraints).

    queens(N, Board) :-
	    dim(Board, [N]),			% make variables
	    tent_set_random(Board, 1..N),	% init tentative values

	    dim(Pos, [N]),			% aux arrays of constants
	    ( foreacharg(I,Pos), for(I,0,N-1) do true ),
	    dim(Neg, [N]),
	    ( foreacharg(I,Neg), for(I,0,-N+1,-1) do true ),

	    CS :~ alldifferent(Board),		% setup constraints ...
	    CS :~ alldifferent(Board, Pos),	% ... in conflict set CS
	    CS :~ alldifferent(Board, Neg),

	    cs_violations(CS, TotalViolation),	% search part
	    steepest(Board, N, TotalViolation),

	    tent_fix(Board),			% instantiate variables
	    cs_clear_satisfied(CS).		% clean up conflict set


    steepest(Board, N, Violations) :-
	    vs_create(Board, Vars),		% create variable set
	    Violations tent_get V0,		% initial violations
	    SampleSize is fix(sqrt(N)),		% neighbourhood size
	    (
		fromto(V0,_V1,V2,0),		% until no violations left
		param(Vars,N,SampleSize,Violations)
	    do
		vs_random_worst(Vars, X),	% get a most violated variable
		tent_minimize_random(		% find a best neighbour
		    (				% nondeterministic move
			random_sample(1..N,SampleSize,I),
			X tent_set I
		    ),
		    Violations,			% violation variable
		    I				% best move-id
		),
		X tent_set I,			% do the move
		Violations tent_get V2		% new violations
	    ).
").

% ----------------------------------------------------------------------

:- ensure_loaded(library(tentative)).
:- reexport tentative.

:- lib(hash).
:- lib(notify_ports).


% ----------------------------------------------------------------------
:- neq_t/3 tent_implements $\= /2.
% ----------------------------------------------------------------------

:- export neq_t/3.
:- export op(700, xfx, $=).
:- comment(neq_t/3, [
    summary:"Tentative value implementation of $\\=/2 arithmetic disequality constraint",
    amode:(neq_t(?,?,+) is det),
    args:["X":"Expression containing tentative variables",
	"Y":"Expression containing tentative variables",
    	"MC":"A monitored_constraint descriptor"],
    see_also:[alldifferent_t/2, alldifferent_t/3, eq_t/3,tent_implements/2],
    desc:html("
	<P>
    	Tentative value implementation of disequality constraint.
	</P><P>
	The violatedness of the constraint is 0 if the disequality holds,
	1 otherwise.
	</P><P>
	The following declaration is in effect, meaning that neq_t/3
	is used whenever $\\= /2 is added to a constraint set:
	<PRE>
	:- neq_t/3 tent_implements $\\= /2.
	</PRE>
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], CS :~ (X $\\= Y).
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    CS = constraint_set(TotalVio{0 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- [X, Y] tent_set [3, 3], CS :~ (X $\\= Y).
    X = X{3 -> 0}
    Y = Y{3 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

neq_t(X, Y, Monitor) :-
	Monitor = monitored_constraint{violations:Viol},
	XYeval tent_is X-Y,
	suspend(eq_demon(XYeval, 0, Viol), 2, XYeval->tentative:tent_chg, Susp),
	enter_suspension_list(suspensions of monitored_constraint, Monitor, Susp),
	eq_demon(XYeval, 0, Viol).



% ----------------------------------------------------------------------
:- eq_t/3 tent_implements $= /2.
% ----------------------------------------------------------------------

:- export eq_t/3.
:- export op(700, xfx, $\=).
:- comment(eq_t/3, [
    summary:"Tentative value implementation of $=/2 arithmetic equality constraint",
    amode:(eq_t(?,?,+) is det),
    args:["X":"Expression containing tentative variables",
	"Y":"Expression containing tentative variables",
    	"MC":"A monitored_constraint descriptor"],
    see_also:[alldifferent_t/2, alldifferent_t/3, neq_t/3,tent_implements/2],
    desc:html("
	<P>
    	Tentative value implementation of equality constraint.
	</P><P>
	The violatedness of the constraint is 0 if the equality holds,
	1 otherwise.
	</P><P>
	The following declaration is in effect, meaning that eq_t/3
	is used whenever $= /2 is added to a constraint set:
	<PRE>
	:- eq_t/3 tent_implements $= /2.
	</PRE>
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], CS :~ (X $= Y).
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- [X, Y] tent_set [3, 3], CS :~ (X $= Y).
    X = X{3 -> 0}
    Y = Y{3 -> 0}
    CS = constraint_set(TotalVio{0 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

eq_t(X, Y, Monitor) :-
	Monitor = monitored_constraint{violations:Viol},
	XYeval tent_is X-Y,
	suspend(eq_demon(XYeval, 1, Viol), 2, XYeval->tentative:tent_chg, Susp),
	enter_suspension_list(suspensions of monitored_constraint, Monitor, Susp),
	eq_demon(XYeval, 1, Viol).

    :- demon eq_demon/3.
    eq_demon(X, Yes, Viol) :-
	X tent_get Xt,
	( Xt =\= 0 -> Viol tent_set Yes ; No is 1-Yes, Viol tent_set No ).


% ----------------------------------------------------------------------
:- integral_t/3 tent_implements integral/2.
% ----------------------------------------------------------------------

:- export integral_t/3.
:- comment(integral_t/3, [
    summary:"Tentative value implementation of approximate integrality",
    amode:(integral_t(?,?,+) is det),
    args:["X":"Tentative variables",
	"Epsilon":"Allowed deviation from integrality",
    	"MC":"A monitored_constraint descriptor"],
    see_also:[eq_t/3, neq_t/3,tent_implements/2],
    desc:html("
	<P>
    	Tentative value implementation of approximate integrality constraint.
	</P><P>
	The violatedness of the constraint is 0 if X's value is within
	tolerance Epsilon of the nearest integers, 1 otherwise.
	</P><P>
	The following declaration is in effect, meaning that integral_t/3
	is used whenever integral/2 is added to a constraint set:
	<PRE>
	:- integral_t/3 tent_implements integral/2.
	</PRE>
	</P>
    "),
    eg:"
    ?- X tent_set 3.2, CS :~ integral(X, 0.1).
    X = X{3.2 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- X tent_set 3.2, CS :~ integral(X, 0.1), X tent_set 3.01.
    X = X{3.01 -> 0}
    CS = constraint_set(TotalVio{0 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

integral_t(X, Epsilon, Monitor) :- (var(X);number(X)), !,
	Monitor = monitored_constraint{violations:Viol},
	suspend(integer_demon(X, Epsilon, Viol), 2, X->tentative:tent_chg, Susp),
	enter_suspension_list(suspensions of monitored_constraint, Monitor, Susp),
	integer_demon(X, Epsilon, Viol).
	
    :- demon integer_demon/3.
    integer_demon(X, Epsilon, Viol) :-
	X tent_get Xt,
	( abs(Xt-round(Xt)) > Epsilon -> Viol tent_set 1 ; Viol tent_set 0 ).



% ----------------------------------------------------------------------
:- alldifferent_t/2 tent_implements alldifferent/1.
% ----------------------------------------------------------------------

:- export alldifferent_t/2.
:- comment(alldifferent_t/2, [
    summary:"Tentative value implementation of alldifferent constraint",
    amode:(alldifferent_t(+,+) is det),
    args:["Cs":"A list or array of variables/values",
    	"MC":"A monitored_constraint descriptor"],
    see_also:[alldifferent_t/3,neq_t/3,tent_implements/2],
    desc:html("
    	Tentative value implementation of alldifferent constraint.
	<P>
	The violatedness of the constraint is the number of identical
	value pairs among its arguments.
	<P>
	The following declaration is in effect, meaning that alldifferent_t/2
	is used whenever alldifferent/1 is added to a constraint set:
	<PRE>
	:- alldifferent_t/2 tent_implements alldiffernet/1.
	</PRE>
    "),
    eg:"
    ?- Xs = [A,B,C], Xs tent_set [1,2,3], CS :~ alldifferent(Xs).
    Xs = [A{1 -> 0}, B{2 -> 0}, C{3 -> 0}]
    CS = constraint_set(TotalVio{0 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

alldifferent_t(Xs0, Monitor) :-
	Monitor = monitored_constraint{violations:Viol},
	Viol tent_set 0,
	arrayify(Xs0, Xs),
	functor(Xs, F, N),
	functor(XCs, F, N),
	( for(I,1,N), param(Xs,XCs) do
	    arg(I, Xs, X), arg(I, XCs, X+0)
	),
	alldifferent1(XCs, Monitor).

    	
% ----------------------------------------------------------------------
:- alldifferent_t/3 tent_implements alldifferent/2.
% ----------------------------------------------------------------------

:- export alldifferent_t/3.
:- comment(alldifferent_t/3, [
    summary:"Tentative value implementation of alldifferent/2 constraint",
    amode:(alldifferent_t(+,+,+) is det),
    args:["Xs":"A list or array of variables/values",
    	"Cs":"A list or array of numbers",
    	"MC":"A monitored_constraint descriptor"],
    see_also:[alldifferent_t/2,neq_t/3,tent_implements/2],
    desc:html("
    	Tentative value implementation of alldifferent constraint with
	offsets. The difference releationship must hold between the
	values resulting from adding each variable Xi to its corresponding
	offset Ci.
	<P>
	The violatedness of the constraint is the number of identical
	value pairs among these Xi+Ci values.
	<P>
	The following declaration is in effect, meaning that alldifferent_t/3
	is used whenever alldifferent/2 is added to a constraint set:
	<PRE>
	:- alldifferent_t/3 tent_implements alldiffernet/2.
	</PRE>
    "),
    eg:"
    ?- Xs = [A,B,C], Xs tent_set [1,2,3], CS :~ alldifferent(Xs, [1,0,0]).
    Xs = [A{1 -> 1}, B{2 -> 1}, C{3 -> 0}]
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

alldifferent_t(Xs0, Cs0, Monitor) :-
	Monitor = monitored_constraint{violations:Viol},
	Viol tent_set 0,
	arrayify(Xs0, Xs),
	arrayify(Cs0, Cs),
	functor(Xs, F, N),
	functor(XCs, F, N),
	( for(I,1,N), param(Xs,Cs,XCs) do
	    arg(I, Xs, X), arg(I, Cs, C), arg(I, XCs, X+C)
	),
	alldifferent1(XCs, Monitor).


    alldifferent1(Xs, Monitor) :-
	Monitor = monitored_constraint{violations:Viol},
	% Make a hash table recording how many times each value occurs
	hash_create(Occ),
	% Create demon
	suspend(alldifferent_demon(Xs, Viol, Occ, Receiver), 3, Xs->tentative:tent_chg, Susp),
	enter_suspension_list(suspensions of monitored_constraint, Monitor, Susp),
	% compute the initial violations
	( foreacharg(X+C,Xs,I), param(Occ,Viol,Xs,Receiver) do
	    register_for_notification(X, I, Receiver),
	    Xt is tent_get(X) + C,
	    ( hash_get(Occ, Xt, Is) ->
		IIs = [I|Is],
		% update table
		hash_set(Occ, Xt, IIs),
		% increment var violation for all in [I|Is]
		( foreach(I,Is), param(Xs) do
		    arg(I, Xs, XI+_C),
		    var_inc_violations(XI, 1)
		),
		length(Is, NewColl),
		% increase violation for X by length(Is)
		var_inc_violations(X, NewColl),
		% increase global violation by length(Is)
		NewViol is tent_get(Viol) + NewColl,
		tent_set(Viol, NewViol)
	    ;
		hash_set(Occ, Xt, [I])
	    )
	).

    :- demon alldifferent_demon/4.
    alldifferent_demon(Xs, Viol, Occ, Receiver) :-
	foreachnotification(alldifferent_demon,
		I:chg(OldX,NewX), [Xs,Viol,Occ], Receiver, _Status, (
	    arg(I, Xs, XI+CI),
	    Old is OldX + CI,
	    New is NewX + CI,
	    hash_get(Occ, Old, IIs),
	    once delete(I, IIs, Is),
	    ( Is = [] ->
		LossXI = 0,
	    	hash_delete(Occ, Old)
	    ;
		length(Is, LossXI),
		hash_set(Occ, Old, Is),
		% Other variables with Old value lose one violation
		( foreach(I1,Is), param(Xs) do
		    arg(I1, Xs, XI1+_C),
		    var_inc_violations(XI1, -1)
		)
	    ),
	    ( hash_get(Occ, New, Js) ->
		length(Js, GainXI),
		hash_set(Occ, New, [I|Js]),
		% Other variables with New value get an extra violation
		( foreach(J,Js), param(Xs) do
		    arg(J, Xs, XJ+_C),
		    var_inc_violations(XJ, 1)
		)
	    ;
		GainXI = 0,
		hash_set(Occ, New, [I])
	    ),
	    DeltaXi is GainXI-LossXI,
	    ( DeltaXi =\= 0 ->
		% adjust violations for XI
		var_inc_violations(XI, DeltaXi),
		% adjust global violations
		NewViol is tent_get(Viol) + DeltaXi,
		tent_set(Viol, NewViol)
	    ;
		true
	    )
	)).


% ----------------------------------------------------------------------
% Auxiliaries
% ----------------------------------------------------------------------

arrayify(Thing, Array) :-
    	( functor(Thing, [], _) ->
	    Array = Thing
	;
	    Array =.. [[]|Thing]
	).


