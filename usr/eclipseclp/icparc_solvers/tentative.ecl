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
% A framework for Local Search based on tentative values
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: tentative.ecl,v 1.5 2013/01/23 19:43:04 jschimpf Exp $
%
% ----------------------------------------------------------------------


:- module(tentative).

:- comment(categories, ["Techniques","Constraints"]).
:- comment(summary, "A framework for Local Search based on tentative values").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2013/01/23 19:43:04 $").
:- comment(copyright, "Cisco Systems").

:- comment(see_also, [library(tentative_constraints)]).

:- comment(desc, html("
    <h3>Overview</h3>
<P>
    This is a library for implementing Local Search algorithms.
    It is intended as a successor for library(repair).
</P><P>
    This library provides the following concepts and primitives:
<UL><LI>
    A variable can be given a <EM>tentative value</EM>. This is a value which
    is attached to the variable, but does not constrain the variable.
</LI><LI>
    Tentative values are observed by <EM>monitored constraints</EM>. A monitored
    constraint checks whether a constraint is satisfied given the current
    tentative values of its variables, or computes a measure of violatedness
    for a tentatively violated constraint.
</LI><LI>
    <EM>Violatedness</EM> can be attached to constraints and to variables.
    Typically, when a constraint is tentatively violated, this increases
    the violatedness count of the constraint itself, and the violatedness
    count of those variables that can be made responsible for the violation.
</LI></UL>
    Actual constraint implementations can be found in the library
    <EM>lib(tentative_constraints)</EM>.
</P>

    <h3>Tentative Values</h3>
<P>
    A tentative value (TV) can be an atomic term or a (possibly nonground)
    compound term. It is a conscious design choice not to allow variable
    tentative values.
    A tentative value can be attached to a variable, and freely changed.
    It is not possible to remove a variable's tentative value once
    it has had one, it can only be replaced by a different one.
    The change of tentative value can be used as a trigger condition for
    waking delayed goals.
</P><P>
    Instantiating a tentative variable to a value V is equivalent to first
    setting/changing its tentative value to V, and then instantiating to V.
</P><P>
    When two tentative variables get unified, one of them acquires the
    tentative value of the other (which one is undefined).  Such unifications
    do not fit well with the concepts of this library and are best avoided.
</P><P>
    Variables with tentative value are printed in the following format:
<PRE>
	X{99 -> 7}
</PRE>
    where the first number (99) is the tentative value, and the second
    number (7) is the variable's violation count.
</P><P>
    The primitives related to tentative values are:
<blockquote><dl>
    <dt>has_tent_value(?X)</dt>
	<dd>X has a tentative (or definitive) value</dd>
    <dt>tent_get(?X, -Val)</dt>
	<dd>get tentative value</dd>
    <dt>tent_set(?X, -Val)</dt>
	<dd>set tentative value</dd>
    <dt>tent_set_all(?Xs, +Val)</dt>
	<dd>set multiple identical tentative values</dd>
    <dt>tent_set_random(?Xs, +Range)</dt>
	<dd>set multiple random tentative values</dd>
    <dt>tent_fix(?X)</dt>
	<dd>instantiate to tentative value</dd>
    <dt>var_get_violations(?X, -Violations)</dt>
	<dd>get the number of violations the variable is currently involved in</dd>
    <dt>var_inc_violations(?Var, +Delta)</dt>
	<dd>add Delta to Var's violation counter</dd>
</dl></blockquote>
    All these operations are undone on backtracking.
</P>


    <h3>Variable Sets</h3>
<P>
    Tentative variables can be grouped into indexed sets, from which elements
    (or their index) can then be selected according to different criteria.
    The corresponding predicates are:
<blockquote><dl>
    <dt>vs_create(+Vars, -VarSet)</dt>
        <dd>construct a variable set from the variables in Vars</dd>
    <dt>vs_all(+VS, -Vs)</dt>
        <dd>get a list of all the variables in the set</dd>
    <dt>vs_element(+VS, +I, -V)</dt>
        <dd>get a particular variable from the set</dd>
    <dt>vs_member(+VS, -V)</dt>
        <dd>enumerate all variables from the set</dd>
    <dt>vs_random(+VS, -Vs), vs_random_index(+VS, -Is)</dt>
        <dd>pick a random variable from the set</dd>
    <dt>vs_random_violated(+VS, -Vs), vs_random_violated_index(+VS, -Is)</dt>
        <dd>pick a random violated variable from the set</dd>
    <dt>vs_all_violated(+VS, -Vs), vs_all_violated_index(+VS, -Is)</dt>
        <dd>get a list of all violated variables from the set</dd>
    <dt>vs_violated(+VS, -V), vs_violated_index(+VS, -I)</dt>
        <dd>enumerate all violated variables from the set</dd>
    <dt>vs_random_worst(+VarSet, -WorstVar), vs_random_worst_index(+VarSet, -I)</dt>
    	<dd>pick a random variable with maximum violations from the set</dd>
    <dt>vs_all_worst(+VS, -Vs), vs_all_worst_index(+VS, -Is)</dt>
        <dd>get a list of all the maximally violated variables from the set</dd>
    <dt>vs_worst(+VS, -V), vs_worst_index(+VS, -I)</dt>
        <dd>enumerate all maximally violated variables from the set</dd>
</dl></blockquote>
</P>


    <h3>Constraint Sets</h3>
<P>
    To monitor a constraint's tentative violatedness, it must be added
    to a constraint set.  The predicates to create, add and remove
    constraints from a constraint set are:
<blockquote><dl>
    <dt>CS :~ C</dt>
    	<dd>add a constraint to the constraint set</dd>
    <dt>cs_create(-CS, +Options)</dt>
    	<dd>create an empty constraint set</dd>
    <dt>cs_clear_all(+CS)</dt>
    	<dd>remove all constraints from the constraint set</dd>
    <dt>cs_clear_satisfied(+CS)</dt>
    	<dd>remove all satisfied constraints from the constraint set</dd>
</dl></blockquote>
    The total violation count of all the constraints in the set can be
    accessed through the following predicates:
<blockquote><dl>
    <dt>cs_violations(+CS, -VioVar)</dt>
    	<dd>get a tentative variable reflecting violatedness of the constraint set</dd>
    <dt>cs_current_violations(+CS, -Vio)</dt>
    	<dd>get the current violatedness of the constraint set (integer)</dd>
</dl></blockquote>
    Constraints from the constraint set can be selected according to
    different criteria through the following predicates:
<blockquote><dl>
    <dt>cs_random_worst(+CS, -C)</dt>
    	<dd>get a random worst violated constraint from the constraint set</dd>
    <dt>cs_all_worst(+CS, -Cs)</dt>
    	<dd>get all worst violated constraints from the constraint set</dd>
    <dt>cs_all_violated(+CS, -Cs)</dt>
    	<dd>get all violated constraints from the constraint set</dd>
    <dt>cs_random_violated(+CS, -Cs)</dt>
    	<dd>get a random violated constraint from the constraint set</dd>
    <dt>cs_all(+CS, -Cs)</dt>
    	<dd>get all constraints from the constraint set</dd>
</dl></blockquote>
</P>


    <h3>Invariants</h3>
<P>
    There is currently one primitive to maintain arithmetic invariants:
<blockquote><dl>
    <dt>-Res tent_is +Expr</dt>
    	<dd>the tentative value of Res is the tentative result of Expr</dd>
</dl></blockquote>
</P>


    <h3>Search and Randomised Primitives</h3>
<P>
    The following primitives support the implementation of the actual
    Local Search routines:
<blockquote><dl>
    <dt>tent_minimize_random(MoveGenerator, Violations, MoveId)</dt>
    	<dd>Find a best neighbour using MoveGenerator</dd>

    <dt>random_element(+Range, -Value)</dt>
	<dd>Pick a random element from Range</dd>

    <dt>random_sample(+Range, +N, -Value)</dt>
    	<dd>Succeed N times with random values from Range</dd>
</dl></blockquote>
</P>


    <h3>Tracing</h3>
<P>
    A simple tracing facility is provided via
<blockquote><dl>
    <dt>tent_trace_array(+Stream, +Name, +ArrayList)</dt>
    	<dd>Print a message whenever a tentative value changes</dd>
</dl></blockquote>

    Also, the Visualisation Tools can be used with this library,
    by creating viewables with type changeable(tentative,Type).
</P>


    <h3>Constraint implementation interface</h3>
<P>
    Constraints are implemented by an implementation predicate. A constraint
    is linked to its implementation predicate by a tent_implements/2
    declaration, e.g.
<pre>
	:- alldifferent_t/2 tent_implements alldifferent/1.
</pre>
    The implementation predicate must have one more argument than the
    constraint itself.  The extra (last) argument is a structure
<pre>
	struct(monitored_constraint(
		alias,		% the constraint goal (or equivalent term)
		violations,	% a tentative variable
		suspensions	% suspensions of the monitoring goals
	)
</pre>
    The implementation predicate is supposed to update the constraint's
    violation TV plus the violation counters of the variables that occur
    in the constraint. It should do this by suspending on the variable's
    tent_chg list, and by registering for exact change notification via:
<blockquote><dl>
    <dt>register_for_notification(?TV, +Tag, ?Receiver)</dt>
    	<dd>register to receive messages about changes to TV's tentative value</dd>
</dl></blockquote>
     This messaging facility is based upon the primitive in lib(notify_ports).
</P>


    <h3>Constraints</h3>
<P>
    Actual constraint implementations can be found in the library
    <EM>lib(tentative_constraints)</EM>.
</P>


    <h3>Example</h3>
<P>
    See <EM>lib(tentative_constraints)</EM>.
</P>
")).


%----------------------------------------------------------------------
% Imports
%----------------------------------------------------------------------

:- lib(linearize).
:- lib(notify_ports).


%----------------------------------------------------------------------
% Variables with tentative values
%----------------------------------------------------------------------

:- meta_attribute(tentative, [
	print:print_tentative/2,
	unify:unify_tentative/2
    ]).

:- local struct(tentative(
    	value,			% the corresponding variable
	tentval,		% current tentative value
	violations,		% (integer, not enforced)
	send_port,		% notification send port
	tent_chg,		% waking list
	sums			% list of sums that its value contributes to
    )).

:- local struct(summand(
    	coeff,			% constant number
    	old,			% updateable number
	sum			% tentative variable
    )).

:- export print_tentative/2.
print_tentative(_{tentative{tentval:Val,violations:Viol}}, Print) ?-
	Print = (Val->Viol).


:- export has_tent_value/1.
:- comment(has_tent_value/1, [
    summary:"X has a tentative value (succeeds also for X nonvar)",
    args:["X":"Any term"],
    amode:(has_tent_value(?) is semidet),
    see_also:[tent_set/2,tent_get/2,tent_fix/1],
    desc:html("
    	Succeeds if X is a variable with a tentative value, or if X is
	a nonvariable (i.e. has a definitive value).
    "),
    eg:"
    ?- tent_set(X, 27), has_tent_value(X).
    X = X{27 -> 0}
    Yes (0.00s cpu)

    ?- has_tent_value(_).
    No (0.00s cpu)

    ?- has_tent_value(35).
    Yes (0.00s cpu)
    "
]).

has_tent_value(_X{tentative{}}) ?- !.
has_tent_value(X) :- nonvar(X).


get_attribute(_{Attr0}, Attr) ?-
	nonvar(Attr0), !,
	Attr = Attr0.


:- export tent_get/2.
:- export op(700,xfx,tent_get).
:- comment(tent_get/2, [
    summary:"Get X's tentative value",
    amode:(tent_get(?,-) is det),
    exceptions:[tentative_value_not_set:"X (or a subterm of X) has no tentative value"],
    args:["X":"Any term, typically a variable", "TV":"Will be bound to a (nonvar) term"],
    see_also:[tent_set/2,has_tent_value/1,tent_fix/1],
    desc:html("
    	Get a variable's (or a whole term's) tentative value, i.e. create a
	term TV that represents the current tentative value of X. 
	<UL>
    	<LI>If X is a tentative variable, TV is bound to the tentative value.</LI>
	<LI>If X is a variable without tentative value, an error is raised.</LI>
    	<LI>If X is atomic, its tentative value is the same as X.</LI>
	<LI>If X is a compound term, TV will be bound to a corresponding compound
	term with all variables replaced by their tentative values. The resulting
	term TV will be a proper instance of X.</LI>
	</UL>
    "),
    eg:"
    ?- tent_set(X, 27), tent_get(X, TV).
    X = X{27 -> 0}
    TV = 27
    Yes (0.00s cpu)

    ?- tent_set(X, 27), tent_set(X, 99), tent_get(X, TV).
    X = X{99 -> 0}
    TV = 99
    Yes (0.00s cpu)

    ?- X = foo(Y), tent_set(X, foo(27)), X tent_get TV.
    X = foo(Y{27 -> 0})
    Y = Y{27 -> 0}
    TV = foo(27)
    Yes (0.00s cpu)

    ?- tent_get(27, TV).
    TV = 27
    Yes (0.00s cpu)

    ?- tent_get(X, TV).
    uncaught exception in exit_block(tentative_value_not_set)
    Abort

    ?- tent_get(foo(X), TV).
    uncaught exception in exit_block(tentative_value_not_set)
    Abort
    "
]).

tent_get(_X{tentative{tentval:V0}}, TVal) ?- !,
	TVal = V0.
tent_get(X, _TVal) :-
	var(X),
	exit_block(tentative_value_not_set).
tent_get(Term, TValTerm) :-
	compound(Term),
	!,
	functor(Term,F,A),
	functor(TValTerm,F,A),
	( for(I,1,A), param(Term,TValTerm) do
	    arg(I,Term,Termi),
	    arg(I,TValTerm,TValTermi),
	    Termi tent_get TValTermi
	).
tent_get(X, TVal) :-
	atomic(X),
	TVal = X.


:- export var_get_violations/2.
:- comment(var_get_violations/2, [
    summary:"Get X's violation count",
    amode:(var_get_violations(?,-) is det),
    exceptions:[tentative_value_not_set:"X is a variable without tentative value"],
    args:["X":"A a tentative variable or instantiated term",
    	"Violations":"Variable, will be bound to a number"],
    see_also:[tent_set/2, var_inc_violations/2],
    desc:html("
	Get X's violations count. The violation count of a tentative variable
	is the cumulative result of all previous increments of that count.
	The violation count of a nonvariable term is 0.
    "),
    eg:"
    ?- tent_set(X, 27), var_get_violations(X, V).
    X = X{27 -> 0}
    V = 0
    Yes (0.00s cpu)

    ?- tent_set(X, 27), var_inc_violations(X, 3), var_get_violations(X, V).
    X = X{27 -> 3}
    V = 3
    Yes (0.00s cpu)

    ?- var_get_violations(a, V).
    V = 0
    Yes (0.00s cpu)

    ?- var_get_violations(_, V).
    uncaught exception in exit_block(tentative_value_not_set)
    Abort
    "
]).

var_get_violations(_X{tentative{violations:Vio0}}, Vio) ?- !,
	Vio = Vio0.
var_get_violations(X, _TVal) :-
	var(X), !,
	exit_block(tentative_value_not_set).
var_get_violations(_, 0).


:- export tent_set/2.
:- export op(700,xfx,tent_set).
:- comment(tent_set/2, [
    summary:"Set X's tentative value to TV",
    amode:(tent_set(-,+) is det),
    amode:(tent_set(+,+) is semidet),
    fail_if:"TV is not an instance of X",
    exceptions:["4":"TV is not sufficiently instantiated"],
    args:["X":"Any term, typically a variable", "TV":"A (nonvar) term"],
    see_also:[tent_get/2,has_tent_value/1,tent_fix/1],
    desc:html("
	Give a variable (or several variables within a term) a tentative value.
	A tentative value cannot be itself a variable.
	<UL>
	<LI>If X is a variable without tentative value, it will be given the
	tentative value TV. In addition, its violatedness count will be
	initialised to 0.</LI>
	<LI>If X already has a tentative value, its tentative value is
	changed to TV. (Note that this may trigger further computation!)
	The violatedness count remains unchanged.</LI>
	<LI>If X is already instantiated, TV must be a strict instance of X.
	Variables within X are given tentative values from the corresponding
	instantiated parts of TV.</LI>
	</UL>
    "),
    eg:"
    ?- tent_set(X, 27).
    X = X{27 -> 0}
    Yes (0.00s cpu)

    ?- tent_set(X, 27), tent_set(X, 99).
    X = X{99 -> 0}
    Yes (0.00s cpu)

    ?- tent_set(foo(X), foo(27)).
    X = X{27 -> 0}
    Yes (0.00s cpu)

    ?- tent_set(X, Y).
    instantiation fault in X tent_set Y
    Abort
    "
]).

tent_set(X, TVal) :- var(TVal), !,
	error(4, tent_set(X, TVal)).
tent_set(X, TVal) :-
	tent_set1(X, TVal).

    tent_set1(X, TVal) :- free(X),
	new_attribute(X, TVal, Attr),
	add_attribute(X, Attr).
    tent_set1(X{Attr}, TVal) ?-
	( var(Attr) ->
	    new_attribute(X, TVal, Attr)
%	; var(TVal) ->
%	    setarg(tentval of tentative, Attr, _)
	;
	    tent_set_attr(Attr, TVal)
	).
%    tent_set1(X, _) :-		% lib(repair) fails here!
%    	nonvar(X).
    tent_set1(X, TX) :-
	compound(X),
	functor(X, F, A),
	functor(TX, F, A),
	( for(I,1,A), param(X,TX) do
	    arg(I, X, Xi),
	    arg(I, TX, Ti),
	    tent_set(Xi, Ti)
	).
    tent_set1(X, TVal) :-
    	atomic(X),
	X = TVal.

:- export tent_set_all/2.
:- comment(tent_set_all/2, [
    summary:"Set the tentative values of all variables within Vars to the same value TV",
    amode:(tent_set_all(?,+) is det),
    args:["Vars":"A term", "TV":"A (nonvar) term"],
    see_also:[tent_set/2,tent_set_random/2],
    desc:html("
	Assign the tentative value TV to each variable in the term Vars.
	Vars would typically be a list (or array) of variables.
    "),
    eg:"
    ?- tent_set_all([A, B, C], 3).
    A = A{3 -> 0}
    B = B{3 -> 0}
    C = C{3 -> 0}
    Yes (0.00s cpu)

    ?- length(L, 5), tent_set_all(L, 3).
    L = [Xi{3 -> 0}, Xi{3 -> 0}, Xi{3 -> 0}, Xi{3 -> 0}, Xi{3 -> 0}]
    Yes (0.00s cpu)
    "
]).

tent_set_all(X, TVal) :- var(TVal), !,
	error(4, tent_set_all(X, TVal)).
tent_set_all(X, TVal) :-
	tent_set_all1(X, TVal).

    tent_set_all1(X, TVal) :- free(X), !,
	new_attribute(X, TVal, Attr),
	add_attribute(X, Attr).
    tent_set_all1(X{Attr}, TVal) ?- !,
	( var(Attr) ->
	    new_attribute(X, TVal, Attr)
	;
	    tent_set_attr(Attr, TVal)
	).
    tent_set_all1(X, TX) :-
	( foreacharg(Xi,X), param(TX) do
	    tent_set_all1(Xi, TX)
	).

:- export tent_set_random/2.
:- comment(tent_set_random/2, [
    summary:"Set the tentative value of each variable within Vars to a random value from the given Range",
    amode:(tent_set_random(?,++) is det),
    args:["Vars":"A term", "Values":"Specification of possible values"],
    see_also:[tent_set/2,tent_set_all/2],
    desc:html("
	Assign to each variable in Vars a random value from the given
	specification of possible values.  The Values specification can be
	<DL>
	<DT>Min..Max</DT>
	    <DD>A range of integers from Min to Max</DD>
	<DT>List</DT>
	    <DD>A list of possible values</DD>
	<DT>Array</DT>
	    <DD>An array of possible values</DD>
	</DL>
    "),
    eg:"
    ?- length(L, 5), tent_set_random(L, 1 .. 9).
    L = [Xi{2 -> 0}, Xi{3 -> 0}, Xi{7 -> 0}, Xi{5 -> 0}, Xi{3 -> 0}]
    Yes (0.00s cpu)

    ?- length(L, 5), tent_set_random(L, [a, e, i, o, u]).
    L = [Xi{o -> 0}, Xi{e -> 0}, Xi{o -> 0}, Xi{e -> 0}, Xi{e -> 0}]
    Yes (0.00s cpu)

    ?- length(L, 5), tent_set_random(L, [](a, e, i, o, u)).
    L = [Xi{u -> 0}, Xi{i -> 0}, Xi{a -> 0}, Xi{o -> 0}, Xi{a -> 0}]
    Yes (0.00s cpu)
    "
]).

tent_set_random(X, Range) :- free(X), !,
	random_element(Range, TVal),
	new_attribute(X, TVal, Attr),
	add_attribute(X, Attr).
tent_set_random(X{Attr}, Range) ?- !,
	random_element(Range, TVal),
	( var(Attr) ->
	    new_attribute(X, TVal, Attr)
	;
	    tent_set_attr(Attr, TVal)
	).
tent_set_random(X, Range) :-
	( foreacharg(Xi,X), param(Range) do
	    tent_set_random(Xi, Range)
	).



:- export tent_fix/1.
:- comment(tent_fix/1, [
    summary:"Instantiate X to its tentative value",
    args:["X":"Any term, typically containing tentative variables"],
    amode:(tent_fix(?) is det),
    exceptions:[tentative_value_not_set:"X (or a subterm of X) has no tentative value"],
    see_also:[tent_set/2,has_tent_value/1,tent_get/2],
    desc:html("
	This is a shorthand for
	<PRE>
		tent_get(X, TV), X = TV.
	</PRE>
    "),
    eg:"
    ?- tent_set(X, 27), tent_fix(X).
    X = 27
    Yes (0.00s cpu)

    ?- X = foo(_, _), tent_set(X, foo(27, 99)), tent_fix(X).
    X = foo(27, 99)
    Yes (0.00s cpu)
    "
]).

tent_fix(Term) :-
	term_variables(Term, Vars),
	( foreach(V,Vars), foreach(T,Tents) do
	    V tent_get T
	),
	Vars = Tents.	% instantiate all vars atomically



:- export var_inc_violations/2.
:- comment(var_inc_violations/2, [
    summary:"Increment X's violation count by Delta",
    amode:(var_inc_violations(?,+) is semidet),
    args:["X":"A a tentative variable", "Delta":"A number"],
    fail_if:"Fails if X is a variable without tentative value",
    see_also:[tent_set/2,has_tent_value/1,tent_get/2,var_get_violations/2],
    desc:html("
	Increment X's violation count by Delta. X should be a variable with
	a tentative value. The initial violation count for every variable is 0.
	<P>
	Calling var_inc_violations/2 on a nonvariable has no effect and
	silently succeeds.
    "),
    eg:"
    ?- tent_set(X, 27), var_inc_violations(X, 3).
    X = X{27 -> 3}
    Yes (0.00s cpu)

    ?- tent_set(X, 27), var_inc_violations(X, 3), var_inc_violations(X, 1).
    X = X{27 -> 4}
    Yes (0.00s cpu)

    ?- tent_set(X, 27), var_inc_violations(X, 3), var_inc_violations(X, -1).
    X = X{27 -> 2}
    Yes (0.00s cpu)

    ?- var_inc_violations(foo, 3).
    Yes (0.00s cpu)

    ?- var_inc_violations(_, 3).
    No (0.00s cpu)
    "
]).

% Error checking has been omitted for efficiency (would cost two
% extra tests). We just fail now if the argument is not a tent var.
var_inc_violations(_{Attr}, C) ?-
	nonvar(Attr),
	Attr = tentative{violations:V0},
	V1 is V0+C,
	setarg(violations of tentative, Attr, V1).
var_inc_violations(Const, _) :-
	nonvar(Const).

/*
var_inc_violations(_{Attr}, C) ?-
	( nonvar(Attr) ->
	    Attr = tentative{violations:V0},
	    V1 is V0+C,
	    setarg(violations of tentative, Attr, V1)
	;
	    exit_block(tentative_value_not_set)
	).
var_inc_violations(Const, _) :-
	nonvar(Const).
*/


:- export tent_set_attr/2.
tent_set_attr(Attr, TVal) :-
	Attr = tentative{tentval:TValOld,sums:Sums,send_port:Port},
	% swap order of next 2 lines?
	setarg(tentval of tentative, Attr, TVal),
	( TValOld == TVal ->
	    true
	;
	    update_sums(Sums, TVal),
	    notify_change(Port, TValOld, TVal),
	    schedule_suspensions(tent_chg of tentative, Attr),
	    wake
	).

    new_attribute(X, TVal, Attr) :-
	Attr = tentative{value:X,tentval:TVal,sums:[],violations:0},
	init_suspension_list(tent_chg of tentative, Attr).

    notify_change(Port, _, _) :- var(Port), !.
    notify_change(Port, Old, New) :-
    	send_notification(Port, chg(Old,New)).


%
% The unify handler
%

unify_tentative(_Y, AttrX) :-
	var(AttrX).
unify_tentative(_Y, AttrX) :-
	compound(AttrX),
	unify_any_tentative(_Y, AttrX).

    unify_any_tentative(_Y{AttrY}, AttrX) ?-
	unify_tent_tent(AttrY, AttrX).
    unify_any_tentative(Y, AttrX) :-
    	nonvar(Y),
	AttrX = tentative{tentval:X,sums:Sums,send_port:Port},
	( X == Y ->
	    true
	;
	    % essentially tent_set_attr(AttrX, Y)
	    setarg(tentval of tentative, AttrX, Y),
	    update_sums(Sums, Y),
	    notify_change(Port, X, Y),
	    schedule_suspensions(tent_chg of tentative, AttrX)
	).
	% close_sender(Port).

    unify_tent_tent(AttrY, AttrX) :-
    	var(AttrY),
	AttrY = AttrX.
    unify_tent_tent(AttrY, AttrX) :-
    	compound(AttrY),
	% X disappears, Y survives
	AttrX = tentative{tentval:TX,violations:VX,sums:SumsX,send_port:PortX},
	AttrY = tentative{tentval:TY,violations:VY,sums:SumsY,send_port:PortY},
	( TX==TY ->
	    true
	;
	    % essentially tent_set_attr(AttrX, TY)
	    setarg(tentval of tentative, AttrX, TY),
	    update_sums(SumsX, TY),
	    notify_change(PortX, TX, TY),
	    schedule_suspensions(tent_chg of tentative, AttrX)
	),
	VXY is VX+VY,
	setarg(violations of tentative, AttrX, VXY),
	append(SumsY, SumsX, SumsYX),	% TODO: merge duplicates?
	setarg(sums of tentative, AttrY, SumsYX),
	merge_suspension_lists(tent_chg of tentative, AttrX, tent_chg of tentative, AttrY),
	merge_senders(PortX, PortY).


%----------------------------------------------------------------------
% Constraint implementation support
%----------------------------------------------------------------------

% Register a tentative variable as contributing Coeff/Unit
% to the sum Sum, i.e. when the TV of X increases by N, the Sum
% increases by N*Coeff. Sum need not have a TV yet!

register_for_sum(_X{Attr}, Coeff, Sum) ?- !,
	( nonvar(Attr) ->
	    Attr = tentative{tentval:Xt,sums:Sums},
	    % nonvar(Coeff), var(Sum)
	    setarg(sums of tentative, Attr,
		    [summand{coeff:Coeff,old:Xt,sum:Sum}|Sums])
	;
	    exit_block(tentative_value_not_set)
	).
register_for_sum(X, _Coeff, _Sum) :-
	var(X),
	exit_block(tentative_value_not_set).
register_for_sum(X, _Coeff, _Sum) :-
	nonvar(X).


update_sums([], _New).
update_sums([Summand|Sums], New) :-
	change_summand(Summand, New),
	update_sums(Sums, New).


% Contribute X to Sum, i.e. add current Xtent to Sumtent and
% register for updates

%:- export extend_sum/3.
extend_sum(_X{Attr}, Coeff, Sum) ?- !,
	( nonvar(Attr) ->
	    Attr = tentative{tentval:Xt, sums:Sums},
	    % nonvar(Coeff), var(Sum), Sum need not have TV yet
	    setarg(sums of tentative, Attr,
		    [summand{coeff:Coeff,old:Xt,sum:Sum}|Sums]),
	    SumNew is tent_get(Sum) + Coeff*Xt,
	    tent_set(Sum, SumNew)
	;
	    exit_block(tentative_value_not_set)
	).
extend_sum(X, _Coeff, _Sum) :-
	var(X),
	exit_block(tentative_value_not_set).
extend_sum(X, Coeff, Sum) :-
	number(X),
	SumNew is tent_get(Sum) + Coeff*X,
	tent_set(Sum, SumNew).



:- export register_for_notification/3.
:- comment(register_for_notification/3, [
    summary:"Constraint implementation: register for notification",
    amode:register_for_notification(?,+,?),
    args:["X":"A non-free term, usually a tentative variable",
	"Tag":"A user-defined term",
    	"Receiver":"A notification-receiver (input or output)"],
    see_also:[library(notify_ports)],
    desc:html("
	<P>
	This is part of the interface for implementing tentative constraints.
	It sets up a receiver in the sense of lib(notify_ports), such
	that every time the tentative value of X changes, a message
	is sent to this receiver. The message has the form
	</P><PRE>
		Tag:chg(Old,New)
	</PRE><P>
	where Tag is the parameter given here in the setup (e.g. the
	index of a variable within the constraint where it occurs),
	Old is the tentative value before the change, and New the
	tentative value after the change.
	</P><P>
	The same receiver can register for many variables. Messages
	from the different variables are distinguished by their Tag.
	</P><P>
	Such a receiver would typically be used by a demon that is woken
	up on tentative value change (tent_chg of tentative), to obtain
	precise information about the changes that caused the wakeup.
	</P>
    "),
    eg:"
    tent_trace_array(Stream, Name, Xs) :-
	    ( foreacharg(X,Xs,I), param(Stream,Name) do
		register_for_notification(X, I, Receiver),
		suspend(tent_trace_demon(Stream, Name, Receiver, Susp),
			1, X->tent_chg, Susp)
	    ).

	:- demon tent_trace_demon/4.
	tent_trace_demon(Stream, Name, Receiver, Susp) :-
	    foreachnotification(tent_trace_demon,
		    I:Notification, [Stream, Name], Receiver, Status, (
		printf(Stream, \"%w[%w]: %w%n\", [Name,I,Notification])
	    )),
	    ( Status = closed ->
		writeln(Stream, Name:Status),
		kill_suspension(Susp)
	    ; true ).
    "
]).

register_for_notification(_X{Attr}, Tag, Receiver) ?- !,
	( nonvar(Attr) ->
	    Attr = tentative{send_port:SendPort},
	    ( var(SendPort) -> open_tagging_sender(SendPort) ; true ),
	    open_tagged_receiver(Tag, SendPort, Receiver)
	;
	    exit_block(tentative_value_not_set)
	).
register_for_notification(X, _, _) :-
	var(X),
	exit_block(tentative_value_not_set).
register_for_notification(X, _, _) :-
	nonvar(X).



init_summand(Coeff, Val, Sum, summand{old:Val,coeff:Coeff,sum:Sum}) :-
	SumNew is tent_get(Sum) + Coeff*Val,
	tent_set(Sum, SumNew).

change_summand(Summand, New) :-
	Summand = summand{old:Old,coeff:Coeff,sum:Sum},
	( Old == New -> true ;
	    SumNew is tent_get(Sum) + Coeff*(New-Old),
	    setarg(old of summand, Summand, New),
	    tent_set(Sum, SumNew)
	).


%----------------------------------------------------------------------
% Randomised primitives
%----------------------------------------------------------------------

% pick a random value from range, list or array
:- export random_element/2.
:- comment(random_element/2, [
    summary:"Pick random element from range or collection",
    amode:(random_element(+,-) is semidet),
    fail_if:"The range specification is empty or invalid",
    args:["Values":"Specification of possible values",
	"X":"Output variable"],
    see_also:[random/1, frandom/1, random_sample/3, tent_set_random/2],
    desc:html("
	<P>
	Select a random value from the given specification of possible
	values.  The Values specification can be
	</P>
	<DL>
	<DT>Min..Max</DT>
	    <DD>A range of integers from Min to Max</DD>
	<DT>List</DT>
	    <DD>A list of possible values</DD>
	<DT>Array</DT>
	    <DD>An array of possible values</DD>
	</DL>
    "),
    eg:"
    ?- random_element(3 .. 7, X).
    X = 5
    Yes (0.00s cpu)

    ?- random_element([a, b, c, d], X).
    X = a
    Yes (0.00s cpu)

    ?- random_element([a, b, c, d], X).
    X = c
    Yes (0.00s cpu)

    "
]).

random_element(From..To, Pick) ?- !,
	From =< To,
	Pick is From + random mod (To-From+1).
random_element([X1|Xs], Pick) ?- !,
    	(
	    foreach(This,Xs),
	    fromto(X1,Old,New,Pick),
	    count(N,2,_)
	do
	    ( frandom < 1/N ->
		New = This
	    ;
		New = Old
	    )
	).
random_element(Xs, Pick) :-
	compound(Xs),
	functor(Xs, [], N),
	I is 1 + random mod N,
	arg(I, Xs, Pick).

/*
:- export random_element/3.
random_element(From..To, Pick, I) ?- !,
	From =< To,
	I0 is random mod (To-From+1),
	Pick is From + I0,
	I is I0+1.
random_element([X1|Xs], Pick, I) ?- !,
    	(
	    foreach(This,Xs),
	    fromto(X1,Old,New,Pick),
	    fromto(1,IOld,INew,I),
	    count(N,2,_)
	do
	    ( frandom < 1/N ->
		New = This,
		INew = N
	    ;
		New = Old,
		INew = IOld
	    )
	).
random_element(Xs, Pick, I) :-
	compound(Xs),
	functor(Xs, [], N),
	I is 1 + random mod N,
	arg(I, Xs, Pick).
*/

% Nondeterministically pick SampleSize random values from range, list or array
% TODO: avoid duplicates
:- export random_sample/3.
:- comment(random_sample/3, [
    summary:"Nondeterministically pick SampleSize random elements",
    amode:(random_sample(+,+,-) is nondet),
    args:["Values":"Specification of possible values",
	"SampleSize":"Integer",
	"X":"Output variable"],
    see_also:[random/1, frandom/1, random_element/2, tent_set_random/2],
    desc:html("
	This predicate succeeds SampleSize times. Each time it succeeds,
	it returns a random value from the given specification of possible
	values.  The Values specification are as in random_element/2.
    "),
    eg:"
    ?- random_sample(0 .. 9, 3, X).
    X = 9
    Yes (0.00s cpu, solution 1, maybe more)
    X = 0
    Yes (0.03s cpu, solution 2, maybe more)
    X = 5
    Yes (0.03s cpu, solution 3)
    "
]).

random_sample(Range, SampleSize, I) :-
	between(1, SampleSize, 1, _),
	random_element(Range, I).


%----------------------------------------------------------------------
% Monitored constraints
%----------------------------------------------------------------------

:- local store(tent_implements).

:- export struct(monitored_constraint(
	alias,				% Goal or alias term
	violations,			% TentVar of integer
	suspensions			% suspension list (for killing only)
    )).


mc_current_violation(monitored_constraint{violations:TV}, Viol) ?-
    	TV tent_get Viol.


mc_current_violation(monitored_constraint{violations:TV,alias:Term}, Viol, Cstr) ?-
	Cstr = Term,
    	TV tent_get Viol.


mc_violations(monitored_constraint{violations:TV}, V) ?-
    	TV tent_get V.


mc_register_suspension(MC, Susp) :- is_suspension(Susp), !,
	enter_suspension_list(suspensions of monitored_constraint, MC, Susp).


mc_create(Constraint, Term, ConsVio, MonitoredConstraint) :-
	MonitoredConstraint = monitored_constraint{violations:ConsVio,alias:Term},
	ConsVio tent_set 0,

	% map constaint to implementation, using table
	functor(Constraint, Name, Arity),
	( store_get(tent_implements, Name/Arity, ImplSpec) ->
	    ImplSpec = ImplMod:ImplName/ImplArity,
	    functor(Implementation, ImplName, ImplArity),
	    arg(ImplArity, Implementation, MonitoredConstraint),
	    ( for(I,1,Arity), param(Constraint,Implementation) do
		arg(I, Constraint, Arg),
		arg(I, Implementation, Arg)
	    ),
	    ImplMod:Implementation
	;
	    printf(error, "Constraint has no tentative implementation: %w%n",
		[Name/Arity]),
	    abort
	).

	% map to +1 arity predicate
%	Constraint =.. List,
%	append(List, [MonitoredConstraint], List1),
%	Implementation =.. List1,

%	call(Implementation).


mc_kill(monitored_constraint{violations:TV,suspensions:Ss}) ?-
	( foreach(S,Ss) do kill_suspension(S) ),
	TV tent_get TV.


:- export tent_implements/2.
:- export op(700,xfx,tent_implements).
:- comment(tent_implements/2, [
    summary:"Associate a constraint with a tentative value implementation",
    template:"++ImplSpec tent_implements ++ConsSpec",
    amode:(tent_implements(++,++) is det),
    args:["ImplSpec":"Term of the form Atom/Integer", "ConsSpec":"Term of the form Atom/Integer"],
    see_also:[(:~)/2],
    desc:html("
	This declaration is part of the constraint implementation interface.
	It links the name/arity of the constraint in the constraint model to
	the name/arity of the predicate that implements the tentative value
	semantics of the constraint.  For example, the declaration
	<PRE>
	    :- alldifferent_t/2 tent_implements alldifferent/1.
	</PRE>
	means that whenever an alldifferent/1 constraint is added to a
	constraint set, e.g. by using
	<PRE>
	    ..., CSet :~ alldifferent(List), ...
	</PRE>
	then the alldifferent_t(List, Monitor) is invoked to implement
	this constraint.
    ")
]).
:- tool(tent_implements/2, tent_implements_/3).
tent_implements_(ImplSpec, ConstrSpec, Module) :-
	( valid_spec(ImplSpec, NI), valid_spec(ConstrSpec, NC) ->
	    ( NI =:= NC+1 ->
		( store_get(tent_implements, ConstrSpec, OldImplMod:OldImplSpec),
		  Module:ImplSpec \== OldImplMod:OldImplSpec
		->
		    printf(warning_output, "Warning: declaration%n    :- %w%noverrides earlier declaration in module %w%n    :- %w%n",
			[ImplSpec tent_implements ConstrSpec, OldImplMod,
			 OldImplSpec tent_implements ConstrSpec]
		    )
		;
		    true
		),
		store_set(tent_implements, ConstrSpec, Module:ImplSpec)
	    ;
		error(6, ImplSpec tent_implements ConstrSpec, Module)
	    )
	;
	    error(5, ImplSpec tent_implements ConstrSpec, Module)
	).


valid_spec(F/N, N1) ?-
	atom(F),
	integer(N),
	N1=N.


%----------------------------------------------------------------------
% Constraint/Conflict Sets
%----------------------------------------------------------------------

:- local struct(constraint_set(
	violations,			% TentVar of integer
	monitored_constraints		% list of monitored_constraint{}
    )).

:- export cs_create/2.
:- comment(cs_create/2, [
    summary:"Create an empty constraint set",
    amode:(cs_create(-,++) is det),
    args:["CS":"Constraint set (output)", "Options":"List of options"],
    see_also:[(:~)/2,cs_clear_all/1,cs_clear_satisfied/1,
    	cs_violations/2, cs_current_violations/2,
	cs_random_worst/2, cs_all_worst/2, cs_all_violated/2,
	cs_random_violated/2, cs_all/2],
    desc:html("
	Create an empty constraint set. No options are currently supported.
	A constraint set is an abstract data structure which should only
	be accessed through the cs_xxx group of predicates. Its purpose
	is to group constraints together and organise access to these
	constraints based on their violatedness counts.
    "),
    eg:"
    ?- cs_create(CS, []).
    CS = constraint_set(Violations{0 -> 0}, [])
    Yes (0.00s cpu)
    "
]).

cs_create(CS, _Options) :-
	CS = constraint_set{violations:Violations, monitored_constraints:[]},
	Violations tent_set 0.


:- export op(800, xfx, alias).
:- export op(900, xfx, :~).
:- export (:~)/2.
:- comment((:~)/2, [
    summary:"Add a constraint to a constraint set",
    template:"CS :~ ConstrSpec",
    amode:(:~(?,+) is det),
    args:["CS":"Constraint set or free variable",
    	"ConstrSpec":"A constraint specification"],
    see_also:[cs_create/2,cs_clear_all/1,cs_clear_satisfied/1,
    	cs_violations/2, cs_current_violations/2,
	cs_random_worst/2, cs_all_worst/2, cs_all_violated/2,
	cs_random_violated/2, cs_all/2, tent_implements/2],
    desc:html("<P>
	Add a constraint to a constraint set. After adding, the constraint
	will be part of the constraint set, and add to its total violatedness.
	The amount of violatedness contributed by the constraint depends on
	the constraint's implementation, but can be scaled.
	</P><P>
	Three forms of constraint specification are recognised here:
	<DL>
	<DT>Constraint</DT>
	    <DD>A constraint goal. The constraint must have a 'tentative
	    implementation', i.e. a predicate must be defined that takes one
	    additional argument and implements the violation monitoring for
	    the constraint (see constraint implementation interface).</DD>
	<DT>Weight * Constraint</DT>
	    <DD>where Weight is an integer. This means that the constraints
	    violatedness is scaled with the factor Weight before being added
	    to the total violatedness of the constraint set.</DD>
	<DT>Constraint alias Term</DT>
	    <DD>Term is the term that can be retrieved from the constraint
	    set, in place of the constraint goal itself (see cs_all/2 etc).</DD>
	<DT>Weight * Constraint alias Term</DT>
	    <DD>Conbination of both modifiers above.</DD>
	</DL>
	<P>
	If CS is a free variable, a new constraint set will be implicitly
	created (as if created with cs_create(CS, [])).
	</P>
    "),
    eg:"
    ?- length(Xs, 5), tent_set_all(Xs, 99), CS :~ alldifferent(Xs).
    Xs = [Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}]
    CS = constraint_set(TotalVio{10 -> 0}, ...)
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

%CS :~ (C1,C2) ?- !,
%	CS :~ C1,
%	CS :~ C2.
CS :~ Weight * Constraint alias Term ?- !,
	cs_add(CS, Constraint, Weight, Term).
CS :~ Weight * Constraint ?- !,
	cs_add(CS, Constraint, Weight, Constraint).
CS :~ Constraint alias Term ?- !,
	cs_add(CS, Constraint, 1, Term).
CS :~ Constraint :-
	cs_add(CS, Constraint, 1, Constraint).

    cs_add(CS, Constraint, Weight, Term) :-
	integer(Weight),
	cs_create_if_needed(CS),
	CS = constraint_set{violations:TotalVio, monitored_constraints:MCs},
	mc_create(Constraint, Term, ConsVio, MonitoredConstraint),
	setarg(monitored_constraints of constraint_set, CS, [MonitoredConstraint|MCs]),
	extend_sum(ConsVio, Weight, TotalVio).

    cs_create_if_needed(CS) :- var(CS), !,
	cs_create(CS, []).
    cs_create_if_needed(constraint_set{}).


:- export cs_clear_all/1.
:- comment(cs_clear_all/1, [
    summary:"Clean up the constraint set completely",
    amode:(cs_clear_all(+) is det),
    args:["CS":"Constraint set"],
    see_also:[cs_clear_satisfied/1],
    desc:html("
	<P>
	Clean up the constraint set completely. All delayed goals related
	to this constraint set will be removed and the constraints forgotten.
	A constraint set that has been cleaned up should not be used any longer.
	</P>
    ")
]).

cs_clear_all(CS) :-
	CS = constraint_set{monitored_constraints:MCs},
	(
	    foreach(MC,MCs)
	do
	    mc_kill(MC)
	),
	setarg(monitored_constraints of constraint_set, CS, []).


:- export cs_clear_satisfied/1.
:- comment(cs_clear_satisfied/1, [
    summary:"Remove the satisfied constraints from the constraint set",
    amode:(cs_clear_satisfied(+) is det),
    args:["CS":"Constraint set"],
    see_also:[cs_clear_all/1],
    desc:html("
	<P>
	Remove all currently satisfied constraints from the constraint set.
	This means, even if they become violated again later, they will no
	longer contribute to the set's violatedness count.  All delayed goals
	related to these constraints will be removed and the constraints
	forgotten.
	</P>
    ")
]).

cs_clear_satisfied(CS) :-
	CS = constraint_set{monitored_constraints:MCs},
	(
	    foreach(MC,MCs),
	    fromto(NewMCs,NewMCs1,NewMCs0,[])
	do
	    MC = monitored_constraint{violations:V},
	    ( tent_get(V) > 0 ->
		NewMCs1 = [MC|NewMCs0]
	    ;
		mc_kill(MC),
		NewMCs1 = NewMCs0
	    )
	),
	setarg(monitored_constraints of constraint_set, CS, NewMCs).


:- export cs_violations/2.
:- comment(cs_violations/2, [
    summary:"Get a tentative variable reflecting the violatedness of the constraint set",
    amode:(cs_violations(+,-) is det),
    args:["CS":"Constraint set", "Vio":"A tentative variable (output)"],
    see_also:[cs_current_violations/2],
    desc:html("
	<P>
	Returns a tentative variable reflecting the (current and future)
	violatedness of the constraint set.
	</P>
    "),
    eg:"
    ?- length(Xs, 5), tent_set_all(Xs, 99), CS :~ alldifferent(Xs),
       cs_violations(CS, V).
    Xs = [Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}]
    CS = constraint_set(V{10 -> 0}, ...)
    V = V{10 -> 0}
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

cs_violations(constraint_set{violations:Violations}, V) ?-
	V = Violations.


:- export cs_current_violations/2.
:- comment(cs_current_violations/2, [
    summary:"Get the current violatedness of the constraint set",
    amode:(cs_current_violations(+,-) is det),
    args:["CS":"Constraint set", "Vio":"An integer (output)"],
    see_also:[cs_violations/2],
    desc:html("
	<P>
	Returns an integer representing the current violatedness of
	the constraint set.
	</P>
    "),
    eg:"
    ?- length(Xs, 5), tent_set_all(Xs, 99), CS :~ alldifferent(Xs),
       cs_current_violations(CS, V).
    Xs = [Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}, Xi{99 -> 4}]
    CS = constraint_set(TotalVio{10 -> 0}, ...)
    V = 10
    There is 1 delayed goal.
    Yes (0.00s cpu)
    "
]).

cs_current_violations(constraint_set{violations:Violations}, V) ?-
	 Violations tent_get V.


:- export cs_all/2.
:- comment(cs_all/2, [
    summary:"Get all constraints in the constraint set",
    amode:(cs_all(+,-) is det),
    args:["CS":"Constraint set", "Cstr":"A list of constraints (output)"],
    see_also:[cs_all_violated/2, cs_all_worst/2],
    desc:html("
	<P>
	Returns a list of all constraints in the constraint set.
	</P><P>
	If the constraints were added with aliases (see :~ /2),
	the alias term is retrieved instead of the constraint goal.
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], CS :~ (X $= Y), CS :~ (X $\\= Y), cs_all(CS, C).
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    C = [X{3 -> 0} $\\= Y{4 -> 0}, X $= Y]
    There are 2 delayed goals.
    Yes (0.00s cpu)
    "
]).

cs_all(constraint_set{monitored_constraints:MCs}, All) ?-
	(
	    foreach(monitored_constraint{alias:Cstr},MCs),
	    foreach(Cstr,All)
	do
	    true
	).


:- export cs_all_violated/2.
:- comment(cs_all_violated/2, [
    summary:"Get all violated constraints in the constraint set",
    amode:(cs_all_violated(+,-) is det),
    args:["CS":"Constraint set", "Cstr":"A list of constraints (output)"],
    see_also:[cs_all/2, cs_all_worst/2],
    desc:html("
	<P>
	Returns a list of all currently violated constraints in the
	constraint set, i.e. all the constraints whose violation count
	is currently nonzero.
	</P><P>
	If the constraints were added with aliases (see :~ /2),
	the alias term is retrieved instead of the constraint goal.
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], CS :~ (X $= Y), CS :~ (X $\\= Y), cs_all_violated(CS, C).
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    C = [X{3 -> 0} $= Y{4 -> 0}]
    There are 2 delayed goals.
    Yes (0.00s cpu)
    "
]).

cs_all_violated(constraint_set{monitored_constraints:MCs}, AllVio) ?-
	(
	    foreach(monitored_constraint{alias:Cstr,violations:Vio},MCs),
	    fromto(AllVio,New,Old,[])
	do
	    Vio tent_get VThis,
	    ( VThis > 0 ->
	    	New = [Cstr|Old]
	    ;
	    	New = Old
	    )
	).


:- export cs_random_violated/2.
:- comment(cs_random_violated/2, [
    summary:"Get random violated constraints in the constraint set",
    amode:(cs_random_violated(+,-) is semidet),
    fail_if:"Fails if there is no violated constraint in the set",
    args:["CS":"Constraint set", "Cstr":"A constraint term (output)"],
    see_also:[cs_random_worst/2, cs_all_violated/2],
    desc:html("
	<P>
	Returns a currently violated constraint from the constraint set,
	i.e. all the constraints whose violation count is currently nonzero.
	If there are several, a random one is returned.
	</P><P>
	If the constraints were added with aliases (see :~ /2),
	the alias term is retrieved instead of the constraint goal.
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], CS :~ (X $= Y), CS :~ (X $\\= Y),
       cs_random_violated(CS, C).
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    C = X{3 -> 0} $= Y{4 -> 0}
    There are 2 delayed goals.
    Yes (0.00s cpu)
    "
]).

cs_random_violated(constraint_set{monitored_constraints:MCs}, Violated) ?-
	(
	    foreach(MC,MCs),
	    fromto(0,NVio1,NVio2,_),		% count the candidates
	    fromto(none,Old,New,ViolatedMC)
	do
	    MC = monitored_constraint{violations:Vio},
	    Vio tent_get VThis,
	    ( VThis > 0 ->
		NVio2 is NVio1+1,
	    	( frandom < 1/NVio2 ->
		    New = MC
		;
		    New = Old
		)
	    ;
		NVio2 = NVio1,
	    	New = Old
	    )
	),
	ViolatedMC = monitored_constraint{alias:Violated}.	% fail if none


:- export cs_all_worst/2.
:- comment(cs_all_worst/2, [
    summary:"Get all worst violated constraints in the constraint set",
    amode:(cs_all_worst(+,-) is det),
    args:["CS":"Constraint set", "Cstr":"A list of constraints (output)"],
    see_also:[cs_all/2, cs_all_violated/2],
    desc:html("
	<P>
	Returns a list of all the worst violated constraints in the
	constraint set, i.e. all the constraints that have maximum
	violation count among the constraints in the set.
	</P><P>
	If the constraints were added with aliases (see :~ /2),
	the alias term is retrieved instead of the constraint goal.
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], CS :~ (X $= Y), CS :~ (X $\\= Y), cs_all_worst(CS, C).
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    C = [X{3 -> 0} $= Y{4 -> 0}]
    There are 2 delayed goals.
    Yes (0.00s cpu)
    "
]).

cs_all_worst(constraint_set{monitored_constraints:MCs}, AllWorst) ?-
	(
	    foreach(MCThis,MCs),
	    fromto(0,VOld,VNew,_VWorst),
	    fromto([],Old,New,AllWorst)
	do
	    mc_current_violation(MCThis, VThis, This),
	    ( VThis > VOld ->
	    	VNew = VThis, New = [This]
	    ; VThis < VOld ->
	    	VNew = VOld, New = Old
	    ; VOld > 0 ->
	    	VNew = VOld, New = [This|Old]
	    ;
	    	VNew = VOld, New = Old
	    )
	).


:- export cs_random_worst/2.
:- comment(cs_random_worst/2, [
    summary:"Get random worst violated constraint from the constraint set",
    amode:(cs_random_worst(+,-) is semidet),
    fail_if:"Fails if there is no violated constraint in the set",
    args:["CS":"Constraint set", "Cstr":"A constraint term (output)"],
    see_also:[cs_random_violated/2, cs_all_worst/2],
    desc:html("
	<P>
	Returns a worst violated constraint from the constraint set, i.e.
	a constraint that has maximum violation count among the constraints
	in the set. If there are several, a random one is picked.
	</P><P>
	If the constraints were added with aliases (see :~ /2),
	the alias term is retrieved instead of the constraint goal.
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], CS :~ (X $= Y), CS :~ (X $\\= Y), cs_random_worst(CS, C).
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    CS = constraint_set(TotalVio{1 -> 0}, ...)
    C = X{3 -> 0} $= Y{4 -> 0}
    There are 2 delayed goals.
    Yes (0.00s cpu)
    "
]).

cs_random_worst(constraint_set{monitored_constraints:MCs}, Worst) ?-
	(
	    foreach(MCThis,MCs),
	    fromto(0,NMax1,NMax2,_),	% count repetitions of maximum
	    fromto(0,VOld,VNew,VWorst),
	    fromto(none,Old,New,Pick)
	do
	    mc_current_violation(MCThis, VThis, This),
	    ( VThis > VOld ->
	    	VNew = VThis, New = This, NMax2 = 1
	    ; VThis < VOld ->
	    	VNew = VOld, New = Old, NMax2 = NMax1
	    ;
		NMax2 is NMax1+1,
	    	( frandom < 1/NMax2 ->
		    VNew = VThis, New = This
		;
		    VNew = VOld, New = Old
		)
	    )
	),
	VWorst > 0,			% fail if no violated constraints
	Worst = Pick.



%----------------------------------------------------------------------
% Variable sets
% currently represented simply an array of variable attributes
% TODO: optionally maintain a heap so that worst violated ones can
% be accessed more efficiently
%----------------------------------------------------------------------

:- export vs_create/2.
:- comment(vs_create/2, [
    summary:"Construct a varset from the variables in Vars",
    amode:(vs_create(?,-) is semidet),
    fail_if:"Vars contains variables without tentative values",
    args:["Vars":"A term containing tentative variables",
    	"VS":"Varset (output)"],
    see_also:[tent_set/2, vs_size/2, vs_element/3,
	vs_all/2, vs_all_violated/2, vs_all_worst/2,
	vs_all_violated_index/2, vs_all_worst_index/2,
	vs_random/2, vs_random_violated/2, vs_random_worst/2,
	vs_random_index/2, vs_random_violated_index/2, vs_random_worst_index/2,
	vs_member/2, vs_violated/2, vs_worst/2,
	vs_violated_index/2, vs_worst_index/2],
    desc:html("
	Create an abstract 'varset' from the tentative variables in a term.
	A varset is an ordered set of variables that can be accessed by
	index, or by their violation properties. Elements are indexed from
	1 to size of the set.
    "),
    eg:"
    ?- Vars = [_,_,_], tent_set_all(Vars, 99), vs_create(Vars, VS).
    Vars = [Xi{99 -> 0}, Xi{99 -> 0}, Xi{99 -> 0}]
    VS = ...
    Yes (0.00s cpu)
    "
]).

vs_create(Term, VS) :-
	vs_create(Term, VS, []).


%:- export vs_create/3.
vs_create(Term, VS, _Options) :-
	term_variables(Term, Xs),
	length(Xs, N),
	functor(VS, [], N),
	( foreach(X,Xs), foreacharg(Attr,VS) do
	    get_attribute(X, Attr)
	).


:- export vs_size/2.
:- comment(vs_size/2, [
    summary:"Get the size of a varset",
    amode:(vs_size(+,-) is det),
    args:["VS":"A varset",
    	"N":"An integer (output)"],
    see_also:[vs_create/2, vs_element/3],
    desc:html("
	Get the size (number of elements) of a varset.
    "),
    eg:"
    ?- Vars=[A,B,C], vs_create(Vars, VS), vs_size(VS, N).
    VS = ...
    N = 3
    Yes (0.00s cpu)
    "
]).
vs_size(VS, N) :-
	functor(VS, [], N).


:- export vs_element/3.
:- comment(vs_element/3, [
    summary:"Get an element of a varset by index",
    amode:(vs_element(+,+,-) is det),
    args:["VS":"A varset",
    	"I":"A positive integer",
	"X":"A variable (output)"],
    see_also:[vs_create/2, vs_size/2],
    desc:html("
	Get the Ith element of a varset. I must be between 1 and the
	size of the set.
    "),
    eg:"
    ?- Vars=[A,B,C], vs_create(Vars, VS), vs_element(VS, 1, X).
    VS = ...
    X = C
    Yes (0.00s cpu)
    "
]).

vs_element(VS, I, X) :-
	arg(I, VS, Attr),
	Attr = tentative{value:X}.


:- export vs_member/2.
:- comment(vs_member/2, [
    summary:"Succeed for each element of a varset",
    amode:(vs_member(+,-) is nondet),
    args:["VS":"A varset",
	"X":"A variable (output)"],
    see_also:[vs_create/2, vs_element/3, vs_all/2, vs_random/2],
    desc:html("
	Backtrack over all elements of a varset.
    ")
]).

vs_member(VS, X) :-
	vs_size(VS, N),
	between(1, N, 1, I),
	arg(I, VS, Attr),
	Attr = tentative{value:X}.


:- export vs_all/2.
:- comment(vs_all/2, [
    summary:"Retrieve all variables from a varset",
    amode:(vs_all(+,-) is det),
    args:["VS":"A varset",
    	"Vars":"A list of tentative variables (output)"],
    see_also:[vs_create/2, vs_random/2, vs_member/2],
    desc:html("
	Retrieve all variables from a varset.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [1,2,3]),
       vs_create(Vars, VS), vs_all(VS, VSVars).
    Vars = [A{1 -> 0}, B{2 -> 0}, C{3 -> 0}]
    VS = ...
    VSVars = [C{3 -> 0}, B{2 -> 0}, A{1 -> 0}]
    Yes (0.00s cpu)
    "
]).

vs_all(VS, Vs) :-
	(
	    foreacharg(tentative{value:V},VS),
	    foreach(V,Vs)
	do
	    true
	).


:- export vs_all_violated/2.
:- comment(vs_all_violated/2, [
    summary:"Retrieve all violated variables from a varset",
    amode:(vs_all_violated(+,-) is det),
    args:["VS":"A varset",
    	"Vars":"A list of tentative variables (output)"],
    see_also:[vs_random_violated/2, vs_violated/2, vs_all_violated_index/2],
    desc:html("
	Retrieve all variables from a varset which have nonzero violation
	counts.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [1,2,3]), vs_create(Vars, VS),
       var_inc_violations(B, 1), vs_all_violated(VS, VSVars).
    Vars = [A{1 -> 0}, B{2 -> 1}, C{3 -> 0}]
    VS = ...
    VSVars = [B{2 -> 1}]
    Yes (0.00s cpu)
    "
]).

vs_all_violated(VS, AllVio) :-
	(
	    foreacharg(This,VS),
	    fromto(AllVio,New,Old,[])
	do
	    This = tentative{value:V,violations:VThis},
	    ( VThis > 0 ->
	    	New = [V|Old]
	    ;
	    	New = Old
	    )
	).


:- export vs_all_violated_index/2.
:- comment(vs_all_violated_index/2, [
    summary:"Retrieve all violated variable indices from a varset",
    amode:(vs_all_violated_index(+,-) is det),
    args:["VS":"A varset",
    	"Vars":"A list of integers (output)"],
    see_also:[vs_random_violated_index/2, vs_violated_index/2, vs_all_violated/2],
    desc:html("
	Retrieve all variable indices from a varset which have nonzero
	violation counts.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [1,2,3]), vs_create(Vars, VS),
       var_inc_violations(B, 1), vs_all_violated(VS, VSIs).
    Vars = [A{1 -> 0}, B{2 -> 1}, C{3 -> 0}]
    VS = ...
    VSVars = [2]
    Yes (0.00s cpu)
    "
]).
vs_all_violated_index(VS, AllVio) :-
	(
	    foreacharg(This,VS,I),
	    fromto(AllVio,New,Old,[])
	do
	    This = tentative{violations:VThis},
	    ( VThis > 0 ->
	    	New = [I|Old]
	    ;
	    	New = Old
	    )
	).


:- export vs_violated/2.
:- comment(vs_violated/2, [
    summary:"Succeeds for each violated variable from a varset",
    amode:(vs_violated(+,-) is nondet),
    args:["VS":"A varset",
    	"Vars":"A tentative variable (output)"],
    see_also:[vs_all_violated/2, vs_random_violated/2, vs_violated_index/2],
    desc:html("
	Backtrack over all variables from a varset which have nonzero
	violation counts.
    ")
]).
vs_violated(VS, Vio) :-
	vs_violated_index(VS, I),
	vs_element(VS, I, Vio).

:- export vs_violated_index/2.
:- comment(vs_violated_index/2, [
    summary:"Succeeds for each violated variable index from a varset",
    amode:(vs_violated_index(+,-) is nondet),
    args:["VS":"A varset",
    	"Vars":"An integer (output)"],
    see_also:[vs_all_violated_index/2, vs_random_violated_index/2, vs_violated/2],
    desc:html("
	Backtrack over all variable indices from a varset which have
	nonzero violation counts.
    ")
]).
vs_violated_index(VS, IVio) :-
	vs_size(VS, N),
	between(1, N, 1, I),
	arg(I, VS, Attr),
	Attr = tentative{violations:VThis},
	VThis > 0,
	IVio = I.


:- export vs_all_worst/2.
:- comment(vs_all_worst/2, [
    summary:"Retrieve all worst violated variables from a varset",
    amode:(vs_all_worst(+,-) is det),
    args:["VS":"A varset",
    	"Vars":"A list of tentative variables (output)"],
    see_also:[vs_random_worst/2, vs_worst/2, vs_all_worst_index/2],
    desc:html("
	Retrieve all variables from a varset whose violation count is
	maximal in the varset.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [1,2,3]), vs_create(Vars, VS),
       var_inc_violations(A, 2),
       var_inc_violations(B, 1),
       var_inc_violations(C, 2),
       vs_all_worst(VS, VSVars).
    Vars = [A{1 -> 2}, B{2 -> 1}, C{3 -> 2}]
    VS = ...
    VSVars = [A{1 -> 2}, C{3 -> 2}]
    Yes (0.00s cpu)
    "
]).

vs_all_worst(VS, AllWorst) :-
	(
	    foreacharg(This,VS),
	    fromto(0,VOld,VNew,_VWorst),
	    fromto([],Old,New,AllWorst)
	do
	    This = tentative{value:V, violations:VThis},
	    ( VThis > VOld ->
	    	VNew = VThis, New = [V]
	    ; VThis < VOld ->
	    	VNew = VOld, New = Old
	    ; VOld > 0 ->
	    	VNew = VOld, New = [V|Old]
	    ;
	    	VNew = VOld, New = Old
	    )
	).


:- export vs_all_worst_index/2.
:- comment(vs_all_worst_index/2, [
    summary:"Retrieve all worst violated variable indices from a varset",
    amode:(vs_all_worst_index(+,-) is det),
    args:["VS":"A varset",
    	"Vars":"A list of integers (output)"],
    see_also:[vs_random_worst_index/2, vs_worst_index/2, vs_all_worst/2],
    desc:html("
	Retrieve all variable indices from a varset whose violation count is
	maximal in the varset.
    ")
]).
vs_all_worst_index(VS, AllWorstI) :-
	(
	    foreacharg(This,VS,I),
	    fromto(0,VOld,VNew,_VWorst),
	    fromto([],Old,New,AllWorstI)
	do
	    This = tentative{violations:VThis},
	    ( VThis > VOld ->
	    	VNew = VThis, New = [I]
	    ; VThis < VOld ->
	    	VNew = VOld, New = Old
	    ; VOld > 0 ->
	    	VNew = VOld, New = [I|Old]
	    ;
	    	VNew = VOld, New = Old
	    )
	).


:- export vs_worst/2.
:- comment(vs_worst/2, [
    summary:"Succeeds for each worst violated variable from a varset",
    amode:(vs_worst(+,-) is nondet),
    args:["VS":"A varset",
    	"Vars":"A tentative variable (output)"],
    see_also:[vs_all_worst/2, vs_random_worst/2, vs_worst_index/2],
    desc:html("
	Backtrack over all variable indices from a varset whose violation
	count is maximal in the varset.
    ")
]).
vs_worst(VS, Worst) :-
	vs_worst_index(VS, IWorst),
	vs_element(VS, IWorst, Worst).

	
:-export vs_worst_index/2.
:- comment(vs_worst_index/2, [
    summary:"Succeeds for each worst violated variable index from a varset",
    amode:(vs_worst_index(+,-) is nondet),
    args:["VS":"A varset",
    	"Vars":"An integer (output)"],
    see_also:[vs_all_worst_index/2, vs_random_worst_index/2, vs_worst/2],
    desc:html("
	Backtrack over all variable indices from a varset whose violation
	count is maximal in the varset.
    ")
]).
vs_worst_index(VS, IWorst) :-
	vs_all_worst_index(VS, Is),
	member(IWorst, Is).


:- export vs_random_worst/2.
:- comment(vs_random_worst/2, [
    summary:"Retrieve a worst violated variable from a varset",
    amode:(vs_random_worst(+,-) is semidet),
    fail_if:"Fails if there is no violated variable in the set",
    args:["VS":"A varset",
    	"Var":"Tentative variable (output)"],
    see_also:[vs_all_worst/2, vs_worst/2, vs_random_worst_index/2],
    desc:html("
	Retrieve a variable from a varset whose violation count is
	maximal in the varset. In case of ties, a random candidate
	is returned.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [1,2,3]), vs_create(Vars, VS),
       var_inc_violations(A, 2),
       var_inc_violations(B, 1),
       var_inc_violations(C, 2),
       vs_random_worst(VS, Worst).
    Vars = [A{1 -> 2}, B{2 -> 1}, Worst{3 -> 2}]
    VS = ...
    Worst = Worst{3 -> 2}       % A or C, the result is random!
    Yes (0.00s cpu)
    "
]).

vs_random_worst(VS, Worst) :-
	vs_random_worst_index(VS, IWorst),
	vs_element(VS, IWorst, Worst).


:- export vs_random_worst_index/2.
:- comment(vs_random_worst_index/2, [
    summary:"Retrieve a worst violated variable index from a varset",
    amode:(vs_random_worst_index(+,-) is semidet),
    fail_if:"Fails if there is no violated variable in the set",
    args:["VS":"A varset",
    	"Var":"An integer (output)"],
    see_also:[vs_all_worst_index/2, vs_worst_index/2, vs_random_worst/2],
    desc:html("
	Retrieve a variable index from a varset whose violation count is
	maximal in the varset. In case of ties, a random candidate
	is returned.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [1,2,3]), vs_create(Vars, VS),
       var_inc_violations(A, 2),
       var_inc_violations(B, 1),
       var_inc_violations(C, 2),
       vs_random_worst(VS, Worst).
    Vars = [A{1 -> 2}, B{2 -> 1}, Worst{3 -> 2}]
    VS = ...
    Worst = 1       % 1 or 3, the result is random!
    Yes (0.00s cpu)
    "
]).
vs_random_worst_index(VS, IWorst) :-
	(
	    foreacharg(This,VS,I),
	    fromto(0,IOld,INew,IWorst),		% track the maximum index
	    fromto(0,VOld,VNew,VWorst),		% track maximum violatedness
	    fromto(0,NMax1,NMax2,_)		% count repetitions of maximum
	do
	    This = tentative{violations:VThis},
	    ( VThis > VOld ->
	    	INew = I, VNew = VThis, NMax2 = 1
	    ; VThis < VOld ->
	    	INew = IOld, VNew = VOld, NMax2 = NMax1
	    ;
		NMax2 is NMax1+1,
	    	( frandom < 1/NMax2 ->
		    INew = I, VNew = VThis
		;
		    INew = IOld, VNew = VOld
		)
	    )
	),
	VWorst > 0.				% fail if no violations



:- export vs_random_violated/2.
:- comment(vs_random_violated/2, [
    summary:"Retrieve a random violated variable from a varset",
    amode:(vs_random_violated(+,-) is semidet),
    args:["VS":"A varset",
    	"Var":"Tentative variable (output)"],
    see_also:[vs_all_violated/2, vs_violated/2, vs_random_violated_index/2],
    fail_if:"The varset does not contain a violated variable",
    desc:html("
	Retrieve a variable from a varset whose violation count is
	nonzero. If there are more than one, a random candidate is returned.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [a,b,c]), vs_create(Vars, VS),
       var_inc_violations(A, 2),
       var_inc_violations(C, 1),
       vs_random_violated(VS, Var).
    Vars = [Var{a -> 2}, B{b -> 0}, C{c -> 1}]
    VS = ...
    Var = Var{a -> 2}	% A or C, the result is random!
    Yes (0.00s cpu)
    "
]).

vs_random_violated(VS, Var) :-
	vs_random_violated_index(VS, IVio),
	vs_element(VS, IVio, Var).


:- export vs_random_violated_index/2.
:- comment(vs_random_violated_index/2, [
    summary:"Retrieve a random violated variable index from a varset",
    amode:(vs_random_violated_index(+,-) is semidet),
    args:["VS":"A varset",
    	"I":"An integer variable (output)"],
    see_also:[vs_all_violated_index/2, vs_violated_index/2, vs_random_violated/2],
    fail_if:"The varset does not contain a violated variable",
    desc:html("
	Retrieve a variable index from a varset whose violation count is
	nonzero. If there are more than one, a random candidate is returned.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [a,b,c]), vs_create(Vars, VS),
       var_inc_violations(A, 2),
       var_inc_violations(C, 1),
       vs_random_violated(VS, I).
    Vars = [A{a -> 2}, B{b -> 0}, C{c -> 1}]
    VS = ...
    I = 1		% 1 or 3, the result is random!
    Yes (0.00s cpu)
    "
]).
vs_random_violated_index(VS, IVio) :-
	(
	    foreacharg(This,VS,I),
	    fromto(0,NVio1,NVio2,_),	% count the candidates
	    fromto(0,Old,New,Pick)
	do
	    This = tentative{violations:VThis},
	    ( VThis > 0 ->
		NVio2 is NVio1+1,
	    	( frandom < 1/NVio2 ->
		    New = I
		;
		    New = Old
		)
	    ;
	    	New = Old, NVio2 = NVio1
	    )
	),
	Pick > 0,			% fail if no violations
	IVio = Pick.


:- export vs_random/2.
:- comment(vs_random/2, [
    summary:"Retrieve a random variable from a varset",
    amode:(vs_random(+,-) is semidet),
    args:["VS":"A varset",
    	"Var":"Tentative variable (output)"],
    see_also:[vs_create/2, vs_element/3, vs_random_index/2],
    desc:html("
	Retrieve a random variable from a varset.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [a,b,c]), vs_create(Vars, VS),
       vs_random(VS, Var).
    Vars = [Var{a -> 0}, B{b -> 0}, C{c -> 0}]
    VS = ...
    Var = Var{a -> 0}	% A, B or C, the result is random!
    Yes (0.00s cpu)
    "
]).

vs_random(VS, Var) :-
	vs_random_index(VS, I),
	vs_element(VS, I, Var).

:- export vs_random_index/2.
:- comment(vs_random_index/2, [
    summary:"Retrieve a random variable index from a varset",
    amode:(vs_random_index(+,-) is semidet),
    args:["VS":"A varset",
    	"Var":"An integer (output)"],
    see_also:[vs_create/2, vs_element/3, vs_random/2],
    desc:html("
	Retrieve a random variable index from a varset.
    "),
    eg:"
    ?- Vars=[A,B,C], tent_set(Vars, [a,b,c]), vs_create(Vars, VS),
       vs_random(VS, I).
    Vars = [A{a -> 0}, B{b -> 0}, C{c -> 0}]
    VS = ...
    I = 1		% 1, 2 or 3, the result is random!
    Yes (0.00s cpu)
    "
]).
vs_random_index(VS, I) :-
	compound(VS),
	functor(VS, [], N),
	I is 1 + random mod N.


%--------------------------------
% Find a move that minimizes violations
%--------------------------------

:- export tent_minimize_random/3.
:- tool(tent_minimize_random/3,tent_minimize_random_/4).
:- comment(tent_minimize_random/3, [
    summary:"Find a move that minimizes violations",
    amode:(tent_minimize_random(+,?,-)),
    fail_if:"Fails if MoveGenerator fails",
    args:[
	"MoveGenerator":"A goal",
	"Violations":"A tentative variable",
	"MoveId":"Placeholder for move identifier"],
    see_also:[tent_set/2],
    desc:html("
	This metapredicate finds a local search move that minimizes
	violations. It requries that the tentative variables and constraints
	over them have been set up beforehand. 
	<P>
    	MoveGenerator is a nondeterministic goal that implements a local
	search step. It should explore all neighbours on backtracking.
	A move should be made by changing tentative values, and
	instantiating MoveId to a unique identifier for every move.
	<P>
	Violations should be a tentative variable that reflects the
	total problem violations that are to be minimized.
	<P>
	MoveId is a variable which occurs in MoveGenerator. At the end of
	minimization, it will contain the ID of the best move. If there
	are multiple moves of the same quality, a random one is returned.
	<P>
	After tent_minimize_random/3 succeeds, all the trial moves are undone
	and the computation state is as before the call. Only the MoveId
	contains the identifier of the best move. This move can then be
	committed to by performing it again according to MoveId.
    "),
    eg:"

    % This example tries 8 moves that lead to different tentative values
    % of Viol. One of the moves that lead to a value of 1 is selected.

    ?-  Viol tent_set 0,
	tent_minimize_random((
		between(1, 8, 1, MoveId),
		arg(MoveId, viol(9,5,6,1,3,6,1,9), N),
		Viol tent_set N
	    ), Viol, MoveId).
     ...
     MoveId = 7		% 4 or 7, the result is random!
     Yes (0.00s cpu)
    "
]).

tent_minimize_random_(MoveGenerator, Violations, MoveId, Module) :-
	shelf_create(best(1.0Inf,0,none), Shelf),
	(
	    call(MoveGenerator)@Module,
	    shelf_get(Shelf, 1, Min1),
	    Violations tent_get Min2,
	    ( Min2 < Min1 ->			% better
		shelf_set(Shelf, 0, best(Min2,1,MoveId)),
		fail
	    ; Min2 > Min1 ->			% worse
		fail
	    ;					% tie
		shelf_get(Shelf, 2, NMin),
		NMin1 is NMin+1,
		( frandom < 1/NMin1 ->
		    shelf_set(Shelf, 0, best(Min2,NMin1,MoveId)),
		    fail
		;
		    shelf_set(Shelf, 2, NMin1),
		    fail
		)
	    )
	;
	    shelf_get(Shelf, 2, Count),
	    Count > 0,				% fail if no solution
	    shelf_get(Shelf, 3, MoveId)		% get random best
	).



%--------------------------------
% Changeable value interface for Visualisation
%--------------------------------

:- export suspend_on_change/2.
suspend_on_change(X, Susp) :-
	insert_suspension(X, Susp, tent_chg of tentative).

:- export get_changeable_value/2.
get_changeable_value(X, V) :-
	tent_get(X, V).


%--------------------------------
% Invariants
%--------------------------------

:- export tent_is/2.
:- export op(700,xfx,tent_is).
:- comment(tent_is/2, [
    summary:"Maintain tentative result of arithmetic expression",
    template:"?Result tent_is +Expr",
    amode:(tent_is(+,?) is det),
    args:["Result":"A (free or tentative) variable",
    	"Expr":"Arithmetic expression containing tentative variables"],
    see_also:[tent_set/2, tent_get/2],
    desc:html("
	<P>
	Implementation of arithemtic invariants.
	</P><P>
	This is similar to the normal arithmetic is/2 predicate, but
	evaluates the expression based on the tentative values of its
	variables.  The result is delivered as (an update to) the
	tentative value of the Result variable.  Once initiated,
	tent_is will stay active and keep updating Result's tentative
	value eagerly whenever the tentative value of any variable in
	Expression changes.
	</P>
    "),
    eg:"
    ?- [X, Y] tent_set [3, 4], Z tent_is X + Y.
    X = X{3 -> 0}
    Y = Y{4 -> 0}
    Z = Z{7 -> 0}
    Yes (0.00s cpu)

    ?- [X, Y] tent_set [3, 4], Z tent_is X + Y, X tent_set 7.
    X = X{7 -> 0}
    Y = Y{4 -> 0}
    Z = Z{11 -> 0}
    Yes (0.00s cpu)
    "
]).

:- tool((tent_is)/2, (tent_is_)/3).
tent_is_(Val,Expr,_Module) :- var(Expr), !,
	( has_tent_value(Expr) ->
	    Val = Expr
	;
	    exit_block(tentative_value_not_set)
	).
tent_is_(Val,Expr,_Module) :- number(Expr), !,
	Val = Expr.
tent_is_(Sum, Expr, Module) :-
	linearize(Expr, [Cst*1 | Terms], NonLin),
	% treat the nonlinear components
	( foreach(V = NonLinExpr, NonLin), param(Module) do
	    update_expr(V, NonLinExpr,Module)
	),
	(
	    foreach(C*V, Terms),
	    fromto(Cst, In, Out, TentSum),
	    param(Sum)
	do
	    Out is In + C * tent_get(V),
	    register_for_sum(V, C, Sum)
	),
	Sum tent_set TentSum.



:- tool(tent_addto/2, tent_addto_/3).
tent_addto_(Val,Expr,_Module) :- var(Expr), !,
	( has_tent_value(Expr) ->
	    Val = Expr
	;
	    exit_block(tentative_value_not_set)
	).
tent_addto_(Sum, Expr, Module) :-
	tent_is_(Aux, Expr, Module),
	NewSum is tent_get(Sum) + tent_get(Aux),
	tent_set(Sum, NewSum),
	register_for_sum(Aux, 1, Sum).



% Out is guaranteed to be a var.
update_expr(Out, Expr, Module) :-
	term_variables(Expr, In),
	tent_call(In, Out, Out is Expr, Module).
	


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
	( var(Susp) ->
	    suspend(tent_call(In, Out, Goal, Module, Susp),
		    2, [In->tent_chg], Susp)
	;
	    true
	),
	Out tent_set Out0.


%----------------------------------------------------------------------
% Tracing
%----------------------------------------------------------------------

%:- export tent_trace/3.
tent_trace(Stream, Name, X) :-
	register_for_notification(X, 0, Receiver),
	suspend(tent_trace_demon(Stream, Name, Receiver, Susp), 1, X->tent_chg, Susp).

:- export tent_trace_array/3.
:- comment(tent_trace_array/3, [
    summary:"Simple tracing facility for several variables",
    amode:(tent_trace_array(+,+,+) is det),
    args:["Stream":"A stream identifier",
    	"Name":"Usually atom or string (but general term allowed)",
    	"ArrayList":"Array or list of tentative variables"],
    see_also:[library(viewable)],
    desc:html("
	<P>
	This predicate sets up a demon that prints a message whenever the
	tentative value of one of the variables in ArrayList changes.
	The message is printed onto Stream and is of the form
	</P><PRE>
		Name[Index]: chg(Old,New)
	</PRE><P>
	where Index is the index of the changed variable in the given
	array, Old is the tentative value before the change, and New
	the tentative value after the change.
	</P><P>
	Another way of tracing tentative value changes is by using the
	Visualisation Tools.
	</P>
    "),
    eg:"
    ?- Xs = [X,Y], tent_set(Xs, [1,2]),
      tent_trace_array(output, hello, Xs),
      tent_set(X, 7),
      tent_set(Y, 3),
      tent_set([X,Y], [5,5]).

    hello[1]: chg(1, 7)
    hello[2]: chg(2, 3)
    hello[1]: chg(7, 5)
    hello[2]: chg(3, 5)

    Xs = [X{5 -> 0}, Y{5 -> 0}]
    There are 2 delayed goals.
    Yes (0.02s cpu)
    "
]).

tent_trace_array(Stream, Name, Xs0) :-
	arrayify(Xs0, Xs),
	( foreacharg(X,Xs,I), param(Stream,Name) do
	    register_for_notification(X, I, Receiver),
	    suspend(tent_trace_demon(Stream, Name, Receiver, Susp), 1, X->tent_chg, Susp)
	).

    :- demon tent_trace_demon/4.
    tent_trace_demon(Stream, Name, Receiver, Susp) :-
	foreachnotification(tent_trace_demon,
		I:Notification, [Stream, Name], Receiver, Status, (
	    printf(Stream, "%w[%w]: %w%n", [Name,I,Notification])
%	    writeln(Stream, Name:Notification)
	)),
	( Status = closed ->
	    writeln(Stream, Name:Status),
	    kill_suspension(Susp)
	; true ).
	

    arrayify(Thing, Array) :-
    	( functor(Thing, [], _) ->
	    Array = Thing
	;
	    Array =.. [[]|Thing]
	).

