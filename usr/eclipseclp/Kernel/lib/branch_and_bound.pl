% ----------------------------------------------------------------------
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
% Copyright (C) 1999-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: branch_and_bound.pl,v 1.7 2015/04/04 22:05:50 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(branch_and_bound).

:- comment(categories, ["Algorithms"]).
:- comment(summary, "Generic branch-and-bound optimization").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Joachim Schimpf, Vassilis Liatsos, IC-Parc, Imperial College, London").
:- comment(date, "$Date: 2015/04/04 22:05:50 $").
:- comment(index, ["branch-and-bound","dichotomic search","optimization"]).
:- comment(desc, html("<P>
    This is a solver-independent library implementing branch-and-bound
    optimization. It can be used with any nondeterministic search routine
    that instantiates a cost variable when a solution is found. The cost
    variable can be an arbitrary numerical domain variable or even a
    simple domain-less Prolog variable.
</P><P>
    The main primitive is bb_min/3.  Assume we have the following
    collection of facts:
<PRE>
        % item(Food, Calories, Price)
        item(bread,  500, 1.50).
        item(eggs,   600, 1.99).
        item(milk,   400, 0.99).
        item(apples, 200, 1.39).
        item(butter, 800, 1.89).
</PRE>
   Then we can find a minimum-calorie solution as follows:
<PRE>
        ?- bb_min(item(Food,Cal,Price), Cal, _).
        Found a solution with cost 500
        Found a solution with cost 400
        Found a solution with cost 200
        Found no solution with cost -1.0Inf .. 199

        Food = apples
        Cal = 200
        Price = 1.39
        Yes (0.00s cpu)
</PRE>
    In this example, the item/3 predicate serves as a nondeterministic
    generator of solutions with different values for the variable Cal,
    which we have chosen as our cost variable.  As can be seen from the
    progress messages, the optimization procedure registers increasingly
    good solutions (i.e. solutions with smaller cost), and finally delivers
    the minimum-cost solution with Cal=200.
</P><P>
    Alternatively, we can minimize the item price:
<PRE>
        ?- bb_min(item(Food,Cal,Price), Price, bb_options{delta:0.05}).
        Found a solution with cost 1.5
        Found a solution with cost 0.99
        Found no solution with cost -1.0Inf .. 0.94

        Food = milk
        Cal = 400
        Price = 0.99
        Yes (0.00s cpu)
</PRE>
    Because the price is non-integral, we had to adjust the step-width
    of the optimization procedure using the delta-option.
</P>
<H3>Optimization with Constraints</H3>
<P>
    This library is designed to work together with arbitrary constraint
    solvers, for instance library(ic).  The principle there is to wrap
    the solver's nondeterministic search procedure into a bb_min/3 call.
    This turns a program that finds all solutions into one that finds
    the best solution.  For example:
<PRE>
        ?- [X,Y,Z] #:: 1..5,                   % constraints (model)
           X+Z #>= Y,

           C #= 3*X - 5*Y + 7*Z,               % objective function

           bb_min(labeling([X,Y,Z]), C, _).    % nondet search + b&b

        Found a solution with cost 5
        Found a solution with cost 0
        Found a solution with cost -2
        Found a solution with cost -4
        Found a solution with cost -6
        Found no solution with cost -15.0 .. -7.0
        X = 4
        Y = 5
        Z = 1
        C = -6
        Yes (0.00s cpu)
</PRE>
    The code shows the general template for such an optimization solver:
    All constraints should be set up BEFORE the call to bb_min/3,
    while the nondeterministic search procedure (here labeling/1)
    must be invoked WITHIN bb_min/3.  The branch-and-bound procedure
    only works if it envelops all nondeterminism.
</P><P>
    The cost variable (here C) must be defined in such a way that it is
    instantiated (possibly by propagation) whenever the search procedure
    succeeds with a solution.  Moreover, good, early bounds on the cost
    variable are important for efficiency, as they help the branch-and-bound
    procedure to prune the search.  Redundant constraints on the cost
    variable can sometimes help.
</P>

<H3>Note on the treatment of bounded reals</H3>
<P>
    The library allows the cost to be instantiated to a number of type
    breal.  This is useful e.g. when using lib(ic) to solve problems
    with continuous variables.  When the variable domains have been
    narrowed sufficiently, the problem variables (in particular the
    cost variable) should be instantiated to a bounded real, e.g.
    using the following idiom:
    <PRE>
	    X is breal_from_bounds(get_min(X),get_max(X))
    </PRE>
    Bounded reals contain some uncertainty about their true value. If
    this uncertainty is too large, the branch-and-bound procedure may
    not be able to compare the quality of two solutions. In this case,
    a warning is issued and the search terminated prematurely.  The
    problem can be solved by increasing the delta-parameter, or by
    locating the cost value more precisely.
</P>")).

:- export
	bb_min/3,		% main predicate
	bb_min_cost/4,
	bb_min/6,
	minimize/2,

	bb_init/2,		% underlying primitives
	bb_cost/2,
	bb_solution/2,
	bb_finish/1,
	bb_probe/7.

:- meta_predicate((
	bb_min(0,*,*),
	bb_min(0,*,*,*,*,*),
	minimize(0,*),
	bb_probe(*,*,0,*,*,*,*)
    )).

:- export struct(bb_options(
	    strategy,		% atom
	    from,		% number
	    to,			% number
	    delta,		% number
	    factor,		% number
	    timeout,		% number
	    probe_timeout,	% number
	    solutions,		% atom
	    report_success,	% GoalPrefix/A
	    report_failure,	% GoalPrefix/A
	    report_timeout	% GoalPrefix/A
	)).

:- lib(timeout).

:- import
	% general-purpose predicates
	call_local/1,
	par_true/0,
	worker_boundary/0
    from sepia_kernel.


%----------------------------------------------------------------------
% Toplevel
%----------------------------------------------------------------------

:- tool(minimize/2, minimize/3).
minimize(Goal, Cost, Module) :-
	bb_min(Goal, Cost, _DefaultOptions, Module).

:- tool(bb_min/3, bb_min/4).
bb_min(Goal, Cost, Options, Module) :-
	% we used to use the Goal directly as the Template, but that can
	% cause problems when Goal contains strange stuff, like handles
	term_variables(Goal, Template),
	bb_min(Goal, Cost, Template, Solution, Opt, Options, Module),
	( Options = bb_options{solutions:one} ->
	    % instantiate to the optimum solution (atomically)
	    Template-Cost = Solution-Opt
	;
	    % impose Opt as a lower bound, because there could be
	    % better solutions within the delta-range
	    set_var_bounds(Cost, -1.0Inf, Opt),
	    get_var_bounds(Cost, _, Max),
	    % only if bound can't be imposed, instantiate instead
	    ( Max =< Opt -> true ; Cost = Opt ),
	    call(Goal)@Module
	).

:- tool(bb_min_cost/4, bb_min_cost_/5).
bb_min_cost_(Goal, Cost, Opt, Options, Module) :-
	bb_min(Goal, Cost, [], [], Opt, Options, Module).

:- tool(bb_min/6, bb_min/7).
bb_min(Goal, Cost, Template, Solution, Opt, Options, Module) :-
	( var(Cost) ; number(Cost) ),
	callable(Goal),
	default_options(Options),
	check_options(Options),
	!,
	initial_bounds(Cost, Options, From, To),
	bb_init(To, Handle),
	Options = bb_options{timeout:Timeout},
        ( Timeout > 0, Timeout < 1.0Inf ->
	    timeout(
		bb_any(From, To, Goal, Template, Cost, Handle, Module, Options),
		Timeout,
		report_timeout(Options, From..To, Handle, Module)
	    )
	;
	    bb_any(From, To, Goal, Template, Cost, Handle, Module, Options)
	),
	bb_solution(Handle, Solution),	% may fail
	bb_cost(Handle, Opt),
	bb_finish(Handle).
bb_min(Goal, Cost, _Template, _Solution, _Opt, Options, Module) :-
	error(5, bb_min(Goal, Cost, Options), Module).

    bb_any(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	% The following line is for parallelism (scheduling and copying)
	%worker_boundary, par_true,
	Options = bb_options{strategy:Strategy},
	( Strategy == continue ->
	    bb_continue(From, To, Goal, Template, Cost, Handle, Module, Options)
	; Strategy == restart ->
	    bb_delta(From, To, Goal, Template, Cost, Handle, Module, Options)
	; Strategy == step ->
	    bb_delta(From, To, Goal, Template, Cost, Handle, Module, Options)
	; Strategy == dichotomic ->
	    bb_dichotomic(From, To, Goal, Template, Cost, Handle, Module, Options)
	;
	    error(6, bb_min(Goal, Cost, Options), Module)
	).
    bb_any(_From,_To,_Goal,_Template,_Cost,_Handle,_Module,_Options).


default_options(bb_options{
	    from:From,to:To,factor:Factor,strategy:Strategy,solutions:Solutions,
	    delta:Delta,timeout:Timeout,probe_timeout:ProbeTimeout}) :-
	set_default(Strategy, continue),
	set_default(Solutions, one),
	set_default(From, -1.0Inf),
	set_default(To, 1.0Inf),
	( Strategy==dichotomic -> set_default(Factor, 0.5)
	; set_default(Factor, 1) ),
	set_default(Delta, 1),
	set_default(Timeout, 0),
	set_default(ProbeTimeout, 0).

    set_default(X, X) :- !.
    set_default(_, _).

check_options(bb_options{from:From,to:To,factor:Factor,solutions:Solutions,
	     delta:Delta,timeout:Timeout,probe_timeout:ProbeTimeout}) :-
	precise_number(From),
	precise_number(To),
	precise_number(Delta),
	precise_number(Factor),
	(Solutions==one;Solutions==all),
	From =< To,
	0 < Factor, Factor =< 1, 
	0 < Delta,
	0 =< Timeout,
	0 =< ProbeTimeout.

precise_number(X) :- breal(X), !,
	printf(error, "branch_and_bound: parameter must not be a bounded real: %w%n", [X]),
	fail.
precise_number(X) :- number(X).

%----------------------------------------------------------------------
% Stepwise improvement
% bb_delta always fails or aborts!
%----------------------------------------------------------------------

bb_delta(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	( bb_probe(From, To, Goal, Template, Cost, Handle, Module,Options) ->	% may fail
	    Best is bb_cost(Handle),
	    step(From, Best, Options, NewTo),		% may fail
	    bb_delta(From, NewTo, Goal, Template, Cost, Handle, Module, Options)
	;
	    report_failure(Options, From..To, Handle, Module),
	    fail
	).

    % fails if there is no range left to explore
    % PRE: no breals among the inputs, except Best
    step(From, Best, bb_options{factor:Factor,delta:Delta}, NewTo) :-
	To is number_max(Best),	% we are trying to improve the guaranteed best
	Gap is To - From,
	Gap > 0,		% termination condition
	( Gap < 1.0Inf ->
	    NewTo is min(From + Gap*Factor, To-Delta),
	    NewTo < To		% can only be violated if precision problems
	;
	    NewTo is To - Delta
	),
	( NewTo < number_min(Best) ->
	    true
	;
	    % this prevents looping with overlapping costs
	    writeln(warning_output, "WARNING: bb_min: search terminated prematurely - cost uncertainty too large."),
	    writeln(warning_output, "Either increase bb_min delta parameter, or compute cost more precisely."),
	    fail
	).

    % like breal_max, but preserve the type for non-breals
    number_max(X, Max) :- breal(X), !, breal_max(X, Max).
    number_max(X, X).

    % like breal_min, but preserve the type for non-breals
    number_min(X, Min) :- breal(X), !, breal_min(X, Min).
    number_min(X, X).

%----------------------------------------------------------------------
% Stepwise improvement without restart
% bb_continue always fails or aborts!
%
% The search tree is only traversed once. Every time a solution is
% found, the new cost bound gets imposed dynamically for the rest
% of the search. To do that, we use the fail-event mechanism to trigger
% a demon that imposes the current global bound on the cost variable.
% This is necessary in two situations:
% 1. If we fail across a change of the global bound (after a new solution)
% 2. If we fail across the imposition of a new bound on the cost variable
%    (we have to do it again because the effect is lost)
% For the sake of completeness, we also take care of the case where
% the cost variable is not a domain variable and cannot have a bound
% imposed. In that case, the demon must additionally be woken on
% instantiation of the cost variable to effect the check.
%----------------------------------------------------------------------

:- import
	request_fail_event/3,
	timestamp_init/2,
	timestamp_update/2
    from sepia_kernel.

:- set_event_handler('branch_and_bound:apply_bound', trigger/1).


bb_continue(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	(
	    Stamp = stamp(_),
	    timestamp_init(Stamp, 1),
	    timestamp_update(Stamp, 1),
	    suspend(bb_impose_bound(Cost, Handle, Stamp, From),
		2,	% this should be between 1 (debug) and 2 (constraints)
		trigger('branch_and_bound:apply_bound'), Susp),
	    ( cannot_impose_bound(Cost) ->
		insert_suspension(Cost, Susp, inst of suspend, suspend)
	    ;
		true
	    ),
	    call_local((
		    % impose initial bounds, if possible
		    set_var_bounds(Cost, From, To),
		    Goal,
		    kill_suspension(Susp)
		))@Module,
	    ( var(Cost) ->
		writeln(error, "bb_min: search did not instantiate cost variable"),
		abort
	    ;
		true
	    ),
	    % In case the solution still contains variables,
	    % we want to strip most of their attributes.
	    % Otherwise we might copy the whole constraint store!
	    copy_term(Template, StrippedSolution),
	    % Compute the new bound
	    ( step(From, Cost, Options, NewTo) ->
		% Set all shelf fields atomically (timeout!)
		shelf_set(Handle, 0, sol(s(StrippedSolution),Cost,NewTo)),
		request_fail_event(Stamp, 1, 'branch_and_bound:apply_bound')
	    ;
		% Set all shelf fields atomically (timeout!)
		shelf_set(Handle, 0, sol(s(StrippedSolution),Cost,From)),
	    	% We found an optimal solution, no need to continue search
		!
	    ),
	    report_success(Options, Cost, Handle, Module),
	    fail	% continue search, fail into Goal
	;
	    shelf_get(Handle, 3, NewTo),
	    report_failure(Options, From..NewTo, Handle, Module),
	    fail
	).


% This demon will be woken after failures when a global bound needs
% to be re-imposed. It will itself schedule another event to be raised
% in case we fail across the current demon invocation.
% The bound is imposed using set_var_bounds/3. In case the Cost
% variable does not have a domain which can represent that,
% we need to wake also when Cost gets instantiated.

:- demon bb_impose_bound/4.
bb_impose_bound(Cost, _Handle, _Stamp, _From) :-
	% Optimization: if Cost is a free variable, we can't impose bound
	% neither now nor further up the search tree. No point scheduling
	% further fail-events, need to wait for instantiation of Cost.
	free(Cost), !.
bb_impose_bound(Cost, Handle, Stamp, From) :-
	% (number(Cost) ; meta(Cost))
	% when we fail across this point, we will need to re-apply the bound!
	request_fail_event(Stamp, 1, 'branch_and_bound:apply_bound'),
	% this will apply the bound, if Cost is a domain variable or constat
	NewTo is bb_bound(Handle),
%	writeln(bb_impose_bound(Cost::From..NewTo)),
	set_var_bounds(Cost, From, NewTo).


% succeed if the argument cannot have a bound imposed
% This fails for breals, although the bound cannot really be imposed if
% it overlaps, but we take care of that elsewhere.

cannot_impose_bound(X) :- free(X).
cannot_impose_bound(X) :- meta(X),
	get_var_bounds(X, L, H),
	H =:= 1.0Inf,
	L =:= -1.0Inf,
	% try imposing a bound and check if it worked
	call_priority((
		set_var_bounds(X, L, 0),
		get_var_bounds(X, _, H1),
		H1 =:= 1.0Inf
	    ), 1).


%----------------------------------------------------------------------
% Dichotomic search
% bb_dichotomic always fails or aborts!
%----------------------------------------------------------------------

bb_dichotomic(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	% look for an initial solution
	( bb_probe(From, To, Goal, Template, Cost, Handle, Module, Options) ->	% may fail
	    Best is bb_cost(Handle),
	    bb_dichotomic1(From, false, Best, Goal, Template, Cost, Handle, Module, Options)
	;
	    report_failure(Options, From..To, Handle, Module),
	    fail
	).

    % We assume that there is a solution with cost Best.
    % If FromNoSol==true then we know there is no solution with cost From
    bb_dichotomic1(From, FromNoSol, Best, Goal, Template, Cost, Handle, Module, Options) :-
	split(From, FromNoSol, Best, Options, Split), 	% may fail
	( bb_probe(From, Split, Goal, Template, Cost, Handle, Module, Options) ->
	    NewBest is bb_cost(Handle),
	    bb_dichotomic1(From, FromNoSol, NewBest, Goal, Template, Cost, Handle, Module, Options)
	;
	    report_failure(Options, From..Split, Handle, Module),
	    bb_dichotomic1(Split, true, Best, Goal, Template, Cost, Handle, Module, Options)
	).

    % PRE: no breals among the inputs, except Best
    split(From, FromNoSol, Best, bb_options{factor:Factor,delta:Delta}, Split) :-
	To is number_max(Best),	% we are trying to improve the guaranteed best
	Gap is To - From,
	( FromNoSol == true ->
	    Gap > Delta		% termination condition
	;
	    Gap >= Delta	% termination condition
	),
	( Gap < 1.0Inf ->
	    % Normally, Split is From + Gap*Factor, but we
	    % have to consider all sorts of special cases...
	    FromPlusDelta is From + Delta,
	    ToMinusDelta is To - Delta,
	    NormalSplit is From + Gap*Factor,
	    ( FromPlusDelta > ToMinusDelta ->
		% Gap size between Delta and 2*Delta: split in the middle,
		% so both probe-range and improvement are at least Delta/2
		Split is From + Gap*0.5
	    ; NormalSplit < FromPlusDelta ->
		% make sure the probe covers at least a Delta-range,
		% otherwise proving optimality can take a long time
	    	Split = FromPlusDelta
	    ; NormalSplit > ToMinusDelta ->
		% make sure the improvement is at least Delta,
		% otherwise we might iterate through too many solutions
	    	Split = ToMinusDelta
	    ;
		% normal case: split according to factor
	    	Split = NormalSplit
	    ),
	    % Following line is normally redundant, but can be violated in
	    % case of precision/rounding problems. It then prevents looping.
	    Split < To
	; From >= 0 ->
	    Split is From + 10000000
	; To =< 0 ->
	    Split is To - 10000000
	;
	    Split = 0.0
	),
	( Split < number_min(Best) ->
	    true
	;
	    % this prevents looping with overlapping costs
	    writeln(warning_output, "WARNING: bb_min: search terminated prematurely - cost uncertainty too large."),
	    writeln(warning_output, "Either increase bb_min delta parameter, or compute cost more precisely."),
	    fail
	).

%----------------------------------------------------------------------
% Primitives
%----------------------------------------------------------------------

bb_init(ExtremeCost, Handle) :-
	shelf_create(sol(no_solution,ExtremeCost,ExtremeCost), Handle).

bb_cost(Handle, Cost) :-
	shelf_get(Handle, 2, Cost).

bb_solution(Handle, Solution) :-
	shelf_get(Handle, 1, s(Solution)).	% fail here if no solution

bb_bound(Handle, Bound) :-
	shelf_get(Handle, 3, Bound).

bb_finish(Handle) :-
	shelf_abolish(Handle).


% bb_probe tries to find a solution for Goal in the range From..To.
% If there is a solution, its Template and Cost are stored in Handle,
% the computation is undone, and bb_probe succeeds.
% If there is no solution, Handle is not changed and bb_probe fails.

bb_probe(From, To, Goal, Template, Cost, Handle, Module) :-
	bb_probe(From, To, Goal, Template, Cost, Handle, Module,
		bb_options{report_success:true/0}).

bb_probe(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	(
	    call_local((
		    % impose bounds early if possible
		    set_var_bounds(Cost, From, To),
		    Goal,
		    % in case there was no set_bounds handler:
		    set_var_bounds(Cost, From, To)
		))@Module
	->
	    ( var(Cost) ->
	    	writeln(error, "bb_min: search did not instantiate cost variable"),
		abort
	    ;
		true
	    ),
	    % In case the solution still contains variables,
	    % we want to strip most of their attributes.
	    % Otherwise we might copy the whole constraint store!
	    copy_term(Template, StrippedSolution),
	    % Set all shelf fields atomically (timeout!)
	    shelf_set(Handle, 0, sol(s(StrippedSolution),Cost,Cost)),
	    report_success(Options, Cost, Handle, Module),
	    fail	% to undo the call-effect and succeed with 2nd clause
	;
	    !,
	    fail
	).
bb_probe(_From, _To, _Goal, _Template, _Cost, _Handle, _Module, _Options).


% Get an initial lower and upper bound for the search by intersecting
% the bounds from propagation with the user option input (if any)

initial_bounds(Cost, Options, Lower, Upper) :-
	Options = bb_options{from:UserFrom, to:UserTo},
	get_var_bounds(Cost, CostL, CostU),
	Lower is max(UserFrom, CostL),
	Upper is min(UserTo, CostU).


report_success(bb_options{report_success:Spec}, Cost, Handle, Module) :-
	report_result(Spec, "Found a solution with cost %q%n", Cost, Handle, Module).

report_failure(bb_options{report_failure:Spec}, Range, Handle, Module) :-
	report_result(Spec, "Found no solution with cost %q%n", Range, Handle, Module).

report_timeout(bb_options{report_timeout:Spec}, _InitialRange, Handle, Module) :-
	bb_cost(Handle, Cost),
	report_result(Spec, "Branch-and-bound timeout while searching for solution better than %q%n%b", Cost, Handle, Module).

    report_result(Prefix/A, Msg, Cost, Handle, Module) ?-
    	callable(Prefix),
	!,
	( A==0 -> call(Prefix)@Module
	; A==1 -> call(Prefix,Cost)@Module
	; A==2 -> call(Prefix,Cost,Handle)@Module
	; A==3 -> call(Prefix,Cost,Handle,Module)@Module
	; printf(log_output, Msg, Cost)
	).
    report_result(Prefix, _Msg, Range, Handle, Module) :-
	callable(Prefix),
	!,
	call(Prefix, Range, Handle, Module)@Module.
    report_result(_, Msg, Cost, _, _) :-
	printf(log_output, Msg, Cost).


%----------------------------------------------------------------------
% Documentation
%----------------------------------------------------------------------

:- comment(minimize/2, [
    summary:"Find a minimal solution using the branch-and-bound method",
    desc:html("This is a shorthand for
    	<PRE>
	bb_min(+Goal, ?Cost, _DefaultOptions)
	</PRE>
	See bb_min/3 for details."),
    template:"minimize(+Goal, ?Cost)",
    see_also:[bb_min/3]]).


:- comment(bb_min/3, [
    summary:"Find one or all minimal solutions using the branch-and-bound method",
    see_also:[bb_min/6],
    desc:html("\
	A solution of the goal <EM>Goal</EM> is found that minimizes
	the value of <EM>Cost</EM>.  <EM>Cost</EM> should be a
	variable that is affected, and eventually instantiated, by
	<EM>Goal</EM>.  Usually, <EM>Goal</EM> is the search procedure
	of a constraint problem and <EM>Cost</EM> is the variable
	representing the cost.  The solution is found using the branch
	and bound method:  as soon as a solution is found, it gets
	remembered and the search is continued or restarted with an
	additional constraint on the <EM>Cost</EM> variable which
	requires the next solution to be better than the previous one. 
	Iterating this process finally yields an optimal solution.
</P><P>
	The possible options are
	<DL>
	<DT><STRONG>strategy:</STRONG></DT><DD>
	    <DL>
	    <DT>continue (default)</DT>
	    	<DD>after finding a solution, continue search with the newly
		found bound imposed on Cost</DD>
	    <DT>restart</DT>
	    	<DD>after finding a solution, restart the whole search with
		the newly found bound imposed on Cost</DD>
	    <DT>step</DT>
	    	<DD>a synonym for 'restart'</DD>
	    <DT>dichotomic</DT>
	    	<DD>after finding a solution, split the remaining cost range
		and restart search to find a solution in the lower sub-range.
		If that fails, assume the upper sub-range as the remaining
		cost range and split again.</DD>
	    </DL>
	    The new bound (or the split point, respectively), is computed
	    from the current best solution, taking into account the
	    parameters delta and factor.
	    </DD>
	<DT><STRONG>from:</STRONG></DT>
	    <DD>number - an initial lower bound for the cost (default -1.0Inf).
	    Only useful if Cost is not a domain variable.</DD>

	<DT><STRONG>to:</STRONG></DT>
	    <DD>number - an initial upper bound for the cost (default +1.0Inf).
	    Only useful if Cost is not a domain variable.</DD>

	<DT><STRONG>delta:</STRONG></DT>	
	    <DD>number - minimal absolute improvement required for each step
	    (applies to all strategies). The default value of 1.0 is
	    appropriate for integral costs.  Any solution that improves on
	    the best solution by less than this value will be missed.</DD>

	<DT><STRONG>factor:</STRONG></DT>
	    <DD>number - minimal improvement ratio (with respect to the lower
	    cost bound) for strategies 'continue' and 'restart' (default 1.0),
	    or split factor for strategy 'dichotomic' (default 0.5)</DD>

	<DT><STRONG>solutions:</STRONG></DT><DD>
	    <DL>
	    <DT>one (default)</DT><DD>
		Compute one (of possibly multiple) optimal solutions.</DD>
	    <DT>all</DT><DD>
		Nondeterministically compute all optimal solutions.
		This has a performance penalty, as the search is restarted
		one more time after the optimum has been determined.</DD>
	    </DL>
	    Note the dependence on the delta-parameter: the costs of these
	    solutions may deviate by less than delta from the true optimum.</DD>

	<DT><STRONG>timeout:</STRONG></DT>
	    <DD>number - maximum seconds of cpu time to spend (default: no limit)</DD>

	<DT><STRONG>report_success:</STRONG></DT>
	    <DD>GoalPrefix/N - this specifies a goal to be invoked whenever
	    the branch-and-bound process finds a better solution.  GoalPrefix
	    is a callable term (atom or compound) and N is an integer between
	    0 and 3.  The invoked goal is constructed by adding N optional
	    arguments to GoalPrefix: Cost, Handle and Module.  Cost is
	    a float number representing the cost of the solution found,
	    Handle is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.  
	    To disable any reporting, choose report_success:true/0.
	    The default handler prints a message to log_output.</DD>

	<DT><STRONG>report_failure:</STRONG></STRONG></DT>
	    <DD>GoalPrefix/N - this specifies a goal to be invoked whenever
	    the branch-and-bound process cannot find a solution in a cost
	    range.  GoalPrefix is a callable term (atom or compound) and
	    N is an integer between 0 and 3.  The invoked goal is
	    constructed by adding N optional arguments to GoalPrefix:
	    Cost, Handle and Module.   Cost is a From..To structure
	    representing the range of cost in which no solution could be found,
	    Handle is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.
	    To disable any reporting, choose report_failure:true/0.
	    The default handler prints a message to log_output.</DD>

	<DT><STRONG>report_timeout:</STRONG></DT>
	    <DD>GoalPrefix/N - this specifies a goal to be invoked when the
	    branch-and-bound process times out.  GoalPrefix is a callable
	    term (atom or compound) and N is an integer between 0 and 3.
	    The invoked goal is constructed by adding N optional arguments
	    to GoalPrefix: Cost, Handle and Module.  Cost is a float number
	    representing the cost of the best solution found, Handle
	    is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.
	    To disable any reporting, choose report_timeout:true/0.
	    The default handler prints a message to log_output.</DD>
	</DL>
	The default options can be selected by passing a free variable as
	the Options-argument. To specify other options, pass a bb_options-
	structure in struct-syntax, e.g.
	<PRE>
	    bb_min(..., ..., bb_options{strategy:dichotomic, timeout:60})
	</PRE>
</P><P>
	In order to maximize instead of minimizing, introduce a negated
	cost variable in your model and minimize that instead, e.g.
	<PRE>
	    % maximize Profit
	    Cost #= -Profit,
	    bb_min(search(...), Cost, bb_options{}),
	</PRE>
</P>"),
    args:["Goal":"The (nondeterministic) search goal",
	"Cost":"A (usually numeric domain) variable representing the cost",
	"Options":"A bb_options structure or variable"],
    fail_if:"Goal has no solutions",
    amode:(bb_min(+,?,+) is nondet),
    eg:"
% simple minimization with default options
    ?- bb_min(member(X,[9,6,8,4,7,2,4,7]), X, Options).
    Found a solution with cost 9
    Found a solution with cost 6
    Found a solution with cost 4
    Found a solution with cost 2
    Found no solution with cost -1.0Inf .. 1
    X = 2
    Options = bb_options(continue, -1.0Inf, 1.0Inf, 1, 1, 0, 0, _, _)
    yes.

% coarser granularity: faster, but missing the optimum
    ?- bb_min(member(X,[9,6,8,4,7,2,4,7]), X, bb_options{delta:4}).
    Found a solution with cost 9
    Found a solution with cost 4
    Found no solution with cost -1.0Inf .. 0
    X = 4
    yes.

% alternative strategy based on bisecting the cost space
    ?- bb_min(member(X,[99,60,80,40,70,30,70]), X,
	    bb_options{strategy:dichotomic, from:0}).
    Found a solution with cost 99
    Found a solution with cost 40
    Found no solution with cost 0.0 .. 20.0
    Found a solution with cost 30
    Found no solution with cost 20.0 .. 25.0
    Found no solution with cost 25.0 .. 27.5
    Found no solution with cost 27.5 .. 28.75
    Found no solution with cost 28.75 .. 29.0
    X = 30
    yes.

% examples with library(ic) constraints
    ?- [X,Y,Z] :: 1..5,                    % constraints (model)
       X+Z #>=Y,
       C #= 3*X - 5*Y + 7*Z,               % objective function
       bb_min(labeling([X,Y,Z]), C, _).    % nondet search + b&b

    Found a solution with cost 5
    Found a solution with cost 0
    Found a solution with cost -2
    Found a solution with cost -4
    Found a solution with cost -6
    Found no solution with cost -15.0 .. -7.0
    X = 4
    Y = 5
    Z = 1
    C = -6
    Yes (0.00s cpu)


    ?- [X,Y,Z] :: 1..5,
       X+Z #>=Y,
       C #= 3*X - 5*Y + 7*Z,
       bb_min(search([X,Y,Z],0,input_order,indomain_middle,complete,[]), C, _).

    Found a solution with cost 15
    Found a solution with cost 8
    Found a solution with cost 1
    Found a solution with cost -4
    Found a solution with cost -6
    Found no solution with cost -15.0 .. -7.0
    X = 4
    Y = 5
    Z = 1
    C = -6
    Yes (0.00s cpu)


"]).


:- comment(bb_min_cost/4, [
    summary:"Find the minimal cost using the branch-and-bound method",
    args:["Goal":"The (nondeterministic) search goal",
	"Cost":"A (usually numeric domain) variable representing the cost",
	"Optimum":"A variable which will be set to the optimum value of Cost",
	"Options":"A bb_options structure or variable"],
    see_also:[bb_min/3,bb_min/6],
    desc:html("<P>\
    Determines the minimum possible value that the variable Cost can
    attain in any solution of Goal.  This value is returned as Optimum.
    Neither Cost nor any variable in Goal is instantiated.  The predicate
    is useful when one is interested in sub-optimal solutions, see example.
</P><P>
    This predicate can be defined as
<PRE>
	bb_min_cost(Goal, Cost, Optimum, Options) :-
		bb_min(Goal, Cost, [], _, Optimum, Options).
</PRE>
    Options are interpreted in the same way as for bb_min/6
    (the solutions-parameter is ignored).
</P>"),
    fail_if:"Goal has no solutions",
    amode:(bb_min_cost(+,?,-,+) is semidet),
    eg:"
    % A predicate to enumerate solutions in increasing cost order
    :- lib(ic).
    :- lib(branch_and_bound).

    ic_increasing_cost(Goal, Cost) :-
    	bb_min_cost(Goal, Cost, Opt,
		    bb_options{report_success:true/0,report_failure:true/0}),
	(
	    Cost = Opt,
	    call(Goal)
	;
	    Cost #> Opt,
	    ic_increasing_cost(Goal, Cost)
	).

    % sample run:
    ?- ic_increasing_cost(member(C-X,[9-a,4-b,2-c,4-d]), C).
    C = 2
    X = c
    Yes (0.00s cpu, solution 1, maybe more) ? ;
    C = 4
    X = b
    Yes (0.00s cpu, solution 2, maybe more) ? ;
    C = 4
    X = d
    Yes (0.00s cpu, solution 3, maybe more) ? ;
    C = 9
    X = a
    Yes (0.00s cpu, solution 4, maybe more) ? ;
    No (0.00s cpu)
    "
    ]).

:- comment(bb_min/6, [
    summary:"Find a minimal solution using the branch-and-bound method",
    see_also:[bb_min/3],
    desc:html("\
	A solution of the goal <EM>Goal</EM> is found that minimizes
	the value of <EM>Cost</EM>.  <EM>Cost</EM> should be a
	variable that is affected, and eventually instantiated, by
	<EM>Goal</EM>.  Usually, <EM>Goal</EM> is the search procedure
	of a constraint problem and <EM>Cost</EM> is the variable
	representing the cost.  The solution is found using the branch
	and bound method:  as soon as a solution is found, it gets
	remembered and the search is continued or restarted with an
	additional constraint on the <EM>Cost</EM> variable which
	requires the next solution to be better than the previous one. 
	Iterating this process yields an optimal solution in the end.
	<P>
	The possible options are
	<DL>
	<DT><STRONG>strategy:</STRONG></DT><DD>
	    <DL>
	    <DT>continue (default)</DT>
	    	<DD>after finding a solution, continue search with the newly
		found bound imposed on Cost</DD>
	    <DT>restart</DT>
	    	<DD>after finding a solution, restart the whole search with
		the newly found bound imposed on Cost</DD>
	    <DT>step</DT>
	    	<DD>a synonym for 'restart'</DD>
	    <DT>dichotomic</DT>
	    	<DD>after finding a solution, split the remaining cost range
		and restart search to find a solution in the lower sub-range.
		If that fails, assume the upper sub-range as the remaining
		cost range and split again.</DD>
	    </DL>
	    The new bound (or the split point, respectively), are computed
	    from the current best solution, taking into account the
	    parameters delta and factor.
	    </DD>
	<DT><STRONG>from:</STRONG></DT>
	    <DD>number - an initial lower bound for the cost (default -1.0Inf)</DD>

	<DT><STRONG>to:</STRONG></DT>
	    <DD>number - an initial upper bound for the cost (default +1.0Inf)</DD>

	<DT><STRONG>delta:</STRONG></DT>	
	    <DD>number - minimal absolute improvement required for each step
	    (default 1.0), applies to all strategies</DD>

	<DT><STRONG>factor:</STRONG></DT>
	    <DD>number - minimal improvement ratio (with respect to the lower
	    cost bound) for strategies 'continue' and 'restart' (default 1.0),
	    or split factor for strategy 'dichotomic' (default 0.5)</DD>

	<DT><STRONG>timeout:</STRONG></DT>
	    <DD>number - maximum seconds of cpu time to spend (default: no limit)</DD>

	<DT><STRONG>report_success:</STRONG></DT>
	    <DD>GoalPrefix/N - this specifies a goal to be invoked whenever
	    the branch-and-bound process finds a better solution.  GoalPrefix
	    is a callable term (atom or compound) and N is an integer between
	    0 and 3.  The invoked goal is constructed by adding N optional
	    arguments to GoalPrefix: Cost, Handle and Module.  Cost is
	    a float number representing the cost of the solution found,
	    Handle is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.  
	    To disable any reporting, choose report_success:true/0.
	    The default handler prints a message to log_output.</DD>

	<DT><STRONG>report_failure:</STRONG></STRONG></DT>
	    <DD>GoalPrefix/N - this specifies a goal to be invoked whenever
	    the branch-and-bound process cannot find a solution in a cost
	    range.  GoalPrefix is a callable term (atom or compound) and
	    N is an integer between 0 and 3.  The invoked goal is
	    constructed by adding N optional arguments to GoalPrefix:
	    Cost, Handle and Module.   Cost is a From..To structure
	    representing the range of cost in which no solution could be found,
	    Handle is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.
	    To disable any reporting, choose report_failure:true/0.
	    The default handler prints a message to log_output.</DD>

	<DT><STRONG>report_timeout:</STRONG></DT>
	    <DD>GoalPrefix/N - this specifies a goal to be invoked when the
	    branch-and-bound process times out.  GoalPrefix is a callable
	    term (atom or compound) and N is an integer between 0 and 3.
	    The invoked goal is constructed by adding N optional arguments
	    to GoalPrefix: Cost, Handle and Module.  Cost is a float number
	    representing the cost of the best solution found, Handle
	    is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.
	    To disable any reporting, choose report_timeout:true/0.
	    The default handler prints a message to log_output.</DD>
	</DL>
	The default options can be selected by passing a free variable as
	the Options-argument. To specify other options, pass a bb_options-
	structure in struct-syntax, e.g.
	<PRE>
	bb_options{strategy:dichotomic, timeout:60}
	</PRE>
	In order to maximize instead of minimizing, introduce a negated
	cost variable in your model and minimize that instead.
	<P>
	Unlike bb_min/3, bb_min/6 does <STRONG>not</STRONG> affect Goal or Cost after
	the optimum has been found. Instead, the optimum cost value is returned
	in Optimum, and the Solution argument gets unified with an instance of
	Template where the variables have the values that correspond to the
	optimal solution. Note that bb_min/3 is actually based on bb_min/6
	and can (for the one-solution case) be defined as:
	<PRE>
	bb_min(Goal, Cost, Options) :-
	    bb_min(Goal, Cost, Goal, Goal, Cost, Options).
	</PRE>
	"),
    args:["Goal":"The (nondeterministic) search goal",
	"Cost":"A (usually numeric domain) variable representing the cost",
	"Template":"A term containing all or some problem variables",
	"Solution":"A term which will be unified with the optimized Template",
	"Optimum":"A variable which will be set to the optimum value of Cost",
	"Options":"A bb_options structure or variable"],
    fail_if:"Goal has no solutions",
    amode:(bb_min(+,?,?,?,?,?) is semidet)
    ]).

:- comment(bb_init/2, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_init(++ExtremeCost, -Handle)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_cost/2, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_cost(++Handle, -Cost)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_solution/2, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_solution(++Handle, -Solution)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_finish/1, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_finish(++Handle)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_probe/7, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_probe(++From, ++To, +Goal, ?Template, ?Cost, ++Handle, ++Module)",
    desc:html("
	bb_probe tries to find a solution for Goal in the range From..To.
	If there is a solution, its Template and Cost are stored in Handle,
	the computation is undone, and bb_probe succeeds.
	If there is no solution, Handle is not changed and bb_probe fails.
	The primitive set_var_bounds/3 is used to impose cost bounds
	during the search process in a generic way."),
    see_also:[bb_init/2,bb_cost/2,bb_solution/2,bb_finish/1,bb_min/3,bb_min/6,
    	set_var_bounds/3],
    eg:"% a simple branch-and-bound procedure
my_minimize(Goal, Cost, Solution, OptCost, Module) :-
	bb_init(1000000, Handle),
	(
	    bb_delta(0, 1000000, Goal, Cost, Handle, Module)
	;
	    bb_solution(Handle, Solution),
	    bb_cost(Handle, OptCost)
	),
	bb_finish(Handle).

bb_delta(From, To, Goal, Cost, Handle, Module) :-
	bb_probe(From, To, Goal, Goal, Cost, Handle, Module),
	NewTo is bb_cost(Handle) - 1,
	bb_delta(From, NewTo, Goal, Cost, Handle, Module).
    "]).

