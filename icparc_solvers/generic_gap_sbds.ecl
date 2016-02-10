%
% Module implementing GAP-SBDS.
% WH, IPG and others
%
% $Id: generic_gap_sbds.ecl,v 1.3 2009/07/16 09:11:27 jschimpf Exp $
%

%
% Copyright (C) 2002-2004  The SBDS Group
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%

%
% This code is independent of the integer solver used; see fd_sbds.ecl and
% ic_sbds.ecl for the FD and IC wrappers, respectively.
%

:- comment(categories, ["Constraints"]).
:- comment(summary, "GAP-based Symmetry Breaking During Search (SBDS)").
:- comment(desc, html("\
   This library provides a GAP-based Symmetry Breaking During Search (SBDS)
   library, as described in:<P>

   Ian P. Gent, Warwick Harvey and Tom Kelsey.  \"Groups and Constraints:
   Symmetry Breaking during Search.\".  In P. Van Hentenryck, editor,
   <EM>Proceedings of the Eighth International Conference on Principles and
   Practice of Constraint Programming - CP 2002</EM>, LNCS 2470, pages
   415-430.  Springer-Verlag, 2002.<P>

   It has been enhanced by (amongst other things) being integrated with the
   symmetry expression library described in:<P>

   Warwick Harvey, Tom Kelsey and Karen Petrie.  \"Symmetry Group Expression
   for CSPs.\"  In Barbara Smith et al., editors, <EM>Proceedings of
   SymCon'03: Third International Workshop on Symmetry in Constraint
   Satisfaction Problems, a workshop of CP 2003</EM>, pages 86-96.
   September, 2003.<P>

   This library is still being developed and is expected to evolve.
   Feedback, suggestions, problem reports, etc. welcome.<P>

   Please note that this library currently does not work on Windows machines
   due to its dependence on the GAP interface library.<P>
")).
:- comment(see_also, [library(sym_expr), library(gap)]).
:- comment(author, "Warwick Harvey, Ian Gent (and probably others...)").
:- comment(status, evolving).


:- lib(gap).

:- export
	sbds_initialise/5,
	sbds_initialise_set/5,
	sbds_try/2,
	sbds_try/3,
	sbds_try_set/3,
	sbds_try_set/4,
	sbds_record/3,
	sbds_indomain/1,
	sbds_labeling/1,
	is_sbds_var/1,
	first_solution/1,
	all_solutions/1,
	all_solutions/2.


:- meta_attribute(sbds, [
	print:print_sbds/2,
	unify:unify_sbds/2
%	test_unify:test_unify_sbds/2,
%	suspensions:suspensions_sbds/3,
%	delayed_goals_number:delayed_goals_number_sbds/2,
%	copy_term:copy_term_sbds/2,
%	compare_instances:compare_instances_sbds/3
    ]).

	% Shared SBDS info (shared between a collection of variables).
:- local struct(sbds_shared(
	    array,	% array of SBDS variables
	    num_points,
	    depth,
	    rev_decision_info_list,
	    dim_factors,
	    val_offset,
	    module	% module to call fix, exclude & symmetry preds from
	)).

	% "Local" SBDS info (specific to this variable).
	% Note that a variable can have more than one index, if it appears
	% in the array multiple times (e.g. through unification).
:- local struct(sbds_local(
	    idx,	% index into array
	    shared	% shared SBDS info
	)).

:- local struct(decision_info(
	    point,
	    idx,
	    value,
	    right_transversal
	)).

%%%%%%%%%%%%%%%%%%%%% Initialise  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(sbds_initialise/5, [
	summary: "Initialises the data structures required by the SBDS algorithm",
	args: [
	    "Array": "Array of (integer) decision variables",
	    "VarDimNames": "Names for the dimensions of Array",
	    "ValueDimSpec": "Name of the value dimension and its range",
	    "SymSpecs": "List of symmetry specifiers",
	    "Options": "List of extra options"
	],
	amode: sbds_initialise(+, ++, ++, +, +),
	see_also: [sbds_initialise_set/5, construct_group/8,
		sbds_try/2, sbds_try/3],
	desc: html("\
   Sets up the data structures required to perform an SBDS search.  Array is
   an array containing the (integer) search variables, SymSpecs defines the
   symmetries of the problem, with VarDimNames and ValueDimSpec providing
   some extra required information; these four arguments are the same as the
   first four arguments of construct_group/8: please see the documentation
   for that predicate for a complete description of these arguments and
   details of the supported symmetry specifiers.<P>

   Options is a list of options which modify the way the search progresses.
   Currently no options are supported.<P>

   During search, use sbds_try/2 or sbds_try/3 on variables from Array when
   making search decisions.
")
    ]).

:- tool(sbds_initialise/5, sbds_initialise_body/6).

    /*
    ** Set up a array of SBDS variables, with the given symmetry, fix and
    ** exclude predicates, setting the given options.
    */
sbds_initialise_body(Array, VarDimNames, ValueDimSpec, SymSpecs, Options, Module) :-
	construct_group(Array, VarDimNames, ValueDimSpec, SymSpecs,
		"constructed_group", MaxPoints,
		generic_to_point(DimFactors, ValOffset), _, Module),
	sbds_initialise_body(Array, "constructed_group", MaxPoints,
		DimFactors, ValOffset, Options, Module).

sbds_initialise_body(Array, GroupId, MaxPoints, DimFactors, ValOffset,
		_Options, Module) :-
	Shared = sbds_shared with [
		array:Array,
		num_points:MaxPoints,
		depth:Depth,
		rev_decision_info_list:RevDecisionInfoList,
		dim_factors:DimFactors,
		val_offset:ValOffset,
		module:Module
	    ],
	Depth = 0,
	init_maxdepth,
	init_backtracks,
	RevDecisionInfoList = [],
	gap_command("sbds_stab_0 := %s", [GroupId]),
	% gap_query("LargestMovedPoint(%s)", [GroupId], NumPoints),
	% NumPoints is the worst case depth we can ever reach: if we reach
	% that depth, we must have explicitly decided for each point whether
	% it's "true" or "false".
	dim(Array, Dims),
	init_array(Array, Dims, [], Shared).

init_array(Var, Dims, Idx, Shared) :-
	Dims = [],
	( var(Var) ->
	    init_var(Var, Idx, Shared)
	;
	    true
	).
init_array(Array, Dims, Prefix, Shared) :-
	Dims = [Dim | Dims1],
	(
	    for(Idx, 1, Dim),
	    param(Array, Dims1, Prefix, Shared)
	do
	    append(Prefix, [Idx], Prefix1),
	    X is Array[Idx],
	    init_array(X, Dims1, Prefix1, Shared)
	).

to_point(Shared, Idx, Val, Point) :-
	Shared = sbds_shared with [
		dim_factors:DimFactors,
		val_offset:ValOffset
	    ],
	generic_to_point(DimFactors, ValOffset, Idx, Val, Point).

from_point(Shared, Point, Var, Val) :-
	Shared = sbds_shared with [
		array:Array,
		dim_factors:DimFactors,
		val_offset:ValOffset
	    ],
	generic_from_point(DimFactors, ValOffset, Array, Point, Var, Val).

%%%%%%%%%%%%%%%%%%%%% Initialise - set version %%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(sbds_initialise_set/5, [
	summary: "Initialises the data structures required by the SBDS algorithm (set version)",
	args: [
	    "Array": "Array of integer set decision variables",
	    "VarDimNames": "Names for the dimensions of Array",
	    "ValueDimSpec": "Name of the value dimension and its range",
	    "SymSpecs": "List of symmetry specifiers",
	    "Options": "List of extra options"
	],
	amode: sbds_initialise_set(+, ++, ++, +, +),
	see_also: [sbds_initialise/5, sbds_try_set/3, sbds_try_set/4],
	desc: html("\
   Like sbds_initialise/5, but for set models (i.e. Array should be an array
   of integer set variables).  The \"value\" dimension is the values that can 
   appear in the set, so that, for example, a range of 1..5 indicates that
   the upper bound of the set variables is [1,2,3,4,5].<P>

   See the documentation for sbds_initialise/5 for more details.

   During search, use sbds_try_set/3 or sbds_try_set/4 on variables from
   Array when making search decisions.
")
    ]).

:- tool(sbds_initialise_set/5, sbds_initialise_set_body/6).

sbds_initialise_set_body(Array, VarDimNames, ValueDimName:ValueSpec,
                SymSpecs, Options, Module) :-
        value_spec_to_range(ValueSpec, Lo, Hi),
        ( Lo =\= 1 ->
            printf(error, "Sorry, set elements must start with 1.%n", []),
            abort
        ;
            true
        ),
        append(VarDimNames, [ValueDimName], NewVarDimNames),
        dim(Array, Dims),
        map_sets_to_bools(Array, Dims, Hi, BoolArray),
        sbds_initialise_body(BoolArray, NewVarDimNames, set_in_not_in:0..1,
                SymSpecs, Options, Module).

map_sets_to_bools(Var, [], NBools, Bools) :-
        dim(Bools, [NBools]),
        set_module:membership_booleans(Var, Bools).
map_sets_to_bools(Array, [Dim | Dims], NBools, BoolArray) :-
        functor(BoolArray, [], Dim),
        (
            for(Idx, 1, Dim),
            param(Array, Dims, NBools, BoolArray)
        do
            X is Array[Idx],
            BX is BoolArray[Idx],
            map_sets_to_bools(X, Dims, NBools, BX)
        ).


%%%%%%%%%%%%%%%%%%%%% Make assignments  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get ready to assign Value to the attributed Var

:- comment(sbds_try/2, [
	summary: "Try assigning a value to an SBDS search variable",
	args: [
	    "Var": "The variable to use",
	    "Value": "The value to try"
	],
	amode: sbds_try(+, ++),
	see_also: [sbds_initialise/5, sbds_try/3],
	desc: html("\
   Make an SBDS search choice.  Specifically, try assigning the (integer)
   value Value to the variable Var; on backtracking exclude the value Value
   from the domain of Var.<P>

   Var should be an element of an array previously initialised using
   sbds_initialise/5.<P>

   Use sbds_try/3 if you wish to know which is the currently successful
   branch (Var #= Value or Var #\\= Value).
")
    ]).

:- comment(sbds_try/3, [ 
	summary: "Try assigning a value to an SBDS search variable",
	args: [
	    "Var": "The variable to use",
	    "Value": "The value to try",
	    "Success": "Whether the assignment succeeded or not"
	],
	amode: sbds_try(+, ++, ?),
	see_also: [sbds_initialise/5, sbds_try/2],
	desc: html("\
   Like sbds_try/2, but assign 1 to Success if we have set Var #= Value, and
   assign 0 if we have set Var #\\= Value (effectively, Success reflects the
   truth of Var #= Value when the predicate succeeds).<P>

   See the documentation for sbds_try/2 for more details.
")
    ]).

sbds_try(Var, Value) :-
	sbds_try(Var, Value, _).

sbds_try(Var{sbds:Attr}, Value, Success) :-
	-?->
	!,
	Attr = sbds_local with [idx:Idx, shared:Shared],
	count_backtracks,
	sbds_try_aux(Shared, Idx, Value, Depth, RevDecisionInfoList, Point),
	sbds_try1(Shared, Depth, RevDecisionInfoList, Point, Idx, Var,
		Value, Success).
sbds_try(Var, Value, Success) :-
	nonvar(Var),
	!,
	( Var == Value ->
	    Success = 1
	;
	    Success = 0
	).
sbds_try(Var, _Value, _Success) :-
	printf("%q is not an SBDS variable\n", [Var]),
	abort.

sbds_try_aux(Shared, Idx, Value, Depth, RevDecisionInfoList, Point) :-
	Shared = sbds_shared with [
		depth:Depth0,
		num_points:NumPoints,
		rev_decision_info_list:RevDecisionInfoList0
	    ],
	Depth is Depth0 + 1,
	update_maxdepth(Depth),
	to_point(Shared, Idx, Value, Point),
	% printf(log_output, "%d: Stabilising point %d (idx: %d; val: %q).%n",
	% 	[Depth, Point, Idx, Value]),
	gap_command("sbds_stab_%d := Stabilizer(sbds_stab_%d, %d)",
		[Depth, Depth0, Point]),
	gap_command("sbds_rt := AsList(RightTransversal(sbds_stab_%d, sbds_stab_%d))",
		[Depth0, Depth]),
	gap_command("sbds_rt_as_lists := List([1..Length(sbds_rt)], i -> List([1..%d], j -> j^sbds_rt[i]))",
		[NumPoints]),
	gap_query("sbds_rt_as_lists", [], RTAsLists),
	(
	    foreach(PermAsList, RTAsLists),
	    foreach(Perm, Perms)
	do
	    Perm =.. [[] | PermAsList]
	),
	RightTransversal =.. [[] | Perms],
	DecisionInfo = decision_info with [
		point:Point,
		idx:Idx,
		value:Value,
		right_transversal:RightTransversal
	    ],
	RevDecisionInfoList = [DecisionInfo | RevDecisionInfoList0].


    % Record that the variable at position Idx has been assigned Value as if
    % it were a decision.
sbds_record(Shared, Idx, Value) :-
	sbds_try_aux(Shared, Idx, Value, Depth, RevDecisionInfoList, _Point),
	setarg(depth of sbds_shared, Shared, Depth),
	setarg(rev_decision_info_list of sbds_shared, Shared, RevDecisionInfoList).


sbds_try1(Shared, Depth, RevDecisionInfoList, _Point, _Idx, Var, Value, 1) :-
	% Try making the assignment.
	Var #= Value,
	setarg(depth of sbds_shared, Shared, Depth),
	setarg(rev_decision_info_list of sbds_shared, Shared, RevDecisionInfoList).
sbds_try1(Shared, _Depth, RevDecisionInfoList, _Point, _Idx, _Var, _Value, 0) :-
	% Make the negation (and all its symmetric variants).
	% printf(log_output, "%d: Excluding point %d (idx: %d; val: %q).%n",
	% 	[Depth, Point, Idx, Value]),
	reverse(RevDecisionInfoList, [DecisionInfo | DecisionInfoList]),
	sbds_exclude(DecisionInfo, DecisionInfoList, [], 1, Shared),
	% printf(log_output, "%d: Finished excluding point %d.%n",
	% 	[Depth, Point]),
        true.


% XXX - need to do something about "visited"...

    % DecisionInfoList: Decision1, Decision2, ...
    % RevPermList: Perm i, Perm i-1, ...
sbds_exclude(DecisionInfo, [], RevPermList, _Depth0, Shared) :-
	!,
	DecisionInfo = decision_info with [
		point:Point,
		right_transversal:RightTransversal
	    ],
	(
	    foreacharg(Perm, RightTransversal),
	    %param(Depth0),
	    param(RevPermList, Shared, Point)
	do
	    compute_sym_point([Perm | RevPermList], Point, SymPoint),
	    from_point(Shared, SymPoint, SymVar, SymValue),
	    % printf(log_output, 
	    %	    "%d: Excluding point %d (var: %mw; val: %q).%n",
	    %	    [Depth0, SymPoint, SymVar, SymValue]),
	    SymVar #\= SymValue
	).
sbds_exclude(DecisionInfo, [DecisionInfo1 | DecisionInfoList], RevPermList, Depth0, Shared) :-
	DecisionInfo = decision_info with [
		point:Point,
		%idx:Idx,
		%value:Value,
		right_transversal:RightTransversal
	    ],
	% functor(RightTransversal, _, RTLen),
	% printf(log_output, 
	%	"%d: %d cosets of stab of point %d (idx: %d; val: %q).%n",
	%	 [Depth0, RTLen, Point, Idx, Value]),
	(
	    foreacharg(Perm, RightTransversal),
	    count(_I, 1, _),
	    param(DecisionInfo1, DecisionInfoList, RevPermList, Depth0),
	    param(Shared, Point)
	do
	    compute_sym_point([Perm | RevPermList], Point, SymPoint),
	    from_point(Shared, SymPoint, SymVar, SymValue),
	    #=(SymVar, SymValue, Bool),
	    % printf(log_output, 
	    %	    "%d: Coset %d maps pt to %d (var: %mw; val: %q): %mw.%n",
	    %	    [Depth0, I, SymPoint, SymVar, SymValue, Bool]),
	    check_exclude_or_delay(Bool, DecisionInfo1, DecisionInfoList,
		    [Perm | RevPermList], Depth0, Shared)   % debugged version
	).

check_exclude_or_delay(Bool, DecisionInfo, DecisionInfoList, RevPermList,
		Depth0, Shared) :-
	( var(Bool) ->
	    suspend(check_exclude_or_delay(Bool, DecisionInfo,
			    DecisionInfoList, RevPermList, Depth0, Shared),
		    3, [Bool->inst])
	; Bool == 0 ->
	    true
	;
	    Depth1 is Depth0 + 1,
	    sbds_exclude(DecisionInfo, DecisionInfoList, RevPermList,
		    Depth1, Shared)
	).


compute_sym_point([], SymPoint, SymPoint).
compute_sym_point([Perm | PermList], SymPoint0, SymPoint) :-
	SymPoint1 is Perm[SymPoint0],
	compute_sym_point(PermList, SymPoint1, SymPoint).


%%% Set-based try.

:- comment(sbds_try_set/3, [
	summary: "Try adding/excluding a value to/from an SBDS set search variable",
	args: [
	    "Var": "The set variable to use",
	    "Value": "The value to try",
	    "Bool": "Whether to add or exclude the value first"
	],
	amode: sbds_try_set(+, ++, ++),
	see_also: [sbds_initialise_set/5, sbds_try_set/4, sbds_try/2],
	desc: html("\
   Make an SBDS (set) search choice.  Specifically, if Bool is 1, try adding
   the (integer) value Value to the (set) variable Var; on backtracking
   exclude it.  If Bool is 0, try excluding Value first and try adding it on
   backtracking.<P>

   Var should be an element of an array previously initialised using
   sbds_initialise_set/5.<P>

   Use sbds_try_set/4 if you wish to know which is the currently successful
   branch (Value in Var or Value notin Var).
")
    ]).

:- comment(sbds_try_set/4, [
	summary: "Try adding/excluding a value to/from an SBDS set search variable",
	args: [
	    "Var": "The set variable to use",
	    "Value": "The value to try",
	    "Bool": "Whether to add or exclude the value first",
	    "Success": "Whether the assignment succeeded or not"
	],
	amode: sbds_try_set(+, ++, ++, ?),
	see_also: [sbds_initialise_set/5, sbds_try_set/3, sbds_try/3],
	desc: html("\
   Like sbds_try_set/3, but assign 1 to Success if we have Value in Var, and
   assign 0 if we have Value notin Var (effectively, Success reflects the
   truth of Value in Var when the predicate succeeds).<P>

   See the documentation for sbds_try_set/3 for more details.
")
    ]).

sbds_try_set(Var, Value, Bool) :-
        sbds_try_set(Var, Value, Bool, _).

sbds_try_set(Var, Value, Bool, Success) :-
        % XXX - Makes assumptions about the implementation of
        % membership_booleans...  :)
        ( var(Var) ->
            set_module:membership_booleans(Var, Bools),
            BoolVar is Bools[Value],
            sbds_try(BoolVar, Bool, Success)
        ;
            % Since Var is ground, we just need to check that the chosen
            % value is in/not in the set. 
            set_module:in(Value, Var, InNotIn),
            #=(InNotIn, Bool, Success)
        ).


%%%%%%%% SBDS Search

:- comment(sbds_labeling/1, [
	summary: "Instantiate all integer SBDS variables in a list to elements of their domains",
	args: [
	    "List": "A list of integer SBDS variables or integers"
	],
	amode: sbds_labeling(?),
	see_also: [labeling/1, sbds_indomain/1],
	desc: html("\
   Like labeling/1, but uses sbds_indomain/1 to do the instantiation so that
   the SBDS algorithms are invoked appropriately.
")
    ]).

:- comment(sbds_indomain/1, [
	summary: "Instantiate an integer SBDS variable to an element of its domain",
	args: [
	    "Var": "The (integer) SBDS variable to instantiate, or an integer"
	],
	amode: sbds_indomain(?),
	see_also: [indomain/1, sbds_try/2, sbds_labeling/1],
	desc: html("\
   Like indomain/1, but uses sbds_try/2 to do the instantiation so that the
   SBDS algorithms are invoked appropriately.
")
    ]).

sbds_labeling([]).
sbds_labeling([Var | Vars]) :-
	sbds_indomain(Var),
	sbds_labeling(Vars).

sbds_indomain(X) :-
	nonvar(X).
sbds_indomain(X) :-
	var(X),
	get_lwb(X, LWB),
	sbds_try(X, LWB),
	sbds_indomain(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    /*
    ** Initialise an SBDS variable with the given index and shared SBDS info.
    ** Note that a variable can be initialised more than once, as long as it
    ** belongs to the same SBDS collection each time.
    */
init_var(X{sbds:Attr}, Idx, Shared) :- -?->
	!,
	init_var1(X, Attr, Idx, Shared).
init_var(X, Idx, Shared) :-
	new_sbds_var(X, Idx, Shared).

init_var1(X, Attr, Idx, Shared) :-
	var(Attr),
	new_sbds_var(X, Idx, Shared).
init_var1(X, Attr, _Idx, Shared) :-
	nonvar(Attr),
	% Variable already has an SBDS attribute.
	Attr = sbds_local with [shared:Shared1],
	( Shared == Shared1 ->
	    true
	;
	    printf("Error: %q cannot belong to more than one SBDS collection\n", [X]),
	    abort
	).

new_sbds_var(X, Idx, Shared) :-
	Attr = sbds_local with [idx:Idx, shared:Shared],
	add_attribute(X, Attr, sbds),
	notify_constrained(X).


:- comment(is_sbds_var/1, [
	summary: "Checks whether a variable is an SBDS variable",
	args: [
	    "Var": "The variable to check"
	],
	amode: is_sbds_var(?),
	desc: html("\
   Succeeds iff Var is an SBDS variable (i.e. has been initialised through
   a call to sbds_initialise/5).
")
    ]).

is_sbds_var(_{sbds:(sbds_local with [])}) :- -?->
	true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_sbds(_{sbds:Attr}, Print) :- -?->
	nonvar(Attr),
	Attr = sbds_local with [idx:Idx],
	% Probably don't want to print the whole array for each variable...
	Print = sbds(Idx).

unify_sbds(_, YAttr) :-
	var(YAttr).
unify_sbds(X, YAttr) :-
	nonvar(YAttr),
	unify_any(X, YAttr).

unify_any(_{sbds:XAttr}, YAttr) :- -?->
	unify_meta(XAttr, YAttr).
unify_any(X, _) :-
	nonvar(X).

unify_meta(XAttr, YAttr) :-
	var(XAttr),
	XAttr = YAttr.
unify_meta(XAttr, YAttr) :-
	nonvar(XAttr),
	XAttr = sbds_local with [shared:XShared],
	YAttr = sbds_local with [shared:YShared],
	( XShared == YShared ->
	    true
	;
	    printf("Error: cannot unify variables from different SBDS collections\n", []),
	    abort
	).


%%%%%%%%%%%%%%%%%%%%%%% all solutions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(all_solutions/1, [
	summary: "Find all solutions to the given goal",
	args: [
	    "Goal": "The goal to execute"
	],
	amode: all_solutions(+),
	see_also: [all_solutions/2, first_solution/1],
	desc: html("\
   Finds all solutions for the goal Goal, and prints various statistics
   about the computation (CPU time taken, number of backtracks, etc.).
")
    ]).

:- comment(all_solutions/2, [
	summary: "Find all solutions to the given goal",
	args: [
	    "Goal": "The goal to execute",
	    "NSols": "The number of solutions found"
	],
	amode: all_solutions(+, ?),
	see_also: [all_solutions/1, first_solution/1],
	desc: html("\
   Finds all solutions for the goal Goal, and prints various statistics
   about the computation (CPU time taken, number of backtracks, etc.).  The
   number of solutions found is unified with NSols.
")
    ]).

:- tool(all_solutions/1, all_solutions_body/2).
:- tool(all_solutions/2, all_solutions_body/3).
:- tool(count_solutions/2, count_solutions_body/3).

all_solutions_body(Goal, Module) :-
	all_solutions_body(Goal, _NSols, Module).

all_solutions_body(Goal, NSols, Module) :-
	statistics(times, EclStartTimes),
	initial_sbds_stats(InitialStats),
	count_solutions(Goal, NSols)@Module,
	statistics(times, EclEndTimes),
	sbds_stats(FinalStats),
	print_stats(EclStartTimes, EclEndTimes, InitialStats, FinalStats),
	get_backtracks(Back),
	get_maxdepth(MaxDepth),
	( NSols == 1 ->
	    printf("%d backtracks, %d solution, max depth %d.%n",
		    [Back, NSols, MaxDepth])
	;
	    printf("%d backtracks, %d solutions, max depth %d.%n",
		    [Back, NSols, MaxDepth])
	).

%%%%%%%%%%%%%%%%%%%%%%% first solution %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(first_solution/1, [
	summary: "Find the first solution to the given goal",
	args: [
	    "Goal": "The goal to execute"
	],
	amode: first_solution(+),
	see_also: [all_solutions/1, all_solutions/2],
	desc: html("\
   Finds the first solution for the goal Goal, and prints various statistics
   about the computation (CPU time taken, number of backtracks, etc.).
")
    ]).

:- tool(first_solution/1, first_solution_body/2).

first_solution_body(Goal, Module) :-
	statistics(times, EclStartTimes),
	initial_sbds_stats(InitialStats),
	call(Goal)@Module,
	statistics(times, EclEndTimes),
	sbds_stats(FinalStats),
	print_stats(EclStartTimes, EclEndTimes, InitialStats, FinalStats),
	get_backtracks(Back),
	get_maxdepth(MaxDepth),
	printf("%d backtracks, max depth %d.%n", [Back, MaxDepth]).


%%%%%%%%%%%%%%%%%%%%%%%  Stats  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local struct(sbds_stats(
	    time,       % GAP time (milliseconds)
	    system_time,        % GAP system time (ms) from Gap 4.4
	    time_children,        % GAP user children time (ms) from Gap 4.4
	    system_time_children, % GAP system children time (ms) from Gap 4.4
	    time_flag  % 1 if 4.4 times available, else 0
	)).

sbds_stats(Stats) :-
	Stats = sbds_stats with [
		time:TimeMS,
		system_time:SysTimeMS,
		time_children:TimeChildrenMS,
		system_time_children:SysTimeChildrenMS,
		time_flag:Flag
	    ],
	gap_query("RuntimesList()", [], [TimeMS,SysTimeMS,TimeChildrenMS,SysTimeChildrenMS,Flag]).

initial_sbds_stats(Stats) :-
	Stats = sbds_stats with [
		time:TimeMS,
		system_time:SysTimeMS,
		time_children:TimeChildrenMS,
		system_time_children:SysTimeChildrenMS,
		time_flag:Flag
	    ],
	gap_query("RuntimesList()", [], [TimeMS,SysTimeMS,TimeChildrenMS,SysTimeChildrenMS,Flag]).
	%gap_query("Runtime()", [], TimeMS).

sbds_difference_stats(
		sbds_stats with [
			time:TimeMS0,
			system_time:SysTimeMS0,
			time_children:TimeChildrenMS0,
			system_time_children:SysTimeChildrenMS0,
			time_flag:Flag
		    ],
		sbds_stats with [
			time:TimeMS1,
			system_time:SysTimeMS1,
			time_children:TimeChildrenMS1,
			system_time_children:SysTimeChildrenMS1
		    ],
		sbds_stats with [
			time:TimeMS,
			system_time:SysTimeMS,
			time_children:TimeChildrenMS,
			system_time_children:SysTimeChildrenMS,
			time_flag:Flag
		    ]) :-
	TimeMS is TimeMS1 - TimeMS0,
	SysTimeMS is SysTimeMS1 - SysTimeMS0,
	TimeChildrenMS is TimeChildrenMS1 - TimeChildrenMS0,
	SysTimeChildrenMS is SysTimeChildrenMS1 - SysTimeChildrenMS0.

print_stats([EclStartTime,EclStartSystemTime,EclStartClockTime ],
	    [EclEndTime,EclEndSystemTime,EclEndClockTime ],
	    InitialStats, 
	    FinalStats) :-
	EclSec is EclEndTime - EclStartTime,
	EclSystemSec is EclEndSystemTime - EclStartSystemTime,
	EclClockSec is EclEndClockTime - EclStartClockTime,
	EclTotalSec is EclSystemSec+EclSec,
	sbds_difference_stats(InitialStats, FinalStats, Stats),
	Stats = sbds_stats with [
		time:TimeMS,
		system_time:SysTimeMS,
		time_children:TimeChildrenMS,
		system_time_children:SysTimeChildrenMS,
		time_flag:Flag
	    ],
	GapSec is TimeMS / 1000,
	GapSysSec is SysTimeMS / 1000,
	GapChildrenSec is TimeChildrenMS / 1000,
	GapSysChildrenSec is SysTimeChildrenMS / 1000,
	TotalSec is GapSec + EclSec,
	printf("GAP User CPU:%t%t%10.2f%n", [GapSec]),
	printf("ECLiPSe User CPU:%t%10.2f%n", [EclSec]),
	printf("Total User CPU:%t%t%10.2f%n---%n", [TotalSec]),
	(Flag == 1
	 ->
		printf("GAP System CPU:%t%t%10.2f%n", [GapSysSec]),
		printf("GAP Children CPU:%t%10.2f%n", [GapChildrenSec]),
		printf("GAP Children System CPU:%10.2f%n", [GapSysChildrenSec]),
		GapTotalSec is GapSec+GapSysSec+GapChildrenSec+GapSysChildrenSec
	 ;
		GapTotalSec is GapSec,
		printf("Gap System times unavailable -- consider upgrading to Gap 4.4%n", [])
	),
	printf("ECLiPSe System CPU:%t%10.2f%n---%n", [EclSystemSec]),
	printf("Gap Total CPU:%t%t%10.2f%n", [GapTotalSec]),
	GrandTotalSec is GapTotalSec+EclTotalSec, 
	printf("ECLiPSe Total CPU:%t%10.2f%n", [EclTotalSec]),
	printf("Grand Total CPU:%t%10.2f%n---%n", [GrandTotalSec]) ,
	printf("Elapsed time (wallclock):%10.2f%n---%n", [EclClockSec]).


:- local variable(backtracks), variable(deep_fail).

init_backtracks :-
        setval(backtracks,0).

get_backtracks(B) :-
        getval(backtracks,B).

count_backtracks :-
        setval(deep_fail,false).

count_backtracks :-
        getval(deep_fail,false),        % may fail 
        setval(deep_fail,true),
        incval(backtracks),
        fail.


:- local variable(maxdepth).

init_maxdepth :-
        setval(maxdepth,0).

get_maxdepth(D) :-
        getval(maxdepth,D).

update_maxdepth(D) :-
        getval(maxdepth,Max),
        ( D > Max ->
            setval(maxdepth,D)
        ;
            true
        ).


% From the ECLiPSe docs, but made into a tool.
count_solutions_body(Goal, Total, Module) :-
        init_backtracks,
        shelf_create(count(0), Shelf),
        (
            call(Goal)@Module,
            count_backtracks,
            shelf_inc(Shelf, 1),
            fail
        ;
            shelf_get(Shelf, 1, Total)
        ),
        shelf_abolish(Shelf).


%%%%%%%%%%%%%%%%%%%%%%%%%%% EOF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

