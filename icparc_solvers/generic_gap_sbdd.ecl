%
% Module implementing the ECLiPSe side of GAP-SBDD.
% See SBDDa.g for the GAP side.
% IPG, WH, TK, SAL
%
% $Id: generic_gap_sbdd.ecl,v 1.5 2013/02/13 00:58:47 jschimpf Exp $
%

%
% Copyright (C) 2003-2004  The SBDS Group
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
% This code independent of integer solver used; see sbdd.ecl and ic_sbdd.ecl
% for the FD and IC wrappers, respectively.
%

:- comment(categories, ["Constraints"]).
:- comment(summary, "GAP-based Symmetry Breaking via Dominance Detection (SBDD)").
:- comment(desc, html("\
   This library provides a GAP-based Symmetry Breaking via Dominance
   Detection (SBDD) library, as described in:<P>

   Ian P. Gent, Warwick Harvey, Tom Kelsey and Steve Linton.  \"Generic SBDD
   using Computational Group Theory\".  In Francesca Rossi, editor,
   <EM>Proceedings of the Ninth International Conference on Principles and
   Practice of Constraint Programming - CP 2003</EM>, LNCS 2833, pages
   333-347.  Springer-Verlag, 2003.<P>

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
:- comment(author, "Warwick Harvey, Steve Linton, Tom Kelsey, Ian Gent (and probably others...)").
:- comment(status, evolving).


:- lib(lists).
:- lib(gap).
:- lib(config_opts).

%%%%%%%%%%%%%%%%%%%%% Module Components %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export
	sbdd_initialise/5,
	sbdd_initialise_set/5,
	sbdd_try/2,
	sbdd_try/3,
	sbdd_record/3,
	sbdd_try_set/3,
	sbdd_try_set/4,
	throttle_default/1,
	throttle_depth_unique/2,
	throttle_depth_non_unique/2,
	throttle_depth_mod_unique/2,
	throttle_depth_mod_non_unique/2,
	sbdd_indomain/1,
	sbdd_recorded_indomain/3,
	sbdd_labeling/1,
	is_sbdd_var/1,
	first_solution/1,
	all_solutions/1,
	all_solutions/2.


:- meta_attribute(sbdd, [
	print:print_sbdd/2,
	unify:unify_sbdd/2
    ]).

	 % Shared SBDD info 
:- export struct(sbdd_shared(
	array,		% variable array 
	depth,		% depth in search tree
	dec_info_list,	% partial assignment
	dim_factors,	% for converting between var/val and point
	val_offset,	% for converting between var/val and point
	ground_points,	% all current Var = Val assignments
	throttle_pred,	% control dominance checks
	module		% calling module
    )).

:- comment(struct(sbdd_shared), [
	summary: "Structure containing information for GAP-SBDD search",
	fields: [
		array: "Array of decision variables",
		depth: "Current depth of search tree",
		dec_info_list: "",
		dim_factors: "",
		val_offset: "",
		ground_points: "",
		throttle_pred: "",
		module: ""
	    ],
	desc: html("\
   Most of the contents of this structure are subject to change in future
   versions of the library and thus should not be accessed by user-level
   code.  The user should also not change the contents of this structure!<P>
")	% XXX - document this better.
    ]).

        % "Local" SBDD info (specific to this variable).
:- local struct(sbdd_local(
	idx,	% index position in array
	shared	% shared SBDD info
    )).

:- local struct(dec_info(
	point,
	idx,
	value
    )).

    % Record which directory this file resides in, since we want to look for
    % GAP code in the same directory.
:- local variable(src_dir).
:- get_flag(cwd, SrcDir), setval(src_dir, SrcDir).

%%%%%%%%%%%%%%%%%%%%% Initialise  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(sbdd_initialise/5, [
	summary: "Initialises the data structures required by the SBDD algorithm",
	args: [
	    "Array": "Array of (integer) decision variables",
	    "VarDimNames": "Names for the dimensions of Array",
	    "ValueDimSpec": "Name of the value dimension and its range",
	    "SymSpecs": "List of symmetry specifiers",
	    "Options": "List of extra options"
	],
	amode: sbdd_initialise(+, ++, ++, +, +),
	see_also: [sbdd_initialise_set/5, construct_group/8,
		struct(sbdd_shared), sbdd_try/2, sbdd_try/3],
	desc: html("\
   Sets up the data structures required to perform an SBDD search.  Array is
   an array containing the (integer) search variables, SymSpecs defines the
   symmetries of the problem, with VarDimNames and ValueDimSpec providing
   some extra required information; these four arguments are the same as the
   first four arguments of construct_group/8: please see the documentation
   for that predicate for a complete description of these arguments and
   details of the supported symmetry specifiers.<P>

   Options is a list of options which modify the way the search progresses.
   Currently only one option is supported: throttle(ThrottlePred).  This
   option can be used to control whether or not the SBDD dominance check is
   invoked at any given node.  At each node, before performing the dominance
   check, ThrottlePred is called with an extra argument appended, and if the
   call fails, the dominance check is skipped.  The extra argument is the
   sbdd_shared data structure, which holds the current state of the SBDD
   search; its fields can be examined (e.g. the current search depth) in
   order to decide whether to proceed with the dominance check or not.<P>

   During search, use sbdd_try/2 or sbdd_try/3 on variables from Array when
   making search decisions.
")
    ]).

:- tool(sbdd_initialise/5, sbdd_initialise_body/6).

sbdd_initialise_body(Array, VarDimNames, ValueDimSpec, SymSpecs, Options, Module) :-
	construct_group(Array, VarDimNames, ValueDimSpec, SymSpecs,
		"constructed_group", MaxPoints,
		generic_to_point(DimFactors, ValOffset), _, Module),
	% This information useful when visualising dominance checks.
	% XXX - should only print it if needed.
	printf(log_output, "Dim factors: %Dw; Val offset: %d.%n",
		[DimFactors, ValOffset]),
	sbdd_initialise_body(Array, "constructed_group", MaxPoints,
		DimFactors, ValOffset, Options, Module).

sbdd_initialise_body(Array, GroupId, MaxPoints, DimFactors, ValOffset, Options,
			Module) :-
	Shared = sbdd_shared with [
		array:Array,
		depth:Depth,
		dec_info_list:DecInfoList,
		dim_factors:DimFactors,
		val_offset:ValOffset,
		ground_points:PointSet, 
		throttle_pred:throttle_default,
		module:Module
	    ],
	process_options(Options, Shared),
	Depth = 1,
	init_maxdepth,
	init_backtracks,
	DecInfoList = [],
	getval(src_dir, SrcDir),
	gap_command("Read(\"%s/SBDDa.g\")", [SrcDir]),
	gap_command("SBDDInit(%s, %d)", [GroupId, MaxPoints]),%% Supplied by driver code
	dim(Array, Dims),
	init_array(Array, Dims, [], Shared, [], PointSet).

init_array(Var, [], Idx, Shared, PointSetIn, PointSetOut) :-
	( var(Var) ->
	    init_var(Var, Idx, Shared),
	    % add ground vars only when they become ground
	    suspend(add_new_ground_point(Idx, Var, Shared), 0, [Var->inst]),
	    PointSetOut = PointSetIn
	;
	    to_point(Shared, Idx, Var, Point),
	    PointSetOut = [Point | PointSetIn]
	).
init_array(Array, [Dim | Dims], Prefix, Shared, PointSetIn, PointSetOut) :-
	(
	    for(Idx, 1, Dim),
	    fromto(PointSetIn, PointSetIn, PointSetOut, PointSetOut),
	    param(Array, Dims, Prefix, Shared)
	do
	    append(Prefix, [Idx], Prefix1),
	    X is Array[Idx],
	    init_array(X, Dims, Prefix1, Shared, PointSetIn, PointSetOut)
	).

to_point(Shared, Idx, Val, Point) :-
	Shared = sbdd_shared with [
		dim_factors:DimFactors,
		val_offset:ValOffset
	    ],
	generic_to_point(DimFactors, ValOffset, Idx, Val, Point).

from_point(Shared, Point, Var, Val) :-
	Shared = sbdd_shared with [
		array:Array,
		dim_factors:DimFactors,
		val_offset:ValOffset
	    ],
	generic_from_point(DimFactors, ValOffset, Array, Point, Var, Val).

% Adds a new ground point to the list of ground points in Shared.
add_new_ground_point(Idx, Val, Shared) :-
	Shared = sbdd_shared with [
		ground_points:Points
	    ],
	to_point(Shared, Idx, Val, Point),
	setarg(ground_points of sbdd_shared, Shared, [Point | Points]).

%%%%%%%%%%%%%%%%%%%%% Initialise - set version %%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(sbdd_initialise_set/5, [
	summary: "Initialises the data structures required by the SBDD algorithm (set version)",
	args: [
	    "Array": "Array of integer set decision variables",
	    "VarDimNames": "Names for the dimensions of Array",
	    "ValueDimSpec": "Name of the value dimension and its range",
	    "SymSpecs": "List of symmetry specifiers",
	    "Options": "List of extra options"
	],
	amode: sbdd_initialise_set(+, ++, ++, +, +),
	see_also: [sbdd_initialise/5, sbdd_try_set/3, sbdd_try_set/4],
	desc: html("\
   Like sbdd_initialise/5, but for set models (i.e. Array should be an array
   of integer set variables).  The \"value\" dimension is the values that can 
   appear in the set, so that, for example, a range of 1..5 indicates that
   the upper bound of the set variables is [1,2,3,4,5].<P>

   See the documentation for sbdd_initialise/5 for more details.

   During search, use sbdd_try_set/3 or sbdd_try_set/4 on variables from
   Array when making search decisions.
")
    ]).

:- tool(sbdd_initialise_set/5, sbdd_initialise_set_body/6).

sbdd_initialise_set_body(Array, VarDimNames, ValueDimName:ValueSpec,
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
	sbdd_initialise_body(BoolArray, NewVarDimNames, set_in_not_in:0..1,
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


process_options(Options, Shared) :-
	(
	    foreach(Option, Options),
	    param(Shared)
	do
	    process_option(Option, Shared)
	).

process_option(throttle(ThrottlePred), Shared) ?- !,
	setarg(throttle_pred of sbdd_shared, Shared, ThrottlePred).
process_option(Option, _) :-
	printf("Unrecognised SBDD option %q\n", [Option]),
	abort.


%%%%%%%%%%%%%%%%%%%%% Make assignments  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get ready to assign Value to the attributed Var

:- comment(sbdd_try/2, [
	summary: "Try assigning a value to an SBDD search variable",
	args: [
	    "Var": "The variable to use",
	    "Value": "The value to try"
	],
	amode: sbdd_try(+, ++),
	see_also: [sbdd_initialise/5, sbdd_try/3],
	desc: html("\
   Make an SBDD search choice.  Specifically, try assigning the (integer)
   value Value to the variable Var; on backtracking exclude the value Value
   from the domain of Var.<P>

   Var should be an element of an array previously initialised using
   sbdd_initialise/5.<P>

   Use sbdd_try/3 if you wish to know which is the currently successful
   branch (Var #= Value or Var #\\= Value).
")
    ]).

:- comment(sbdd_try/3, [
	summary: "Try assigning a value to an SBDD search variable",
	args: [
	    "Var": "The variable to use",
	    "Value": "The value to try",
	    "Success": "Whether the assignment succeeded or not"
	],
	amode: sbdd_try(+, ++, ?),
	see_also: [sbdd_initialise/5, sbdd_try/2],
	desc: html("\
   Like sbdd_try/2, but assign 1 to Success if we have set Var #= Value, and
   assign 0 if we have set Var #\\= Value (effectively, Success reflects the
   truth of Var #= Value when the predicate succeeds).<P>

   See the documentation for sbdd_try/2 for more details.
")
    ]).

sbdd_try(Var, Value) :-
	sbdd_try(Var, Value, _).

sbdd_try(Var{sbdd:Attr}, Value, Success) :-
	-?->
	!,
	Attr = sbdd_local with [idx:Idx, shared:Shared],
	Shared = sbdd_shared with [
		depth:Depth0,               %% depth and...
		dec_info_list:DecInfo0      %% ...partial assignment...
	    ],
	Depth is Depth0 + 1,                %% increment depth
	update_maxdepth(Depth),
	to_point(Shared, Idx, Value, Point),
	DecInfo = dec_info with [
		point:Point,
		idx:Idx,
		value:Value
	    ],
	DecInfoList = [DecInfo | DecInfo0], %% update partial assignment
	count_backtracks,
	sbdd_try1(Shared, Depth, DecInfoList, Var, Value, Success).
sbdd_try(Var, Value, Success) :-
	nonvar(Var),         %% only assign vals to variables
	!,
	( Var == Value ->
	    Success = 1
	;
	    Success = 0
	).
sbdd_try(Var, _Value, _Success) :-
	printf("%q is not an SBDD variable\n", [Var]),
	abort.                %% don't do anything to non-SBDD variables


    % Record that the variable at position Idx has been assigned Value as if
    % it were a decision.
sbdd_record(Shared, Idx, Value) :-
	Shared = sbdd_shared with [
		depth:Depth0,               %% depth and...
		dec_info_list:DecInfo0      %% ...partial assignment...
	    ],
	Depth is Depth0 + 1,                %% increment depth
	update_maxdepth(Depth),
	to_point(Shared, Idx, Value, Point),
	DecInfo = dec_info with [
		point:Point,
		idx:Idx,
		value:Value
	    ],
	DecInfoList = [DecInfo | DecInfo0], %% update partial assignment
	setarg(depth of sbdd_shared, Shared, Depth),
	setarg(dec_info_list of sbdd_shared, Shared, DecInfoList).


%%%%%%%%% First try Var = Val
sbdd_try1(Shared, Depth, DecInfoList, Var, Value, 1) :-
	Var #= Value,
	setarg(depth of sbdd_shared, Shared, Depth),
	setarg(dec_info_list of sbdd_shared, Shared, DecInfoList),
	sbdd_check_remove(Shared).	%% Dominance check

%%%%%  Called if we backtracked from Var = Val
sbdd_try1(Shared, Depth, DecInfoList, Var, Value, 0) :-
	% Tell Gap about the backtrack at depth Depth
	(
	    foreach(DecisionInfo,DecInfoList),
	    foreach(Pt,DecPointSet)
	do
	    DecisionInfo = dec_info with [point:Pt]
	),
	RealDepth = Depth, % or length(DecPointSet,RealDepth)
	DecPointSet = [Choice|Node],
	( Node == [] ->
	    gap_command("BacktrackRoot(%Dw,%d)",[DecPointSet,RealDepth])
	;
	    gap_command("Backtrack(%d,%d,%Dw)",[Choice,RealDepth,Node])
	),
	DecInfoList = [_|BackDecInfoList],
	setarg(depth of sbdd_shared, Shared, Depth),
	setarg(dec_info_list of sbdd_shared, Shared, BackDecInfoList),
	Var #\= Value,
	sbdd_check_remove(Shared).	%% Check for dominance


%%%%%%%%%%%%%%%  Check for dominance %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%% If it succeeds we backtrack
%%% If it fails we remove 'near misses' from value domains

sbdd_check_remove(Shared) :-
%         sbdd_check(Shared,Removals),  %% Removals are the near misses...  
	Shared = sbdd_shared with [
		array:Array,
		ground_points:PointSet
	    ],
	dim(Array, Dims),
	array_to_domain_points(Array, Dims, [], Shared, DomPoints, []),
	sbdd_check(Shared, Removals, PointSet, DomPoints),
	(
	    foreach(SymPt,Removals),
	    param(Shared)
	do
	    from_point(Shared, SymPt, SymVar, SymValue),
	    SymVar #\= SymValue
	),
	% Now going to check if this has made everything ground 
	% If so check it is not a duplicate solution
	( Removals = [] ->       % if no removals, no chance of this case
	    true
	;
	    ( ground(Array) ->
		% Because the array is ground, PointSet and DomPoints are
		% the same...  No they're not - one is a list, one is a list
		% of lists.  ;)
		Shared = sbdd_shared with [ground_points:PointSet1],
		array_to_domain_points(Array, Dims, [], Shared, DomPoints1, []),
		sbdd_check(Shared, _, PointSet1, DomPoints1)
	    ;
		true
	    )
	).


%%% Set-based try.

:- comment(sbdd_try_set/3, [
	summary: "Try adding/excluding a value to/from an SBDD set search variable",
	args: [
	    "Var": "The set variable to use",
	    "Value": "The value to try",
	    "Bool": "Whether to add or exclude the value first"
	],
	amode: sbdd_try_set(+, ++, ++),
	see_also: [sbdd_initialise_set/5, sbdd_try_set/4, sbdd_try/2],
	desc: html("\
   Make an SBDD (set) search choice.  Specifically, if Bool is 1, try adding
   the (integer) value Value to the (set) variable Var; on backtracking
   exclude it.  If Bool is 0, try excluding Value first and try adding it on
   backtracking.<P>

   Var should be an element of an array previously initialised using
   sbdd_initialise_set/5.<P>

   Use sbdd_try_set/4 if you wish to know which is the currently successful
   branch (Value in Var or Value notin Var).
")
    ]).

:- comment(sbdd_try_set/4, [
	summary: "Try adding/excluding a value to/from an SBDD set search variable",
	args: [
	    "Var": "The set variable to use",
	    "Value": "The value to try",
	    "Bool": "Whether to add or exclude the value first",
	    "Success": "Whether the assignment succeeded or not"
	],
	amode: sbdd_try_set(+, ++, ++, ?),
	see_also: [sbdd_initialise_set/5, sbdd_try_set/3, sbdd_try/3],
	desc: html("\
   Like sbdd_try_set/3, but assign 1 to Success if we have Value in Var, and
   assign 0 if we have Value notin Var (effectively, Success reflects the
   truth of Value in Var when the predicate succeeds).<P>

   See the documentation for sbdd_try_set/3 for more details.
")
    ]).

sbdd_try_set(Var, Value, Bool) :-
	sbdd_try_set(Var, Value, Bool, _).

sbdd_try_set(Var, Value, Bool, Success) :-
	% XXX - Makes assumptions about the implementation of
	% membership_booleans...  :)
	( var(Var) ->
	    set_module:membership_booleans(Var, Bools),
	    BoolVar is Bools[Value],
	    sbdd_try(BoolVar, Bool, Success)
	;
	    % Since Var is ground, we just need to check that the chosen
	    % value is in/not in the set.
	    set_module:in(Value, Var, InNotIn),
	    #=(InNotIn, Bool, Success)
	).


%%%%%%%% SBDD Search

:- comment(sbdd_labeling/1, [
	summary: "Instantiate all integer SBDD variables in a list to elements of their domains",
	args: [
	    "List": "A list of integer SBDD variables or integers"
	],
	amode: sbdd_labeling(?),
	see_also: [labeling/1, sbdd_indomain/1],
	desc: html("\
   Like labeling/1, but uses sbdd_indomain/1 to do the instantiation so that
   the SBDD dominance checks are performed appropriately.
")
    ]).

:- comment(sbdd_indomain/1, [
	summary: "Instantiate an integer SBDD variable to an element of its domain",
	args: [
	    "Var": "The (integer) SBDD variable to instantiate, or an integer"
	],
	amode: sbdd_indomain(?),
	see_also: [indomain/1, sbdd_try/2, sbdd_labeling/1],
	desc: html("\
   Like indomain/1, but uses sbdd_try/2 to do the instantiation so that the
   SBDD dominance checks are performed appropriately.
")
    ]).

sbdd_labeling([]).
sbdd_labeling([Var | Vars]) :-
	sbdd_indomain(Var),
	sbdd_labeling(Vars).

sbdd_indomain(X) :-
	nonvar(X).
sbdd_indomain(X) :-
	var(X),
	get_lwb(X, LWB),
	sbdd_try(X, LWB),
	sbdd_indomain(X).

sbdd_recorded_indomain(Shared, Idx, X) :-
	nonvar(X),
	sbdd_record(Shared, Idx, X).
sbdd_recorded_indomain(Shared, Idx, X) :-
	var(X),
	get_lwb(X, LWB),
	sbdd_try(X, LWB, Success),
	( Success == 1 ->
	    true
	;
	    sbdd_recorded_indomain(Shared, Idx, X)
	).


%%%%%%%%% Throttling preds

:- comment(throttle_default/1, [
	summary: "Default throttling predicate",
	args: [
	    "SBDDInfo": "The sbdd_shared data structure"
	],
	amode: throttle_default(+),
	see_also: [sbdd_initialise/5, sbdd_initialise_set/5,
		throttle_depth_unique/2, throttle_depth_non_unique/2,
		throttle_depth_mod_unique/2, throttle_depth_mod_non_unique/2],
	desc: html("\
   Default throttling predicate used if none is given to sbdd_initialise/5
   or sbdd_initialise_set/5.  This predicate simply succeeds, meaning that
   a dominance check is performed at every node in the search tree.
")
    ]).

throttle_default(_Shared) :-
	true.

:- comment(throttle_depth_unique/2, [
	summary: "Example throttling predicate",
	args: [
	    "MaxDepth": "Cut-off depth",
	    "SBDDInfo": "The sbdd_shared data structure"
	],
	amode: throttle_depth_unique(++, +),
	see_also: [sbdd_initialise/5, sbdd_initialise_set/5,
		throttle_default/1, throttle_depth_non_unique/2,
		throttle_depth_mod_unique/2, throttle_depth_mod_non_unique/2],
	desc: html("\
   Throttling predicate for use as an option to sbdd_initialise/5
   or sbdd_initialise_set/5.  This predicate succeeds if the search depth
   does not exceed MaxDepth, or if the variable array is ground (ensuring
   solutions are guaranteed to be unique).
")
    ]).

throttle_depth_unique(MaxDepth, Shared) :-
	Shared = sbdd_shared with [array:Array, depth:Depth],
	( Depth =< MaxDepth ->
	    true
	;
	    ground(Array)
	).

:- comment(throttle_depth_non_unique/2, [
	summary: "Example throttling predicate",
	args: [
	    "MaxDepth": "Cut-off depth",
	    "SBDDInfo": "The sbdd_shared data structure"
	],
	amode: throttle_depth_non_unique(++, +),
	see_also: [sbdd_initialise/5, sbdd_initialise_set/5,
		throttle_default/1, throttle_depth_unique/2,
		throttle_depth_mod_unique/2, throttle_depth_mod_non_unique/2],
	desc: html("\
   Throttling predicate for use as an option to sbdd_initialise/5
   or sbdd_initialise_set/5.  This predicate succeeds if the search depth
   does not exceed MaxDepth.  Note that this means that solutions found are
   not guaranteed to be unique.
")
    ]).

throttle_depth_non_unique(MaxDepth, Shared) :-
	Shared = sbdd_shared with [depth:Depth],
	Depth =< MaxDepth.

:- comment(throttle_depth_mod_unique/2, [
	summary: "Example throttling predicate",
	args: [
	    "DepthModulus": "Depth modulus",
	    "SBDDInfo": "The sbdd_shared data structure"
	],
	amode: throttle_depth_mod_unique(++, +),
	see_also: [sbdd_initialise/5, sbdd_initialise_set/5,
		throttle_default/1, throttle_depth_unique/2,
		throttle_depth_non_unique/2, throttle_depth_mod_non_unique/2],
	desc: html("\
   Throttling predicate for use as an option to sbdd_initialise/5
   or sbdd_initialise_set/5.  This predicate succeeds if the search depth is
   a multiple of DepthModulus, or if the variable array is ground (ensuring
   solutions are guaranteed to be unique).
")
    ]).

throttle_depth_mod_unique(Mod, Shared) :-
	Shared = sbdd_shared with [array:Array, depth:Depth],
	( Depth mod Mod =:= 0 ->
	    true
	;
	    ground(Array)
	).

:- comment(throttle_depth_mod_non_unique/2, [
	summary: "Example throttling predicate",
	args: [
	    "DepthModulus": "Depth modulus",
	    "SBDDInfo": "The sbdd_shared data structure"
	],
	amode: throttle_depth_mod_non_unique(++, +),
	see_also: [sbdd_initialise/5, sbdd_initialise_set/5,
		throttle_default/1, throttle_depth_unique/2,
		throttle_depth_non_unique/2, throttle_depth_mod_unique/2],
	desc: html("\
   Throttling predicate for use as an option to sbdd_initialise/5
   or sbdd_initialise_set/5.  This predicate succeeds if the search depth is
   a multiple of DepthModulus.  Note that this means that solutions found
   are not guaranteed to be unique.
")
    ]).

throttle_depth_mod_non_unique(Mod, Shared) :-
	Shared = sbdd_shared with [depth:Depth],
	Depth mod Mod =:= 0.


%%%%%%%%% Misc preds

sbdd_check(Shared, Removals, PointSet, Doms) :-
	( PointSet = [Pt | Pts] ->
	    Shared = sbdd_shared with [throttle_pred:ThrottlePred, module:Module],
	    ( call(ThrottlePred, Shared)@Module ->
		gap_query("CheckOptionList(%Dw,%d,%Dw)", [Pts, Pt, Doms], Res),
		Res \== false,
		( Res = [true|Rest] ->
		    Removals = Rest
		;
		    Removals = []
		)
	    ;
		gap_command("MaintainOptionList(%Dw,%d,%Dw)", [Pts, Pt, Doms]),
		Removals = []
	    )
	;
	    Removals = []
	).

all_domains_unit([]).
all_domains_unit([[_]|Rest]) :- all_domains_unit(Rest).

array_to_domain_points(Var, [], Idx, Shared, [DomPoints | DomPointsTail],
		DomPointsTail) :-
	var_to_dom_list(Var, Dom),
	(
	    foreach(Val, Dom),
	    foreach(Point, DomPoints),
	    param(Idx, Shared)
	do
	    to_point(Shared, Idx, Val, Point)
	).
array_to_domain_points(Array, [Dim | Dims], Prefix, Shared,
		DomPoints, DomPointsTail) :-
	(
	    for(Idx, 1, Dim),
	    fromto(DomPoints, DomPoints, DomPointsTail, DomPointsTail),
	    param(Array, Dims, Prefix, Shared)
	do
	    append(Prefix, [Idx], Prefix1),
	    X is Array[Idx],
	    array_to_domain_points(X, Dims, Prefix1, Shared,
		    DomPoints, DomPointsTail)
	).

%%%%%%%%%%%%%%%% Init. toolbox %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
** Initialise an SBDD variable with the given index and shared SBDD info.
** Note that a variable can be initialised more than once, as long as it
** belongs to the same SBDD collection each time.
*/
init_var(X{sbdd:Attr}, Idx, Shared) :- -?->
	 !,
	 init_var1(X, Attr, Idx, Shared).
init_var(X, Idx, Shared) :-
	 new_sbdd_var(X, Idx, Shared).

init_var1(X, Attr, Idx, Shared) :-
	 var(Attr),
	 new_sbdd_var(X, Idx, Shared).
init_var1(X, Attr, _Idx, Shared) :-
	 nonvar(Attr),
	 % Variable already has an SBDD attribute.
	 Attr = sbdd_local with [shared:Shared1],
	 ( Shared == Shared1 ->
	     true
	 ;
	     printf("Error: %q cannot belong to more than one SBDD
collection\n", [X]),
	     abort
	 ).

new_sbdd_var(X, Idx, Shared) :-
	 Attr = sbdd_local with [idx:Idx, shared:Shared],
	 add_attribute(X, Attr, sbdd),
	 notify_constrained(X).


:- comment(is_sbdd_var/1, [
        summary: "Checks whether a variable is an SBDD variable",
        args: [
            "Var": "The variable to check"
        ],
        amode: is_sbdd_var(?),
        desc: html("\
   Succeeds iff Var is an SBDD variable (i.e. has been initialised through
   a call to sbdd_initialise/5).
")
    ]).

is_sbdd_var(_{sbdd:(sbdd_local with [])}) :- -?->
        true.


%%%%%%%%%%%%%%%%%%%%%% Print and unify attributed variables %%%%%%%%%%%

print_sbdd(_{sbdd:Attr}, Print) :- -?->
	 nonvar(Attr),
	 Attr = sbdd_local with [idx:Idx],
	 % don't want to print the whole array for each variable...
	 Print = sbdd(Idx).

unify_sbdd(_, YAttr) :-
	 var(YAttr).
unify_sbdd(X, YAttr) :-
	 nonvar(YAttr),
	 unify_any(X, YAttr).

unify_any(_{sbdd:XAttr}, YAttr) :- -?->
	 unify_meta(XAttr, YAttr).
unify_any(X, _) :-
	 nonvar(X).

unify_meta(XAttr, YAttr) :-
	 var(XAttr),
	 XAttr = YAttr.
unify_meta(XAttr, YAttr) :-
	 nonvar(XAttr),
	 XAttr = sbdd_local with [shared:XShared],
	 YAttr = sbdd_local with [shared:YShared],
	 ( XShared == YShared ->
	     true
	 ;
	     printf("Error: cannot unify variables from different SBDD
collections\n", []),
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
   about the computation (CPU time taken, number of dominance checks
   performed, etc.).
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
   about the computation (CPU time taken, number of dominance checks
   performed, etc.).  The number of solutions found is unified with NSols.
")
    ]).

:- tool(all_solutions/1, all_solutions_body/2).
:- tool(all_solutions/2, all_solutions_body/3).
:- tool(count_solutions/2, count_solutions_body/3).

all_solutions_body(Goal, Module) :-
	all_solutions_body(Goal, _NSols, Module).

all_solutions_body(Goal, NSols, Module) :-
	statistics(times, EclStartTimes),
	initial_sbdd_stats(InitialStats),
	count_solutions(Goal, NSols)@Module,
	statistics(times, EclEndTimes),
	sbdd_stats(FinalStats),
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
   about the computation (CPU time taken, number of dominance checks
   performed, etc.).
")
    ]).

:- tool(first_solution/1, first_solution_body/2).

first_solution_body(Goal, Module) :-
	statistics(times, EclStartTimes),
	initial_sbdd_stats(InitialStats),
	call(Goal)@Module,
	statistics(times, EclEndTimes),
	sbdd_stats(FinalStats),
	print_stats(EclStartTimes, EclEndTimes, InitialStats, FinalStats),
	get_backtracks(Back),
	get_maxdepth(MaxDepth),
	printf("%d backtracks, max depth %d.%n", [Back, MaxDepth]).


%%%%%%%%%%%%%%%%%%%%%%%  Stats  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local struct(sbdd_stats(
	    time,	% GAP time (milliseconds)
            system_time,        % GAP system time (ms) from Gap 4.4
            time_children,        % GAP user children time (ms) from Gap 4.4
            system_time_children, % GAP system children time (ms) from Gap 4.4
            time_flag,  % 1 if 4.4 times available, else 0
	    dom_dom,	% Num successful dominance checks
	    dom_no_del,	% Num unsuccessful checks with no dom dels
	    dom_del,	% Num unsuccessful checks with dom dels
	    dom_orbit	% Num of skipped checks due to orbit test
	)).

sbdd_stats(Stats) :-
	Stats = sbdd_stats with [
		time:TimeMS,
                system_time:SysTimeMS,
                time_children:TimeChildrenMS,
                system_time_children:SysTimeChildrenMS,
                time_flag:Flag,
		dom_dom:DomDom,
		dom_no_del:DomNoDel,
		dom_del:DomDel,
		dom_orbit:DomOrbit
	    ],
	gap_query("RuntimesList()", [], [TimeMS,SysTimeMS,TimeChildrenMS,SysTimeChildrenMS,Flag]),
	gap_query("DominanceStats", [], [DomDom, DomNoDel, DomDel, DomOrbit]).

initial_sbdd_stats(Stats) :-
	Stats = sbdd_stats with [
		time:TimeMS,
                system_time:SysTimeMS,
                time_children:TimeChildrenMS,
                system_time_children:SysTimeChildrenMS,
                time_flag:Flag,
		dom_dom:0,
		dom_no_del:0,
		dom_del:0,
		dom_orbit:0
	    ],
	gap_query("RuntimesList()", [], [TimeMS,SysTimeMS,TimeChildrenMS,SysTimeChildrenMS,Flag]).
	%gap_query("Runtime()", [], TimeMS).

sbdd_difference_stats(
		sbdd_stats with [
			time:TimeMS0,
                        system_time:SysTimeMS0,
                        time_children:TimeChildrenMS0,
                        system_time_children:SysTimeChildrenMS0,
                        time_flag:Flag,
			dom_dom:DomDom0,
			dom_no_del:DomNoDel0,
			dom_del:DomDel0,
			dom_orbit:DomOrbit0
		    ],
		sbdd_stats with [
			time:TimeMS1,
                        system_time:SysTimeMS1,
                        time_children:TimeChildrenMS1,
                        system_time_children:SysTimeChildrenMS1,
			dom_dom:DomDom1,
			dom_no_del:DomNoDel1,
			dom_del:DomDel1,
			dom_orbit:DomOrbit1
		    ],
		sbdd_stats with [
			time:TimeMS,
                        system_time:SysTimeMS,
                        time_children:TimeChildrenMS,
                        system_time_children:SysTimeChildrenMS,
                        time_flag:Flag,
			dom_dom:DomDom,
			dom_no_del:DomNoDel,
			dom_del:DomDel,
			dom_orbit:DomOrbit
		    ]) :-
	TimeMS is TimeMS1 - TimeMS0,
	SysTimeMS is SysTimeMS1 - SysTimeMS0,
	TimeChildrenMS is TimeChildrenMS1 - TimeChildrenMS0,
	SysTimeChildrenMS is SysTimeChildrenMS1 - SysTimeChildrenMS0,
	DomDom is DomDom1 - DomDom0,
	DomNoDel is DomNoDel1 - DomNoDel0,
	DomDel is DomDel1 - DomDel0,
	DomOrbit is DomOrbit1 - DomOrbit0.

print_stats([EclStartTime,EclStartSystemTime,EclStartClockTime ],
	    [EclEndTime,EclEndSystemTime,EclEndClockTime ],
            InitialStats, 
            FinalStats) :-
	EclSec is EclEndTime - EclStartTime,
	EclSystemSec is EclEndSystemTime - EclStartSystemTime,
	EclClockSec is EclEndClockTime - EclStartClockTime,
        EclTotalSec is EclSystemSec+EclSec,
	sbdd_difference_stats(InitialStats, FinalStats, Stats),
	Stats = sbdd_stats with [
		time:TimeMS,
                system_time:SysTimeMS,
                time_children:TimeChildrenMS,
                system_time_children:SysTimeChildrenMS,
                time_flag:Flag,
		dom_dom:DomDom,
		dom_no_del:DomNoDel,
		dom_del:DomDel,
		dom_orbit:DomOrbit
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
	printf("Elapsed time (wallclock):%10.2f%n---%n", [EclClockSec]),
	printf("Dominance Checks: successful %d; unsuccessful %d; "
		"domain deletion %d; orbit %d.%n",
		[DomDom, DomNoDel, DomDel, DomOrbit]).


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

