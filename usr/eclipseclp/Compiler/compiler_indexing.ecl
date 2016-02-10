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
% Version:	$Id: compiler_indexing.ecl,v 1.12 2010/07/25 13:29:04 jschimpf Exp $
%----------------------------------------------------------------------

:- module(compiler_indexing).

:- comment(summary, "ECLiPSe III compiler - indexing").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2010/07/25 13:29:04 $").

:- use_module(compiler_common).
:- import state_lookup_binding/3 from compiler_analysis.

:- lib(hash).

:- comment(desc, ascii("
   This pass finds information that can be exploited for indexing (i.e.
   filtering alternatives from disjunctions). The disjunctions are annotated
   with this information.

   The code generator uses this information to generate switch-instructions
   and try-sequences.
")).


% Structure describing a guard test:
% Specifies which values of a variable the guard will accept.
% Guard goals that cannot be indexed are represented with varid:0,class:[]
:- local struct(guard(
    	branchnr,	% branch in which this guard occurs
	varid,		% variable that this guard tests (or 0)
	class		% list of: atomic Tag name, value(Val,Tag) or N/A
    )).


:- export indexing_transformation/3.

indexing_transformation(Goals, OutGoals, Options) :-
	indexing_transformation(Goals, OutGoals, det, Options).

indexing_transformation([], [], _Det, _Options).
indexing_transformation([Goal|Goals], OutGoals, Det, Options) :-
	( Goal = disjunction{branches:Branches,determinism:BranchDets} ->
	    OutGoals = [OutGoal|OutGoals1],
	    update_struct(disjunction, [branches:OutBranches], Goal, OutGoal),
	    index_disjunction(Goal),
	    dump_indexes(Goal, Options),
	    (
		foreach(Branch,Branches),
		foreach(OutBranch,OutBranches),
		foreacharg(BranchDet,BranchDets),
		param(Options)
	    do
		indexing_transformation(Branch, OutBranch, BranchDet, Options)
	    )

	; Goal = goal{functor:cut_to/1,kind:simple,definition_module:sepia_kernel,
			args:[variable{varid:VarId}],state:State,callpos:CutPos} ->
	    % Eliminate cuts that are always in the last (or only) alternative
	    (
		state_lookup_binding(State, VarId, ++(cutpoint(SaveCutPos))),
		in_following_branch_guard(SaveCutPos, CutPos),
		last_alternative(Det)
	    ->
		OutGoals = OutGoals1		% eliminate the cut!
	    ;
		OutGoals = [Goal|OutGoals1]
	    )
	;
	    OutGoals = [Goal|OutGoals1]
	),
	indexing_transformation(Goals, OutGoals1, Det, Options).


/*
Algorithm:
    Scan the guards in every branch of the disjunction.  The guards
    are the leading goals in the disjunctions up to, but not
    including, the first regular goal or the first cut(_to).

    A guard that can be used for indexing is represented as a struct
    guard{}, which describes the conditions under which a guard is
    satisfied for a particular variable.  It lists the value classes
    for which the guard must (t) or may (m) pass.  For example, a
    guard X=3 in branch 5 of the disjunctions is represented as
    guard{branchnr:5,varid:Xid,[[integer,3]-t]) A value class is a
    list containing the tag and optionally a value.

    Then the guards are grouped by variable, and translated into
    a decision tree, where the first level corresponds to the tags,
    and the second level to values.  However, the tree implementation
    is general and the structure can be made more complex.

    Finally, the decision trees for the different variables are
    evaluated, and ordered according to their selectivity.

    The weighted decision trees form the input for the code generator.

    CAUTION: the entries in the index tree *assume* (for purposes of
    definitive passing of guard and commit) that the indexing code
    tests for exactly the tag/value given in the tree entry.
*/

index_disjunction(disjunction{branches:Branches,branchlabels:BranchLabelArray,
		state:StartState,
		indexvars:Args,indexes:OrderedIndexes,determinism:Determinism}) :-

	% Collect all guards of all branches into one list of guard{}
	hash_create(VaridsInCommittedGuards),
	(
	    % for each branch in the disjunction
	    count(I,1,NBranches),
	    foreach(Branch,Branches),
	    fromto(GuardsByBranch,Guards0,Guards1,[]),
	    param(StartState,VaridsInCommittedGuards)
	do
	    extract_guards_from_prefix(Branch, I, StartState, [], GuardInfo0, false, _UnifyFlag, End),
	    ( End == commit ->
		% remember the varids that occur in committed guards
		( foreach(guard{varid:VarId},GuardInfo0), param(VaridsInCommittedGuards) do
		    hash_set(VaridsInCommittedGuards, VarId, [])
		),
		exploit_commit(GuardInfo0, GuardInfo1)
	    ;
		GuardInfo1 = GuardInfo0
	    ),
	    sort(varid of guard, =<, GuardInfo1, GuardInfo2),
	    % remove marker entries of non-indexable guards
	    ( GuardInfo2 = [guard{varid:0}|GuardInfo] ->
		true
	    ;
		GuardInfo = GuardInfo2
	    ),
	    append(GuardInfo, Guards1, Guards0)
	),
	dim(BranchLabelArray, [NBranches]),

	% Heuristic: If any of the branches had committed guards, we
	% use for indexing only the variables that occurred in at least
	% one committed guard. This reduces the number of useless
	% indexes on what are probably output variables.
	( hash_count(VaridsInCommittedGuards, 0) ->
	    % no committed guards at all: index everything
	    UsefulGuardsByBranch = GuardsByBranch
	;
	    % filter out likely output-variables
	    (
		foreach(Guard,GuardsByBranch),
		fromto(UsefulGuardsByBranch,UGBB1,UGBB0,[]),
		param(VaridsInCommittedGuards)
	    do
		Guard = guard{varid:VarId},
	    	( hash_contains(VaridsInCommittedGuards,VarId) ->
		    UGBB1 = [Guard|UGBB0]
		;
		    UGBB1 = UGBB0
		)
	    )
	),

	% Compute the set of indexable varids and initialise
	% one index descriptor for each of them
	project_arg(varid of guard, UsefulGuardsByBranch, VarIdsMulti),
	sort(0, <, VarIdsMulti, VarIds),
	(
	    % for each indexable variable
	    foreach(VarId,VarIds),
	    foreach(VarDesc,Args),
	    foreach(index{variable:VarDesc,partition:DT},Indexes)
	do
	    % create variable access descriptor (must be done before
	    % compute_lifetimes) for use in generate_code later
	    new_vardesc(VarId, VarDesc),
	    % init the decision tree for this variable
	    dt_init(DT)
	),

	% Now incrementally build the decision trees by adding
	% each branch to each variable's decision tree.
	% PRE: UsefulGuardsByBranch are sorted first by branch, then by varid.
	(
	    % for each branch in the disjunction
	    for(I,1,NBranches),
	    fromto(UsefulGuardsByBranch,Guards1,Guards4,[]),
	    param(Indexes)
	do
	    (
		% for each indexable variable
		foreach(index{variable:variable{varid:VarId},partition:DT},Indexes),
		fromto(Guards1,Guards2,Guards3,Guards4),
		param(I)
	    do
		( Guards2 = [guard{varid:VarId,branchnr:I,class:AltClasses}|Guards3] ->
		    ( foreach(Class-Pass,AltClasses), param(I,DT) do
			( Pass=c -> Final=yes ; Final=no),
			dt_add(DT, Class, I, Final)
		    )
		;
		    % no guard for VarId in branch I
		    Guards3 = Guards2,
		    dt_add(DT, [], I, no)
		)
	    )
	),
	    
	% Evaluate and sort indexes according to quality
	( foreach(index{partition:Dt,quality:Q},Indexes) do
	    eval_index_quality(Dt, Q)
	),
	sort(quality of index, =<, Indexes, OrderedIndexes),
	eval_index_det(OrderedIndexes, NBranches, Determinism).



% Takes the goals that start the given branch, and a starting state.
% Computes a representation of the guard goals, plus a flag indicating
% whether the guard ends with or without a commit.
extract_guards_from_prefix([], _BranchNr, _StartState, Info, Info, UnifyFlag, UnifyFlag, end).
extract_guards_from_prefix([Goal|Goals], BranchNr, StartState, Info0, Info, UnifyFlag0, UnifyFlag, End) :-
	(
	    % consider only builtin predicates
	    % caution: regular preds can wake (and fail!)
	    Goal = goal{kind:simple,definition_module:sepia_kernel},
	    extract_guards_from_goal(Goal, BranchNr, StartState, UnifyFlag0, UnifyFlag1, Guard, End)
	->
	    and_guard(Guard, Info0, Info1),
	    ( var(End) ->
		extract_guards_from_prefix(Goals, BranchNr, StartState, Info1, Info, UnifyFlag1, UnifyFlag, End)
	    ;
		% end of guard detected
	    	Info = Info1, UnifyFlag = UnifyFlag0
	    )

	; Goal = goal{kind:head,state:HeadState} ->
	    % Use the head's binding information instead of what was known
	    % prior to the disjunction
	    extract_guards_from_prefix(Goals, BranchNr, HeadState, Info0, Info, UnifyFlag0, UnifyFlag, End)
%	    extract_guards_from_prefix(Goals, BranchNr, StartState, Info0, Info, UnifyFlag0, UnifyFlag, End)

	; Goal = disjunction{branches:[SubBranch1|SubBranches]} ->
	    % look into the prefixes of the branches
	    extract_guards_from_prefix(SubBranch1, BranchNr, StartState, [], SubInfo1, UnifyFlag0, UnifyFlag1, _End),
	    (
		foreach(SubBranch,SubBranches),
		fromto(SubInfo1,SubInfo2,SubInfo4,SubInfo),
		fromto(UnifyFlag1,UnifyFlag2,UnifyFlag3,UnifyFlag),
		param(BranchNr,StartState, UnifyFlag0)
	    do
		extract_guards_from_prefix(SubBranch, BranchNr, StartState, [], SubInfo3, UnifyFlag0, UnifyFlagI, _End),
		or_guards(SubInfo2, SubInfo3, SubInfo4),
		or_flags(UnifyFlagI, UnifyFlag2, UnifyFlag3)
	    ),
	    and_guards(Info0, SubInfo, Info),
	    End = end	% not sure about the scope of commits in sub-branches
	;
	    % end of guard
	    Info = Info0, UnifyFlag = UnifyFlag0, End = end
	).


% PRE: the goal is a builtin from sepia_kernel.
% Fail if encountering a goal that signals end-of-guard.
% Regular goal can cause waking (and therefore insert failures).
% StartState is the analysis state at the beginning of the disjunction.
% UnifyFlag is true if goals between switch and the current goal might
% have unified any switch variables (and thus weakened the guards).
% Additionally, we set UnifyFlagAfter iff the current goal can unify a
% switch variable (and thus weaken the switch conditions for subsequent guards).

extract_guards_from_goal(goal{functor:get_cut/1},
		_BranchNr, _StartState, UnifyFlag, UnifyFlag, true, _) :- !.

extract_guards_from_goal(goal{functor:cut_to/1},
		_BranchNr, _StartState, UnifyFlag, UnifyFlag, true, commit) :- !.

extract_guards_from_goal(goal{functor:(=)/2, args:[Lhs,Rhs], state:GoalState},
		BranchNr, StartState, _UnifyFlag, UnifyFlagAfter, Guard, _) :- !,
	% unifications should be normalised and always
	% have a variable on the left hand side
	certainly_once Lhs = variable{varid:VarId},
	% state_lookup_binding should succeed iff the variable was known
	% before the start of the disjunction
	( state_lookup_binding(StartState, VarId, _Binding) ->
	    ( atomic_tag(Rhs, Tag) ->
		( value_indexable(Tag) ->
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[var]-t,[Tag,Rhs]-t]}
		; single_value(Tag) ->
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[var]-t,[Tag]-t]}
		;
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[var]-t,[Tag]-m]}
		)
	    ; Rhs = structure{name:F,arity:A,args:Args} ->
		(all_fresh_vars(Args, A, GoalState) -> PassFlag=t ; PassFlag=m ),
		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[var]-t,[structure,F/A]-PassFlag]}
	    ; Rhs = [A1|A2] ->
		(all_fresh_vars([A1,A2], 2, GoalState) -> PassFlag=t ; PassFlag=m ),
		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[var]-t,[list]-PassFlag]}
	    ; Rhs = variable{varid:VarId} ->
		% an X=X dummy goal
		Guard = true
	    ; verify Rhs = variable{},
		%%% REVIEW: classes should be disjoint
%		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[]-m,[var]-t]}
%		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[]-m]}
		Guard = guard{branchnr:BranchNr,varid:0,class:[]}
	    ),
	    % Conservatively assume that the goal may (directly, via aliasing,
	    % or via occurrences in the Rhs) unify this or another switch variable
	    UnifyFlagAfter = true
	    % This could be more precise, using binding information:
	    % A = X				false
	    % A = f(X,X)			false
	    % X = f(A,B)	with inst(X)	false
	    % X = f(A,B)	with univ(X)	true
	    % X = f(A,A)			true
	    % X = f(Y)			true
	    % X = f(a)			true
	    % X = f(g(_))			true
	    %	state_lookup_binding(GoalState, VarId, LhsBinding),
	    %	( binding_inst(LhsBinding) -> \+all_fresh_term(Rhs) ; true ).
	;
	    % Nothing known about the variable at switch time, so it can't be
	    % used for indexing. Check whether it can fail at call time.
	    ( state_lookup_binding(GoalState, VarId, _Binding) ->
		% insert marker for possibly failing goal
		Guard = guard{branchnr:BranchNr,varid:0,class:[]},
		% Conservatively assume that the goal may (via aliasing or
		% occurrences in Rhs) unify another switch variable
		UnifyFlagAfter = true
	    ;
		% a fresh variable, goal will always succeed
		Guard = true,
		% No danger of the guard unifying a switch variable
		UnifyFlagAfter = false
	    )
	).

extract_guards_from_goal(goal{functor:(==)/2, args:[Lhs,Rhs]},
		BranchNr, StartState, UnifyFlag, UnifyFlag, Guard, _) :- !,
	( Lhs = variable{varid:VarId}, Rhs \= variable{} ->
	    extract_guards_from_identity(VarId, Rhs, BranchNr, StartState, UnifyFlag, Guard)
	; Rhs = variable{varid:VarId}, Lhs \= variable{} ->
	    extract_guards_from_identity(VarId, Lhs, BranchNr, StartState, UnifyFlag, Guard)
	; Lhs = variable{varid:VarId}, Rhs = variable{varid:VarId} ->
	    Guard = true
	;
	    % goal may fail, but can't be used for indexing
	    Guard = guard{branchnr:BranchNr,varid:0,class:[]}
	).

extract_guards_from_goal(goal{functor:(?=)/2, args:[Lhs,Rhs], state:GoalState},
		BranchNr, StartState, UnifyFlag, UnifyFlag, Guard, _) :- !,
	% matchings should not be preceded by unifications
	verify UnifyFlag == false,
	% matchings should be normalised and always
	% have a variable on the left hand side
	certainly_once Lhs = variable{varid:VarId},
	% state_lookup_binding should succeed iff the variable was known
	% before the start of the disjunction
	( state_lookup_binding(StartState, VarId, _Binding) ->
	    ( atomic_tag(Rhs, Tag) ->
		( value_indexable(Tag) ->
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[Tag,Rhs]-t]}
		; single_value(Tag) ->
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[Tag]-t]}
		;
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[Tag]-m]}
		)
	    ; Rhs = attrvar{} ->
		% TODO fresh vars check
		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[var,meta]-m]}
	    ; Rhs = structure{name:F,arity:A,args:Args} ->
		(all_fresh_vars(Args, A, GoalState) -> PassFlag=t ; PassFlag=m ),
		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[structure,F/A]-PassFlag]}
	    ;
		Rhs = [A1|A2],
		(all_fresh_vars([A1,A2], 2, GoalState) -> PassFlag=t ; PassFlag=m ),
		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[list]-PassFlag]}
	    )
	;
	    % This can happen if the lhs was an output mode (-) argument
	    warning("Output mode (-) overrides matching clause semantics"),
	    Guard = true
	).

extract_guards_from_goal(goal{
    		functor:Test/1, args:[variable{varid:VarId}] },
		BranchNr, StartState, UnifyFlag, UnifyFlag, Guard, _) :-
	type_test(Test, TestClasses),
	!,
	( state_lookup_binding(StartState, VarId, _Binding) ->
	    binding_effect_on_guard(UnifyFlag, TestClasses, Classes),
	    Guard = guard{branchnr:BranchNr,varid:VarId,class:Classes}
	;
	    % nothing known about the variable,
	    % goal may fail, but can't be used for indexing
	    % (we could probably be more precise here)
	    Guard = guard{branchnr:BranchNr,varid:0,class:[]}
	).

    % For the ==/2 predicate, matching, etc
    extract_guards_from_identity(VarId, Rhs, BranchNr, StartState, UnifyFlag, Guard) :-
	% state_lookup_binding should succeed iff the variable was known
	% before the start of the disjunction
	( state_lookup_binding(StartState, VarId, _Binding) ->
	    % Binding after switch can make guard true in var case!
	    binding_effect_on_guard(UnifyFlag, [], VarClass),
	    ( atomic_tag(Rhs, Tag) ->
		( value_indexable(Tag) ->
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[Tag,Rhs]-t|VarClass]}
		; single_value(Tag) ->
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[Tag]-t|VarClass]}
		;
		    Guard = guard{branchnr:BranchNr,varid:VarId,class:[[Tag]-m|VarClass]}
		)
	    ; Rhs = structure{name:F,arity:A} ->
		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[structure,F/A]-m|VarClass]}
	    ; verify Rhs = [_|_],
		Guard = guard{branchnr:BranchNr,varid:VarId,class:[[list]-m|VarClass]}
	    )
	;
	    % nothing known about the variable at switch time, can't be used for indexing,
	    Guard = guard{branchnr:BranchNr,varid:0,class:[]}
	).


% The tags that have switch_on_value instructions
:- mode value_indexable(+).
value_indexable(integer).
value_indexable(atom).
value_indexable(structure).


single_value([]).


% Compute the tag of a value
:- mode atomic_tag(+,-).
atomic_tag(X, bignum) :- sepia_kernel:bignum(X), !.
atomic_tag(X, integer) :- integer(X).
atomic_tag([], '[]') :- !.
atomic_tag(X, atom) :- atom(X).
atomic_tag(X, breal) :- breal(X).
atomic_tag(X, double) :- float(X).
atomic_tag(X, rational) :- rational(X).
atomic_tag(X, handle) :- is_handle(X).	% can't occur in textual source
atomic_tag(X, string) :- string(X).


% Compute the tag sets resulting from various type tests
% Also set the pass-flag:
%	t	with this tag the test is definitely satisfied
%	m	with this tag the test may be satisfied
type_test(atom,		[[atom]-t,[[]]-t]).
type_test(atomic,	[[[]]-t,[atom]-t,[bignum]-t,[breal]-t,[goal]-t,[double]-t,[handle]-t,[integer]-t,[rational]-t,[string]-t]).
type_test(bignum,	[[bignum]-t]).
type_test(breal,	[[breal]-t]).
type_test(callable,	[[[]]-t,[atom]-t,[list]-t,[structure]-t]).
type_test(compound,	[[list]-t,[structure]-t]).
type_test(float,	[[double]-t]).
type_test(free,		[[var,free]-t]).
type_test(ground,	[[[]]-t,[atom]-t,[bignum]-t,[breal]-t,[goal]-t,[list]-m,[structure]-m,[double]-t,[handle]-t,[integer]-t,[rational]-t,[string]-t]).	% not only tag test
type_test(integer,	[[bignum]-t,[integer]-t]).
type_test(is_event,	[[atom]-m,[handle]-m]).	% not only tag test!
type_test(is_handle,	[[handle]-t]).
type_test(is_list,	[[[]]-t,[list]-m]).	% not only tag test!
type_test(is_suspension, [[goal]-m]).		% not only tag test!
type_test(meta,		[[var,meta]-t]).
type_test(nonground,	[[var]-t,[list]-m,[structure]-m]).	% not only tag test
type_test(nonvar,	[[[]]-t,[atom]-t,[bignum]-t,[breal]-t,[goal]-t,[list]-t,[structure]-t,[double]-t,[handle]-t,[integer]-t,[rational]-t,[string]-t]).
type_test(number,	[[bignum]-t,[breal]-t,[double]-t,[integer]-t,[rational]-t]).
type_test(rational,	[[rational]-t]).
type_test(real,		[[breal]-t,[double]-t]).
type_test(string,	[[string]-t]).
type_test(var,		[[var]-t]).


% A unification of the switch-argument in between the switch and the
% guard test can make the guard true in the var-case as well.
binding_effect_on_guard(true, TestClasses, Classes) ?-
	or_classes(TestClasses, [[var]-m], Classes).
binding_effect_on_guard(false, Classes, Classes).


and_guards(Guards1, Guards2, Guards) :-
	(
	    foreach(Guard,Guards1),
	    fromto(Guards2,Guards3,Guards4,Guards)
	do
	    and_guard(Guard, Guards3, Guards4)
	).


% OldGuards is a list containing at most one guard{} for each VarId
% Guard is a guard{} for a particular VarId
and_guard(true, Guards, Guards).
and_guard(Guard, OldGuards, NewGuards) :-
	Guard = guard{branchnr:BranchNr,varid:VarId,class:Classes},
	% lookup and replace guard for BranchNr and VarId
	OldGuard = guard{branchnr:BranchNr,varid:VarId,class:OldClasses},
	( selectchk(OldGuard, OldGuards, NewGuard, NewGuards) ->
	    NewGuard = guard{branchnr:BranchNr,varid:VarId,class:NewClasses},
	    and_classes(Classes, OldClasses, NewClasses)
	;
	    NewGuards = [Guard|OldGuards]
	).


% Compute the conjunctions of the guards, represented as class lists.
% Class lists are supposed to contain disjoint, alternative classes.
and_classes(Ls, Rs, Cs) :-
	(
	    foreach(LClass-LPass,Ls) * foreach(RClass-RPass,Rs),
	    fromto(Cs,Cs1,Cs0,[])
	do
	    ( and_class(LClass,RClass,Class) ->
		and_pass(LPass, RPass, Pass),
		Cs1 = [Class-Pass|Cs0]
	    ;
		Cs1 = Cs0
	    )
	).


    % guard passing only guaranteed if both guards are guaranteed to pass
    and_pass(t, t, t) :- !.
    and_pass(_, _, m).


    % keep the more specific class only. e.g. [atom,a] * [atom] -> [atom,a]
    % fail if classes are incomparable
    and_class(L, R, C) :-
    	( append(L, _, R) ->	% L is the prefix
	    C = R
    	; append(R, _, L) ->	% R is the prefix
	    C = L
	).


or_guards(Guards1, Guards2, OrGuards) :-
	(
	    foreach(guard{varid:VarId,branchnr:BNr,class:Class1},Guards1),
	    fromto(Guards2,Guards3,Guards4,_),
	    fromto(OrGuards,OrGuards1,OrGuards2,[])
	do
	    ( VarId \== 0, delete(guard{varid:VarId,branchnr:BNr,class:Class2},Guards3,Guards4) ->
		or_classes(Class1, Class2, OrClass),
		OrGuards1 = [guard{varid:VarId,branchnr:BNr,class:OrClass}|OrGuards2]
	    ;
		Guards3 = Guards4, OrGuards1 = OrGuards2
	    )
	).


% Compute the disjunction of the guards, represented as class lists.
% Class lists are supposed to contain disjoint, alternative classes.
or_classes(Ls, Rs, Cs) :-
	sort(1, =<, Ls, Ls1),
	sort(1, =<, Rs, Rs1),
	merge(1, =<, Ls1, Rs1, Cs0),
	( Cs0 = [C1|Cs1] ->
	    (
		fromto(C1,C0,C,Cn),
		fromto(Cs1,[C2|Cs2],Cs2,[]),
		fromto(Cs,Cs3,Cs4,[Cn])
	    do
		( class_subsumes(C0, C2, C) ->
		    Cs3 = Cs4			% drop C2
		;
		    Cs3 = [C0|Cs4], C = C2
		)
	    )
	;
	    Cs = Cs0
	).

    class_subsumes(LC-LP, RC-RP, LC-P) :-
    	append(LC, Rest, RC),
	( Rest == [] ->
	    or_pass(LP, RP, P)	% LC==RC: choose stronger pass flag
	;
	    P = LP	% LC-LP
	).

    or_pass(t, _, t) :- !.
    or_pass(_, t, t) :- !.
    or_pass(_, _, m).


or_flags(false, false, false) :- !.
or_flags(false, true, true) :- !.
or_flags(true, false, true) :- !.
or_flags(true, true, true) :- !.


% Check whether Args is a list of disjoint fresh variables
all_fresh_vars(Args, Arity, State) :-
	(
	    foreach(variable{varid:VarId},Args),
	    foreach(VarId,VarIds),
	    param(State)
	do
	    \+ state_lookup_binding(State, VarId, _Binding)
	),
	sort(VarIds, UniqVarIds),
	length(UniqVarIds, Arity).


% Succeed iff term does not contain old variables or internal aliasing
all_fresh_term(Term, _State) :- atomic(Term).
all_fresh_term(structure{arity:Arity,args:Args}, State) ?- !,
	all_fresh_vars(Args, Arity, State).
all_fresh_term([A1|A2], State) ?- !,
	all_fresh_vars([A1,A2], 2, State).
all_fresh_term(variable{varid:VarId}, State) ?- !,
	\+ state_lookup_binding(State, VarId, _Binding).
%all_fresh_term(attrvar{}, _State) ?- fail.


% If we had only one guarded variable followed by commit,
% we change its pass-markers from -t to -c to indicate that
% any subsequent branches cannot be reached for these classes.
exploit_commit([Guard0], [Guard]) :- !,
	Guard0 = guard{class:Classes0},
	update_struct(guard, class:Classes, Guard0, Guard),
	(
	    foreach(Class-Pass0,Classes0),
	    foreach(Class-Pass,Classes)
	do
	    ( Pass0=t -> Pass=c ; Pass=Pass0 )
	).
exploit_commit(GuardInfo, GuardInfo).



% Evaluate index quality: A positive float, the smaller the better.
% Roughly computes fan-out (number of alternatives jumped to)
% divided by fan-in (number of different argument values tested for).

eval_index_quality(Dt, Q) :-	
	% collect all occurring sets of alternatives
	dt_values(Dt, Branches0),
	( dt_lookup2(Dt, [var], _, _) ->
	    Branches = Branches0
	;
	    Branches = [[]|Branches0]
	),
	% remove duplicate sets
	sort(Branches, BranchesSets),
	(
	    foreach(BranchesSet,BranchesSets) >> foreach(_,BranchesSet),
	    count(_,1,NTargetBranches)
	do
	    true
	),
	% This is the quality measure
	Q is NTargetBranches/length(BranchesSets).


%
% Compute determinacy information after indexing analysis
% (we only look at the first index, and assume it is going to be
% implemented accurately by the generated indexing code)
%
% BranchDets: For each branch of the disjunction, which position it can take:
%	det - never one of several matching alternatives
%	try - always the first of several matching alternatives
%	trust - always the last of several matching alternatives
%	retry - can be anywhere in try sequence
%	failure - never matches (dead code)
%
% DisjDet: The whole disjunction is classified as:
%	semidet - if it never creates a choicepoint
%	nondet - otherwise
%

eval_index_det([index{partition:Dt}|_], NBranches, BranchDets) :- !,
	dt_list(Dt, Parts),
	hash_create(NonLasts),
	hash_create(Dets),
	hash_create(NonFirsts),
	(
	    foreach(_Key-Branches,Parts),
	    param(NonLasts,Dets,NonFirsts)
	do
	    ( Branches = [] ->
		true
	    ;
	    	Branches = [B1|Bs],
		( Bs = [] ->
		    hash_set(Dets, B1, true)
		;
		    hash_set(NonLasts, B1, true),
		    ( fromto(Bs,[Bi|Bs1],Bs1,[Bn]), param(NonFirsts,NonLasts) do
			hash_set(NonFirsts, Bi, true),
			hash_set(NonLasts, Bi, true)
		    ),
		    hash_set(NonFirsts, Bn, true)
		)
	    )
	),
	dim(BranchDets, [NBranches]),
	( foreacharg(BranchDet,BranchDets,I), param(NonLasts,Dets,NonFirsts) do
	    ( hash_contains(NonFirsts, I) ->
		( hash_contains(NonLasts, I) ->
		    BranchDet = retry
		;
		    BranchDet = trust
		)
	    ; hash_contains(NonLasts, I) ->
		BranchDet = try
	    ; hash_contains(Dets, I) ->
		BranchDet = det
	    ;
		BranchDet = failure
	    )
	).
eval_index_det([], NBranches, BranchDets) :-
	verify NBranches >= 2,
	dim(BranchDets, [NBranches]),
	arg(1, BranchDets, try),
	arg(NBranches, BranchDets, trust),
	( for(I,2,NBranches-1), param(BranchDets) do
	    arg(I, BranchDets, retry)
	).


% Debugging: print readable summary of index

dump_indexes(disjunction{callpos:CallPos,determinism:BranchDets,indexes:Indexes}, options{print_indexes:Flag}) :-
	( Flag==on, Indexes = [_|_] ->
	    ( foreacharg(BrDet,BranchDets), fromto(semidet,Det1,Det2,Det) do
	    	( (BrDet==det;BrDet==failure) -> Det2=Det1 ; Det2=nondet )
	    ),
	    printf("INDEXES for (%w) disjunction %w%n", [Det,CallPos]),
	    (
		count(I,1,_),
%		foreach(index{quality:Q,variable:variable{varid:VarId},partition:Dt},Indexes)
		foreach(index{quality:Q,partition:Dt},Indexes)
	    do
		Q1 is round(10*Q)/10,	% printf's rounding is unreliable
%		printf("%d. Quality %.1f, variable %d%n", [I,Q1,VarId]),
		printf("%d. Quality %.1f%n", [I,Q1]),
		dt_list(Dt, Parts), 
		( foreach(Part,Parts) do
		    printf("    %w%n", [Part])
		)
	    ),
	    printf("Branch determinisms for disjunction %w%n", [CallPos]),
	    ( foreacharg(BranchDet,BranchDets,I) do
		printf("    Branch %d: %w%n", [I,BranchDet])
	    )
	;
	    true
	).
