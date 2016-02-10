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
% Version:	$Id: compiler_varclass.ecl,v 1.15 2009/07/16 09:11:23 jschimpf Exp $
%
% Related paper (although we haven't used any of their algorithms):
% H.Vandecasteele,B.Demoen,G.Janssens: Compiling Large Disjunctions
% KU Leuven 2001
% ----------------------------------------------------------------------


:- module(compiler_varclass).

:- comment(summary, "ECLiPSe III compiler - variable classification").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2009/07/16 09:11:23 $").

:- comment(desc, html("
    This pass (consisting of several phases) does the following jobs:
    <UL>
    <LI>
    Computing the lifetimes of variables, thus classifying them into void
    variables (for which singleton warnings may be generated), temporary
    variables (whose lifetime does not extend across regular predicate calls),
    and permanent variables (which require an environment slot). This
    information is filled into the class-slots of the Body's variable{}
    descriptors. Note that variables of the same name which occur only in
    alternative disjunctive branches, are considered separate variables,
    and may be assigned difference storage classes. 
    <LI>
    Decide whether values should be passed into disjunctions via environment
    variables or as pseudo-aruments via argument registers.
    <LI>
    The second phase assigns concrete environment slots to variables,
    ordered such that lifetimes that end later are put in slots with
    lower numbers (if possible), which enables environment trimming.
    It also computes the total environment size needed.
    <LI>
    The third phase computes environment activity maps for every relevant
    position in the code.  These are needed to tell the garbage collector
    which slots are not yet initialised, and which slots lifetime has
    already ended even if the environment hasn't been trimmed (yet).
    </UL>
    <P>
    Note that, in this context, we talk about 'first' and 'last' occurrences
    only with a granularity of 'call positions', e.g. all occurrences of a
    variable in the first chunk it occurs in are considered 'first'.
    That way, later compiler stages are still free to reorder operations
    within each chunk without affecting the variable classification.
    <P>
    This pass recognises the options 'print_lifetimes' (on/off) and
    'warnings' (on/off) for singleton variable warnings.
")).


:- lib(hash).

:- use_module(compiler_common).
:- use_module(compiler_map).


% struct(slot) describes one true, distinct variable. There may be more
% of those than there are variables in the source, because we classify
% variables in parallel disjunctive branches as being distinct.

:- local struct(slot(		% one for every truly distinct variable
	source_info,		% for error messages only
	firstpos,		% position of first occurrence
				% (must be first for sorting!)
	lastpos,		% position of last occurrence
	class			% shared with all occurrences (struct(variable))
    )).

:- comment(struct(slot), [
    summary:"Temporary data structure during computation of lifetimes",
    fields:[
	firstpos:"call position of first variable occurrence",
	lastpos:"call position of last variable occurrence",
	class:"shared class-field of all variable occurrences"
    ],
    see_also:[struct(variable)]
]).

% Maybe we could speed up processing by sharing the variable descriptors
% for each chunk, and keeping them separately. This would benefit the passes
% compute_lifetimes and assign_env_slots - they would not have to deal
% with multiple occurrences in the same chunk.


%----------------------------------------------------------------------
% Variable lifetimes and detection of false sharing
%
% We build a map that stores for each variable the first and last occurrences
% (in terms of call positions).  This is needed for classifying variables
% as permanent. We are not interested to know which occurrence _within_ a chunk
% is first, this will be determined later when generating code for the chunk.
% This has the advantage that everything within the chunk can still be
% reordered after this pass.
%
% Because of disjunctive branches, there can be more than one
% first and last occurrence of each variable. Moreover, variables
% with the same name in different branches are really different
% variables, so this pass finds out how many different variables
% there really are.
%
% The disjunctions are conceptually traversed in parallel.
% When joining up, we merge the branches's maps into one.
% 
% Data structures:
%    Variable occurrence:
%	variable(VarId, IsAFirst, IsALast, ClassAndPermLocation)
%    Maintain map of:
%	VarId - [slot(FirstPos,LastPos,LastFlag,Location), ...]
%		one slot for each truly distinct variable
% 
% The two interesting operations are
%
%	- registering a new occurrence of a variable
%	- merging the information when disjunctive branches join up
%
%
% TODO: could keep slot lists in reverse order wrt firstpos,
% then they could be merged more efficiently.
%----------------------------------------------------------------------

:- comment(classify_variables/3, [
    summary:"Compute variable sharing, lifetimes and storage locations",
    amode:classify_variables(+,+,+),
    args:[
	"Body":"Normalised predicate",
	"EnvSize":"Extra environment slots needed",
	"Options":"options-structure"
    ],
    see_also:[print_occurrences/1]
]).

:- export classify_variables/3.
classify_variables(Body, EnvSize, Options) :-
	verify EnvSize == 0,	% not yet done
	compiler_map:init(Lifetimes0),
	compute_lifetimes(Body, nohead, _PredHead, Lifetimes0, Lifetimes),
	assign_env_slots(Lifetimes, MaxEnvSize, Options),
	mark_env_activity(Body, MaxEnvSize),
	( Options = options{print_lifetimes:on} ->
	    printf("------ Environment size %d ------%n", [EnvSize]),
	    print_occurrences(Lifetimes)
	;
	    true
	).


compute_lifetimes([], _PredHead, nohead, Map, Map).
compute_lifetimes([Goal|Goals], PredHead0, PredHead, Map0, Map) :-
	compute_lifetimes(Goal, PredHead0, PredHead1, Map0, Map1),
	compute_lifetimes(Goals, PredHead1, PredHead, Map1, Map).
compute_lifetimes(disjunction{branches:Branches,callpos:DisjPos,
		    branchlabels:BLA, indexvars:IndexArgs,
		    arity:Arity,args:DisjArgs,branchheadargs:HeadArgsArray},
		PredHead, nohead, Map0, Map) :-
	% Index variables are accessed just before the disjunction
	prev_call_pos(DisjPos, PreDisjPos),
	compute_lifetimes_term(PreDisjPos, IndexArgs, Map0, Map1),
	% Select pseudo-arguments to pass into the disjunction
	select_pseudo_arguments(Branches, PredHead, PreDisjPos, Map1, Map2, DisjArgs, Arity),
	( DisjArgs == [] ->
	    HeadArgsArray = []	% instead of []([],...,[]), save some space
	;
	    arity(BLA, NBranches),
	    dim(HeadArgsArray, [NBranches])
	),
	(
	    foreach(Branch,Branches),
	    foreach(BranchMap,BranchMaps),
	    count(I,1,_),
	    param(Map2,DisjArgs,HeadArgsArray,DisjPos,Arity)
	do
	    % prefix pseudo-head arguments to the branch
	    make_branch_head(I, HeadArgsArray, DisjArgs, HeadArgs),
	    append(DisjPos, [I,1], BranchFirstPos),
	    compute_lifetimes_term(BranchFirstPos, HeadArgs, Map2, Map3),
	    compute_lifetimes(Branch, Arity-HeadArgs, _PredHead, Map3, BranchMap)
	),
	merge_branches(DisjPos, BranchMaps, Map).
compute_lifetimes(Goal, PredHead0, PredHead, Map0, Map) :-
	Goal = goal{kind:Kind,callpos:GoalPos,args:Args,functor:_/Arity},
	( Kind == head -> verify PredHead0==nohead, PredHead = Arity-Args
	; Kind == simple -> PredHead = PredHead0
	; PredHead = nohead
	),
	compute_lifetimes_term(GoalPos, Args, Map0, Map).

    compute_lifetimes_term(CallPos, [X|Xs], Map0, Map) :-
	compute_lifetimes_term(CallPos, X, Map0, Map1),
	compute_lifetimes_term(CallPos, Xs, Map1, Map).
    compute_lifetimes_term(CallPos, Occurrence, Map0, Map) :-
	Occurrence = variable{},
	register_occurrence(CallPos, Occurrence, Map0, Map).
    compute_lifetimes_term(CallPos, attrvar{variable:Avar,meta:Meta}, Map0, Map) :-
	compute_lifetimes_term(CallPos, Avar, Map0, Map1),
	compute_lifetimes_term(CallPos, Meta, Map1, Map).
    compute_lifetimes_term(CallPos, structure{args:Args}, Map0, Map) :-
	compute_lifetimes_term(CallPos, Args, Map0, Map).
    compute_lifetimes_term(_CallPos, Term, Map, Map) :- atomic(Term).


% When encountering a variable VarId at CallPos:
% 
% VarId not seen at all so far:
%	- add new slot entry
%	- it's the first and last occurrence
%
% VarId has one slot:
%	- it's a new last ocurrence of that variable
%	- update the slot's last-information
%
% VarId has multiple slots:
%	- the new occurrence means the multiple slots must be merged
%	- the summary slot takes the common prefix of all first occurrences
%	- the current occurrence is the last
%	- the locations are all unified
 
register_occurrence(CallPos, Occurrence, Map0, Map) :-
	Occurrence = variable{source_info:Source,varid:VarId,class:Location},
	( compiler_map:search(Map0, VarId, OldEntry) ->
	    OldEntry = [OldSlot|Slots],
	    OldSlot = slot{firstpos:FP0,class:Location},
	    merge_slots(Slots, FP0, FP, Location),
	    update_struct(slot, [firstpos:FP,lastpos:CallPos], OldSlot, NewSlot),
	    compiler_map:det_update(Map0, VarId, [NewSlot], Map)
	;
	    % first occurrence
	    compiler_map:det_insert(Map0, VarId, [slot{source_info:Source,firstpos:CallPos,
	    	lastpos:CallPos, class:Location}], Map)
	).

    % - unifies all the slot's class fields
    % - computes the common prefix for the first position
    merge_slots([], FP, FP, nonvoid(_)).
    merge_slots([slot{firstpos:ThisFP,class:Location}|Slots], FP0, FP, Location) :-
    	common_pos(ThisFP, FP0, FP1),
	merge_slots(Slots, FP1, FP, Location).


% Merge the slot information from the branches:
% 
% The maps from the different branches may contain (for a particular VarId):
%
% all first occurrence(s) in current disjunction:
%			---C1--C2--	C1-C2
%	---------------|				-> C1-C2,D1-D2
%			---D1--D2--	D1-D2
%	keep all (they are different)
%
% common, identical entries:
%			-----------	A1-A2
%	---A1--A2------|				-> A1-A2
%			-----------	A1-A2
%	first and last occurrence are older than the disjunction
%	we keep one of them (they are all the same).
% 
% multiple entries, last occurrences older than current disjunction:
%	---A1--A2--	-----------	A1-A2,B1-B2
%		   |---|				-> A1-A2,B1-B2
%	---B1--B2--	-----------	A1-A2,B1-B2
%	keep one of each (they are different)
% 
% first occurrence older, last in current disjunction:
% some branches will still have old-old entry
%			-----C-----	A1-C
%	---A1--A2------|				-> A1-CD
%			-----------	A1-A2
%	where CD is the end of disjunction's callpos (C<CD)
%
% first occurrence older, last in current disjunction:
% some branches may still have old-old entry
%			-----C-----	A1-C
%	---A1--A2------|				-> A1-CD
%			-----D-----	A1-D
%	where CD is the end of disjunction's callpos (C<CD,D<CD)
%
% first occurrences older, last in current disjunction:
% some branches will still have multiple old-old entries
%	---A1--A2--	-----C-----	AB-C
%		   |---|				-> AB-CD
%	---B1--B2--	-----------	A1-A2,B1-B2
%	where CD is the end of disjunction's callpos (C<CD)
%	and AB the common prefix of the first occurrences (AB<A1,AB<B1).
%
% first occurrence older, last _is_ current disjunction:
% some branches may still have multiple old-old entries
%	---A1--A2--	-----C-----	AB-C
%		   |---|				-> AB-CD
%	---B1--B2--	-----D-----	AB-D
%	where CD is the end of disjunction's callpos (C<CD,D<CD)
%	and AB the common prefix of the first occurrences (AB<A1,AB<B1).
%
% entries with common first and different last occurrences:
%	- first occurrence is older than the disjunction!
%	- summarise them into one entry (by taking the common prefix of the
%	last occurrences, and unifying the class)
%
% entries whose first occurrence differs:
%	- the first occurrence may be in this or in an earlier disjunction!
%	- keep them both, they represent conceptually different variables.

merge_branches(DisjPos, BranchMaps, MergedMap) :-
	(
	    foreach(Map,BranchMaps),
	    fromto(Lists, [MapList|Lists1], Lists1, Tail)
	do
	    compiler_map:to_sorted_assoc_list(Map, MapList)
	),
	merge_sorted_lists(Lists, Tail, MergedList),
	concat_same_key_values_unstable(MergedList, GroupedList),
	(
	    foreach(VarId-Slots,GroupedList),
	    foreach(VarId-NewSlots,NewGroupedList),
	    param(DisjPos)
	do
	    % remove duplicates AND sort by ascending firstpos
	    sort(Slots, SortedNoDupSlots),
	    SortedNoDupSlots = [slot{firstpos:OldestFirst}|_],
	    (
		compare_pos(OldestFirst, DisjPos, Res),
		verify Res = (<),
	    	slots_ending_ge(DisjPos, SortedNoDupSlots, SlotsEnteringDisj),
		SlotsEnteringDisj = [Slot1|_]
	    ->
		% replace with a single summary slot
		append(DisjPos, [?,?], DisjEndPos),
		update_struct(slot, [firstpos:OldestFirst, lastpos:DisjEndPos], Slot1, NewSlot),
		NewSlots = [NewSlot]
	    ;
	    	% all occurrences in current disjunction
	    	% or all before current disjunction
		NewSlots = SortedNoDupSlots
	    )
	),
	compiler_map:from_sorted_assoc_list(NewGroupedList, MergedMap).


    slots_ending_ge(_Pos, [], []).
    slots_ending_ge(Pos, [Slot|Slots], SlotsGe) :-
	Slot = slot{lastpos:LP},
	( compare_pos(LP, Pos, Res) ->
	    verify Res = (<),
	    slots_ending_ge(Pos, Slots, SlotsGe)
	;
	    SlotsGe = [Slot|SlotsGe1],
	    slots_ending_ge(Pos, Slots, SlotsGe1)
	).


% From the candidates in VarIdTable, pick those that (so far) have their only occurrences
% in the chunk before the disjunction at PreDisjPos. 
select_pseudo_arguments(Branches, PredHead, PreDisjPos, Map0, Map, DisjArgs, DisjArity) :-
	vars_in_first_chunks(Branches, VarIdTable),
	hash_list(VarIdTable, VarIds, _),
	(
	    foreach(VarId,VarIds),
	    fromto(Map0,Map1,Map2,Map),
	    param(PreDisjPos,VarIdTable)
	do
	    % a variable that only occurs in PreDisPos can have only one slot
	    (
		compiler_map:search(Map1, VarId, Slots),
		Slots = [slot{firstpos:PreDisjPos,lastpos:LP,class:Location,source_info:Source}]
	    ->
	    	verify PreDisjPos == LP,
		% instantiate VarId's table entry to argument descriptor
		hash_get(VarIdTable, VarId, ArgDesc),
		verify var(ArgDesc),
		ArgDesc = variable{varid:VarId,class:Location,source_info:Source},
		% Classify the pre-disjunction occurrence as nonvoid(temp) here,
		% and remove its entry from the Map.  That way, future
		% occurrences will be considered first occurrences again.
		Location = nonvoid(temp),
		compiler_map:delete(Map1, VarId, Map2)
	    ;
		% Not useful as pseudo-argument: delete it from the candidate table
		hash_delete(VarIdTable, VarId),
	    	Map1 = Map2
	    )
	),
	% Table now contains the varids we want to use as arguments
	hash_count(VarIdTable, IdealDisjArity),
	% For those disjunction-pseudo-args that match clause head args,
	% put them in the same argument position (provided it is not beyond
	% the disjunction's arity)
	( PredHead = HeadArity-HeadArgs ->
	    (
		for(_,1,min(HeadArity,IdealDisjArity)),
		fromto(HeadArgs,[variable{varid:VarId}|HeadArgs1],HeadArgs1,_),
		fromto(DisjArgs,[ArgDesc|DisjArgs1],DisjArgs1,DisjArgs2),
		fromto(RemainingPositions,RemPos1,RemPos2,DisjArgs2),
		param(VarIdTable)
	    do
	    	( hash_remove(VarIdTable, VarId, ArgDesc) ->
		    RemPos1 = RemPos2
		;
		    RemPos1 = [ArgDesc|RemPos2]
		)
	    )
	;
	    verify PredHead==nohead,
	    RemainingPositions = DisjArgs
	),
	DisjArity is min(IdealDisjArity, #wam_registers),
	length(DisjArgs, DisjArity),
	hash_list(VarIdTable, _VarIds, RemainingArgDescs0),
	sort(varid of variable, =<, RemainingArgDescs0, RemainingArgDescs),
	(
	    foreach(ArgDesc,RemainingPositions),
	    fromto(RemainingArgDescs,[ArgDesc|ArgDescs],ArgDescs,Overflow)
	do
	    true
	),
	verify (Overflow==[] ; IdealDisjArity > DisjArity).


% Build a hash map of all VarIds that occur in first chunks of
% the given disjunctive branches. This is just a heuristic, and we do in fact
% look beyond true/0 to catch some special cases like true,cut_to(C) sequences.
vars_in_first_chunks(Branches, Occurs) :-
	hash_create(Occurs),
	(
	    foreach(Branch,Branches),
	    param(Occurs)
	do
	    vars_in_first_chunk(Branch, Occurs)
	).

    vars_in_first_chunk([], _Occurs).
    vars_in_first_chunk([Goal|Goals], Occurs) :-
	( Goal = goal{kind:Kind,args:Args,functor:F} ->
	    vars_in_term(Args, Occurs),
	    ( Kind == regular, F \== true/0 ->
	    	true
	    ;
		vars_in_first_chunk(Goals, Occurs)
	    )
	;
	    true
	).

    vars_in_term([X|Xs], Occurs) :-
	vars_in_term(X, Occurs),
	vars_in_term(Xs, Occurs).
    vars_in_term(variable{varid:VarId}, Occurs) :-
	hash_set(Occurs, VarId, _empty).
    vars_in_term(attrvar{variable:Avar,meta:Meta}, Occurs) :-
	vars_in_term(Avar, Occurs),
	vars_in_term(Meta, Occurs).
    vars_in_term(structure{args:Args}, Occurs) :-
	vars_in_term(Args, Occurs).
    vars_in_term(Term, _Occurs) :- atomic(Term).


% Make head variables for the branch's pseudo-arguments
% Set their source_info field to 'none' because we don't want singleton
% warnings in case they are the only occurrence in a branch and behind.
make_branch_head(_I, [], [], []) :- !.
make_branch_head(I, HeadArgsArray, DisjArgs, HeadArgs) :-
	arg(I, HeadArgsArray, HeadArgs),
	(
	    foreach(variable{varid:VarId,source_info:_Source},DisjArgs),
	    foreach(variable{varid:VarId,source_info:none},HeadArgs)
	do
	    true
	).


:- comment(print_occurrences/1, [
    summary:"Debugging: print result of variable lifetime analysis",
    amode:print_occurrences(+),
    args:[
	"Lifetimes":"A map varid->struct(slot)"
    ],
    see_also:[classify_variables/3]
]).

print_occurrences(Map) :-
	writeln("------ Variable Lifetimes ------"),
	compiler_map:count(Map, N),
	( for(VarId,1,N), param(Map) do
	    compiler_map:lookup(Map, VarId, Slots),
	    printf("Variable #%d:%n", [VarId]),
	    ( foreach(Slot,Slots) do printf("  %w%n", [Slot]) ),
	    nl
	).


%----------------------------------------------------------------------
% This pass does:
% - Variable classification (void, temp, perm)
% - Environment slot allocation
% - Environment size computation:
%	-1  no environment needed
%	 0  empty environment needed
%	>0  environment of given size needed
%
% Environment slots are allocated in a similar way as in the WAM or
% in ECLiPSe I, i.e. ordered according to their last occurrence. This
% means that the environment can shrink during clause execution (whether
% physically by trimming, or virtually - for gc only - by size tracking).
%
% If we have variables local to branches, they can use the same slot as
% other local variables in parallel branches.
% But we do NOT reuse slots for consecutive lifetimes, e.g.
%	p :- p(X), q(X), r(Y), s(Y).
% This could only be done when either determinism information is
% available, or an extra trail check/trailing is accepted:  If there
% were a choicepoint inside p/1 or q/1, reusing X's slot would require
% conditional (value-)trailing of the old slot value before it is reused
% for Y.
%
% A problem is posed by variables whose lifetime starts before a disjunction
% and extends into one or more disjunctive branches (without surviving the
% disjunction): it may not be possible to compute an optimal slot with
% minimal lifetime, because the relative order of the ends of lifetimes
% with other variables may be different in different branches.  We currently
% treat such slots as always surviving until the end of the disjunction,
% but note that environment activity maps contain precise information,
% so that garbage collection is not negatively affected by this.
%----------------------------------------------------------------------

assign_env_slots(Map, EnvSize, Options) :-
	compiler_map:to_assoc_list(Map, MapList),
	% strip keys and flatten
	(
	    foreach(_-Slots,MapList) >> foreach(Slot,Slots),
	    foreach(Slot,FlatSlots)
	do
	    true
	),
	classify_voids_and_temps(FlatSlots, PermSlots, Options),
	% The sorting here is a bit subtle: we rely on the callpos
	% partial order being compatible with the total term order.
	sort(firstpos of slot, >=, PermSlots, SlotsIncStart),
	sort(lastpos of slot, >=, SlotsIncStart, SlotsInc),
	init_branch(Branch),
	foreachcallposinbranch(Branch, SlotsInc, SlotsRest, 0, EnvSize),
	verify SlotsRest==[].


% Deal with the void and temporary variables, and filter them out
classify_voids_and_temps(AllSlots, PermSlots, Options) :-
	(
	    foreach(Slot,AllSlots),
	    fromto(PermSlots,PermSlots2,PermSlots1,[]),
	    param(Options)
	do
	    Slot = slot{firstpos:First,lastpos:Last,class:Loc,source_info:Source},
	    ( var(Loc) ->			% void
		Loc = void,
		singleton_warning(Source, Options),
		PermSlots2=PermSlots1
	    ;
		Loc = nonvoid(Where),	% needs assignment
		verify var(Where),
		( First == Last ->
		    Where = temp,
		    PermSlots2=PermSlots1
		;
		    PermSlots2=[Slot|PermSlots1]
		)
	    )
	).


log_assignment(slot{source_info:Source}, Loc) ?- !,
	( Source = annotated_term{type:var(Name),line:Line} ->
	    printf(log_output, "%w	%w (%d)%n", [Loc,Name,Line])
	;
	    printf(log_output, "%w	%w%n", [Loc,Source])
	).


foreachcallposinbranch(_Branch, [], [], Y, Y).
foreachcallposinbranch(Branch, [Slot|Slots], RestSlots, Y0, Y) :-
	% Branch is list of even length, e.g. [], [7,2]
	% SlotPos is list of odd length, e.g. [7], [7,2,7] but not [7,?,?]
	Slot = slot{lastpos:SlotPos,class:nonvoid(Loc)},
	( append(Branch, RelPos, SlotPos) ->
	    RelPos = [PosInBranch|SubBranch],
	    verify PosInBranch \== ?,
	    ( (SubBranch = [] ; SubBranch = [?,?]) ->
		Y1 is Y0+1, Loc = y(Y1),	% assign env slot
%		log_assignment(Slot, Loc),
		Slots1 = Slots
	    ;
		% SlotPos is deeper down, RelPos=[7,2,7], [7,2,7,?,?] or longer
		% process branches at callpos [7]
		append(Branch, [PosInBranch], Pos),	% nested disjunction
		foreachbranchatcallpos(Pos, [Slot|Slots], Slots1, Y0, Y0, Y1)
	    ),
	    foreachcallposinbranch(Branch, Slots1, RestSlots, Y1, Y)
	;
	    % the first slot does not end in this branch, return
	    RestSlots = [Slot|Slots],
	    Y = Y0
	).

% process all slots that start with Pos
foreachbranchatcallpos(_Pos, [], [], _Y0, Y, Y).
foreachbranchatcallpos(Pos, [Slot|Slots], RestSlots, Y0, Ymax0, Ymax) :-
	% Pos is list of odd length, e.g. [7], [7,2,7], but not [7,?,?]
	% SlotPos is list of odd length, e.g. [7], [7,2,7] but not [7,?,?]
	Slot = slot{lastpos:SlotPos},
	% is Slot in a branch below this callpos? Always true for initial invocation
	( append(Pos, RelPos, SlotPos) ->
	    RelPos = [RelBranch|SubPos],
	    verify RelBranch \== ?,
	    % RelPos is [2,7] or [2,7,?,?] or [2,7,2,7] or longer
	    % which means we are going into branch 2 at Pos
	    ( (SubPos = [_] ; SubPos = [_,?,?]) ->
		append(Pos, [RelBranch], Branch),
		foreachcallposinbranch(Branch, [Slot|Slots], Slots1, Y0, Y1),
		Ymax1 is max(Ymax0,Y1),
		foreachbranchatcallpos(Pos, Slots1, RestSlots, Y0, Ymax1, Ymax)
	    ;
		append(Pos, [_,_], Pos1),	% branch deeper down
		append(Pos1, _, SlotPos),
		foreachbranchatcallpos(Pos1, [Slot|Slots], RestSlots, Y0, Ymax0, Ymax)
	    )
	;
	    % Slot not at this callpos, return
	    RestSlots = [Slot|Slots],
	    Ymax = Ymax0
	).


%----------------------------------------------------------------------
% Computing environment activity maps
%
% We assume that environment slots are already allocated to permanent
% variables. The job of this phase is to compute environment slot activity
% maps for various points in the code, in particular call positions
% and entry and exit points of disjunctive branches. These maps are
% simple bitmaps, with bit i-1 (i>0) indicating that Yi is active.
%
% We make a forward and a backward pass through the directed acyclic
% graph formed by the normalised clause. During the forward pass, we
% annotate every goal with two sets:
%	- seen_before (the slots that occurred before this goal)
%	- seen_here (the slots that occur in this goal)
%
% Then we make a backward pass to discover the last occurrences and
% compute the actual environment activity maps. With the current strategy
% of globalising all environment variables, a slot's activity ends at
% the call that has its last occurrence(s).
%----------------------------------------------------------------------

% Auxiliary structures built during forward pass, and traversed backward
:- local struct(rev_goal(	% wrapper for goal{}
    	goal,		% the goal{} all this belongs to
	max_y,		% max y slot accessed in this goal
	seen_before,	% bitmap of variables seen before this goal
	seen_here)	% bitmap of variables occurring in this goal
    ).

:- local struct(rev_disj(	% wrapper for disjunction{}
	disjunction,	% the disjunction{} all this belongs to
    	rev_branches,	% list of reversed branches, for backward traversal
	max_y_setup,	% max y slot in setup before disjunction entry
	max_y_heads,	% max y slot accessed in all branch heads
	seen_before,	% bitmap of variables seen before this branch
	seen_at_end,	% bitmap of variables seen at end of each branch
	seen_at_ends)	% list of bitmaps of variables seen at end of each branch
    ).


mark_env_activity(Clause, MaxEnvSize) :-
	mark_env_activity_fwd(Clause, 0, _Before, [], Reverse),
	mark_env_activity_bwd(Reverse, 0, _After, -1, EntryEnvSize),
	verify MaxEnvSize =:= max(EntryEnvSize,0).


mark_env_activity_fwd([], Seen, Seen, Reverse, Reverse).
mark_env_activity_fwd([Goal|Goals], Seen0, Seen, Reverse0, Reverse) :-
	mark_env_activity_fwd(Goal, Seen0, Seen1, Reverse0, Reverse1),
	mark_env_activity_fwd(Goals, Seen1, Seen, Reverse1, Reverse).
mark_env_activity_fwd(Disjunction, Seen0, Seen, Reverse, [RevDisj|Reverse]) :-
	Disjunction = disjunction{branches:Branches,args:DisjArgs,
		branchheadargs:HeadArgsArray,indexvars:IndexVars},
	RevDisj = rev_disj{rev_branches:RevBranches,disjunction:Disjunction,
		max_y_setup:MaxYSetup, max_y_heads:MaxYHeads,
		seen_before:SeenBefore, seen_at_end:Seen, seen_at_ends:SeenEnds},
	mark_env_activity_args(IndexVars, Seen0, Seen1, -1, MaxY1),
	mark_env_activity_args(DisjArgs, Seen1, SeenBefore, MaxY1, MaxYSetup),
	(
	    foreach(Branch,Branches),
	    foreach(RevBranch,RevBranches),
	    foreach(SeenEndBranch,SeenEnds),
	    fromto(SeenBefore,Seen3,Seen4,Seen),
	    fromto(-1,MaxY1,MaxY2,MaxYHeads),
	    count(BranchI,1,_),
	    param(SeenBefore,HeadArgsArray)
	do
	    ( HeadArgsArray == [] ->
		SeenAfterHead = SeenBefore, MaxY1 = MaxY2
	    ;
		arg(BranchI, HeadArgsArray, HeadArgs),
		mark_env_activity_args(HeadArgs, SeenBefore, SeenAfterHead, MaxY1, MaxY2)
	    ),
	    mark_env_activity_fwd(Branch, SeenAfterHead, SeenEndBranch, [], RevBranch),
	    Seen4 is Seen3 \/ SeenEndBranch
	).
mark_env_activity_fwd(Goal, Seen0, Seen, Reverse, [RevGoal|Reverse]) :-
	Goal = goal{args:Args},
	RevGoal = rev_goal{max_y:MaxY,seen_here:UsedHere,seen_before:Seen0,goal:Goal},
	mark_env_activity_args(Args, 0, UsedHere, -1, MaxY),
	Seen is Seen0 \/ UsedHere.


    :- mode mark_env_activity_args(+,+,-,+,-).
    mark_env_activity_args([], EAM, EAM, MaxY, MaxY).
    mark_env_activity_args([X|Xs], EAM0, EAM, MaxY0, MaxY) :-
	mark_env_activity_term(X, EAM0, EAM1, MaxY0, MaxY1),
	mark_env_activity_args(Xs, EAM1, EAM, MaxY1, MaxY).

    :- mode mark_env_activity_term(+,+,-,+,-).
    mark_env_activity_term(Var, EAM0, EAM, MaxY0, MaxY) :-
	Var = variable{class:Loc},
	( Loc = nonvoid(y(Y)) ->
	    EAM is setbit(EAM0, Y-1),		% set the seen-flag
	    MaxY is max(MaxY0,Y)
	;
	    EAM0=EAM, MaxY0=MaxY
	).
    mark_env_activity_term(attrvar{variable:Avar,meta:Meta}, EAM0, EAM, MaxY0, MaxY) :-
	mark_env_activity_term(Avar, EAM0, EAM1, MaxY0, MaxY1),
	mark_env_activity_term(Meta, EAM1, EAM, MaxY1, MaxY).
    mark_env_activity_term([X|Xs], EAM0, EAM, MaxY0, MaxY) :-
	mark_env_activity_term(X, EAM0, EAM1, MaxY0, MaxY1),
	mark_env_activity_term(Xs, EAM1, EAM, MaxY1, MaxY).
    mark_env_activity_term(structure{args:Args}, EAM0, EAM, MaxY0, MaxY) :-
	mark_env_activity_term(Args, EAM0, EAM, MaxY0, MaxY).
    mark_env_activity_term(Term, EAM, EAM, MaxY, MaxY) :- atomic(Term).



% Backwards traversal of the clause DAG to discover last occurrences.
% Using the auxiliary data structure created during the forward pass,
% and the seen_before/seen_here-fields filled in during the forward pass.

mark_env_activity_bwd([], After, After, ESize, ESize).
mark_env_activity_bwd([Goal|Goals], After0, After, ESize0, ESize) :-
	mark_env_activity_bwd(Goal, After0, After1, ESize0, ESize1),
	mark_env_activity_bwd(Goals, After1, After, ESize1, ESize).
mark_env_activity_bwd(rev_disj{rev_branches:Branches,
			max_y_setup:MaxYSetup,
			max_y_heads:MaxYHeads,
			seen_before:SeenBeforeDisj,
			seen_at_end:SeenEndDisj,
			seen_at_ends:SeenEnds,
			disjunction:disjunction{
			    entrymap:DisjEntryEAM,
			    exitmap:DisjExitEAM,
			    entrysize:EntryESize,
			    exitsize:ExitESize,
			    branchentrymaps:BranchEntryEamArray,
			    branchinitmaps:BranchExitInits}},
		After0, After, ExitESize, ESize) :-
	% EAM after exiting the disjunction
	DisjExitEAM is SeenEndDisj /\ After0,
	(
	    foreach(Branch,Branches),
	    foreach(SeenEnd,SeenEnds),
	    foreach(BranchEntryEAM,BranchEntryEAMs),
	    foreach(BranchExitInit,BranchExitInits),
	    fromto(After0,After1,After2,After),
	    fromto(ExitESize,ESize1,ESize2,ESize3),
	    param(SeenBeforeDisj,After0,ExitESize,DisjExitEAM)
	do
	    % slots that are active after the disjunction, but not
	    % at the end of the branch, must be initialised
	    % on branch exit!
	    BranchExitEAM is SeenEnd /\ After0,
	    BranchExitInit is DisjExitEAM /\ \BranchExitEAM,
	    mark_env_activity_bwd(Branch, After0, BranchAndAfter, ExitESize, BranchEntryESize),
	    ESize2 is max(ESize1,BranchEntryESize),
	    BranchEntryEAM is SeenBeforeDisj /\ BranchAndAfter,
	    After2 is After1 \/ BranchAndAfter
	),
	% EntryESize is at least 0 because disjunctions are assumed
	% to be regular and require at least an empty environment
	EntryESize is max(0,max(ESize3,MaxYHeads)),
	ESize is max(EntryESize,MaxYSetup),
	% EAM before entering the disjunction
	DisjEntryEAM is SeenBeforeDisj /\ After,
	BranchEntryEamArray =.. [[]|BranchEntryEAMs].
mark_env_activity_bwd(rev_goal{max_y:MaxY,seen_before:Before,seen_here:UsedHere,goal:Goal}, After0, After, ESize0, ESize) :-
	Goal = goal{envmap:EAM,envsize:ESize0},
	ESize is max(max(0,ESize0),MaxY),	% need at least empty environment
	% if variables were not globalised, slots would remain active during call:
%	EAM is UsedHere \/ (Before /\ After0),
	% when unsafe variables are globalised, slots are released on call:
	EAM is (UsedHere \/ Before) /\ After0,
	After is After0 \/ UsedHere.

