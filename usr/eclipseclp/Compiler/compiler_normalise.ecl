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
% Version:	$Id: compiler_normalise.ecl,v 1.24 2013/02/26 01:21:20 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_normalise).

:- comment(summary, "ECLiPSe III compiler - source code normaliser").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf, Kish Shen").
:- comment(date, "$Date: 2013/02/26 01:21:20 $").

:- comment(desc, html("
	This module creates the normalised form of the source predicate on
	which the subsequent compiler passes do their work.
	<P>
	The clause structure is destroyed and replaced by inline disjunction.
	The idea being that the compiler will treat disjunctions such that
	we don't lose anything by doing so.
	<P>
	The decision to treat disjunctions directly implies that the bodies
	have forks and joins of control flow, which makes everything much
	more complicated compared to a conjunction-only clause compiler.
	The upside is that there is more scope for optimization, e.g.
	more sharing of the same environment, less data movement.
	<P>
	In the normalised form, the code is generally put in to a form that
	is more convenient for later processing:
	<UL>
	<LI>wrappers identify type of item
	<LI>lists of goals rather than comma/semicolon structures
	</UL>
	The constituents of a normalised source are:
	<PRE>
	Pred ::=	Conjunction

	Conjunction ::=	[Goal|...]

	Goal ::=	SimpleGoal
		|	Disjunction

	SimpleGoal ::=	goal{Term, ...}		% also used for head

	Disjunction ::=	disjunction{Branches, ...}

	Branches ::=	[Conjunction|...]

	Term ::=	variable{...}
		|	structure{...}
		|	ground_structure{...}	% not yet done
		|	[Term|...]		% list
		|	AtomicTerm		% atomic terms literally
	</PRE>

")).

:- lib(hash).

:- use_module(compiler_common).

:- import print_goal_state/3 from compiler_analysis.

:- import
	meta_index/2,
	get_attribute/3
   from sepia_kernel.


% Utilities to deal with (optionally uninstantiated) annotated terms
:- local op(700, xfx, =:).
Var =: _Template :- var(Var), !.
%Term =: Term2 :- verify instance(Term, Term2), Term = Term2.	% SLOW!
Term =: Term.

varg(I, T, A) :- ( var(T) -> true ; arg(I, T, A) ).

ann_update_term(_NewTermAnn, Ann, _NewAnn) :- var(Ann), !.
ann_update_term(NewTermAnn, Ann, NewAnn) :-
	type_of(NewTermAnn, Type),
	update_struct(annotated_term, [term:NewTermAnn,type:Type], Ann, NewAnn).

ann_location(Ann, '', 0, 0, 0) :- var(Ann), !.
ann_location(annotated_term{file:File,line:Line,from:From,to:To}, File, Line, From, To).
		

%----------------------------------------------------------------------

:- export normalize_clauses_annotated/6.
:- comment(normalize_clauses_annotated/6, [
    summary:"Transforms a list of clauses into the normalised predicate representation",
    args:[
	"Clauses":"A list of clauses for a single predicate",
	"AnnClauses":"A corresponding list of annotated clauses (or uninstantated)",
	"Normalised":"The normalised, ground form of the predicate",
	"VarCount":"Number of distinct variables in the normalised code",
	"Options":"Options",
	"Module":"Context module"
    ],
    amode:normalize_clauses_annotated(+,+,-,-,+,+),
    desc:ascii("
	Build the ground representation of a single predicate (clause list).

	This deals with:
	    -	replacing variables with descriptors (struct variable())
	    	and assigning an integer variable identifier.
		There is a fresh descriptor for every occurrence!
	    -	wrapping structures into descriptors (struct structure())
	    -	flattening conjunctions and disjunctions
	    -	modules (lookup and context), including tool->tool_body
	    -	variable goals -> metacall
	    -	classifying goals as simple or regular

	A normalised body or any other conjunction is a list of subgoals.
	A normalised disjunction is a struct disjunction()
	A normalised other subgoal is a struct goal()
    ")
]).

normalize_clauses_annotated(Clauses, AnnClauses, NormClauses, VarCount, Options, Module) :-
	normalize_clause_list(Clauses, AnnClauses, NormClauses, VarCount, Options, Module).
%	reorder_goals(NormClauses0, NormClauses).


%----------------------------------------------------------------------
% Method for dealing with variables:
% In the normalised representation, variables are replaced by
% variable{} descriptors, initially with uninstantiated varid-fields.
% A list of these descriptors is collected, and used at the end by
% assign_varids/3 to fill in the varid-fields.
%
% Handling of cuts in control constructs:
%	call(!)		local effect
%	once(!)		local effect
%	not(!)		local effect
%	(! -> ...)	local effect (was error in ECLiPSe I)
%	(... -> !)	global effect
%	(! ; ...)	global effect
%	(... ; !)	global effect
%
% Handling of 'true':
%	true/0 should really be removed completely in this step. However,
%	historically true/0 has been a regular goal in ECLiPSe, and
%	is used to force waking before cuts, etc.  So removing it completely
%	would break old code in subtle ways. We therefore remove it only if
%	it is followed by a regular goal (including disjunction and cut),
%	or if it occurs at the end of a branch.
%
% Disjunctions:
%	Disjunctions are flattened as much as possible.
%----------------------------------------------------------------------

:- mode normalize_body(?,?,+,+,-,+,+,-,-,+,+,+,+).
normalize_body(Var, AnnVar, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM, _Last) :-
	var(Var), !,
	ann_update_term(call(AnnVar), AnnVar, AnnCall),
	normalize_goal(call(Var), AnnCall, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM).

normalize_body(true, _Ann, _Branch, CallNr, CallNr, _Cut, Vs, Vs, Goals, Goals, _LMP, _CM, last) :- !.
	% remove true/0 at end of branches

%normalize_body(call(G), Ann, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, Goals0, Goals, _LMP, CM, Last) :-
%	nonvar(G), !,
%	Goals0 = [SavecutGoal|Goals1],
%	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
%	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
%	% cuts have local effect!
%        Ann =: annotated_term{term:call(AnnG)},
%        normalize_body(G, AnnG, Branch, CallNr1, CallNr, LocalCut, Vs1, Vs, Goals1, Goals, CM-any, CM, Last).

normalize_body(once(G), Ann, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, Goals0, Goals, _LMP, CM, _Last) :- !,
	Goals0 = [SavecutGoal|Goals1],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
	% cuts have local effect!
        Ann =: annotated_term{term:once(AG)},
        normalize_body(G, AG, Branch, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals1, Goals2, CM-any, CM, any),
	Goals2 = [CuttoGoal|Goals],
	same_call_pos(Branch, CallNr2, CallNr, CallPos1),
	cutto_goal(CallPos1, Vs2, Vs, LocalCut, CuttoGoal).

normalize_body(not(G), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LMP, CM, Last) :-
	LMP=LM-_, get_flag((not)/1, definition_module, sepia_kernel)@LM,
	!,
        Ann =: annotated_term{term:not(AG)},
        ann_update_term(\+AG, Ann, Ann0),
        normalize_body(\+G, Ann0, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LMP, CM, Last).

normalize_body(\+G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LMP, CM, Last) :- !,
        Ann =: annotated_term{term:(\+AG)},
        ann_update_term(fail, Ann, AnnFail),
        ann_update_term(true, Ann, AnnTrue),
        ann_update_term((AG->AnnFail), Ann, AnnCond),
        ann_update_term((AnnCond;AnnTrue), Ann, AnnITE),
        normalize_body((G->fail;true), AnnITE, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LMP, CM, Last).

normalize_body((G1->G2), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, _LMP, CM, _Last) :- !,
	% this is a ->/2 that's _not_ inside a ;/2
	Goals0 = [SavecutGoal|Goals1],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
	% cuts have local effect!
        Ann =: annotated_term{term:(AG1->AG2)},
        normalize_body(G1, AG1, Branch, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals1, Goals2, CM-any, CM, any),
	Goals2 = [CuttoGoal|Goals3],
	same_call_pos(Branch, CallNr2, CallNr3, CallPos1),
	cutto_goal(CallPos1, Vs2, Vs3, LocalCut, CuttoGoal),
	normalize_body(G2, AG2, Branch, CallNr3, CallNr, Cut, Vs3, Vs, Goals3, Goals, CM-any, CM, last).

	% TODO: compile softcut!  Preliminary: metacall it.
normalize_body((G1*->G2;G3), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM, Last) ?-
	LMP=LM-_, get_flag((*->)/2, definition_module, sepia_kernel)@LM,
	!,
	ann_update_term(call(Ann), Ann, AnnCall),
	normalize_body(call(G1*->G2;G3), AnnCall, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM, Last).

normalize_body((G1;G2), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LMP, CM, _Last) :- !,
	Goals = [SavecutGoal,disjunction{callpos:CallPos,branches:Branches}|Goals0],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, DisjCut, SavecutGoal),
	new_call_pos(Branch, CallNr1, CallNr2, _CallPos),
	new_call_pos(Branch, CallNr2, CallNr, CallPos),
        Ann =: annotated_term{term:(AG1;AG2)},
	normalize_left_branch(G1, AG1, CallPos, 1, BranchNr1, Cut, DisjCut, Vs1, Vs2, Branches, Branches1, CM-any, CM),
	normalize_right_branch(G2, AG2, CallPos, BranchNr1, _NBranches, Cut, DisjCut, Vs2, Vs, Branches1, [], CM-any, CM).

normalize_body((G1,G2), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LMP, CM, Last) :- !,
	% this could be changed such that the lookup module propagates
	% through the comma (would be incompatible with Eclipse =< 5)
	Ann =: annotated_term{term:(AG1,AG2)},
	( G1 == true, starts_regular(G2, CM) ->
	    % a true followed by a regular goal: eliminate the true/0
	    normalize_body(G2, AG2, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, CM-any, CM, Last)
	;
	    normalize_body(G1, AG1, Branch, CallNr0, CallNr1, Cut, Vs0, Vs1, Goals, Goals1, CM-any, CM, any),
	    normalize_body(G2, AG2, Branch, CallNr1, CallNr, Cut, Vs1, Vs, Goals1, Goals0, CM-any, CM, Last)
	).

normalize_body(G@M, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, _CM, Last) :-
	LMP=LM-_, get_flag((@)/2, definition_module, sepia_kernel)@LM,
        !,
	% this could be changed such that the lookup module propagates
	% through the @ (would be incompatible with Eclipse =< 5)
	Ann =: annotated_term{term:(AG@_AM)},
        ( atom(M) ->
            normalize_body(G, AG, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, M, Last)
	; var(G) ->
	    ann_update_term(call(AG), Ann, AnnCall),
	    normalize_goal(call(G), AnnCall, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, M)
	;
	    normalize_goal(G, AG, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, M)
	).

normalize_body(LM:G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM, Last) :-
	atom(LM), nonvar(G),
	LMP=LMC-_, get_flag((:)/2, definition_module, sepia_kernel)@LMC,
	!,
        Ann =: annotated_term{term:(_ALM:AG)},
        normalize_body(G, AG, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM-exported, CM, Last).

normalize_body(!, _Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LMP, _CM, _Last) :- !,
	Goals = [CuttoGoal|Goals0],
	same_call_pos(Branch, CallNr0, CallNr, CallPos),
	cutto_goal(CallPos, Vs0, Vs, Cut, CuttoGoal).

normalize_body(X=Y, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM, _Last) :- !,
        simplify_unification(X=Y, Ann, UnifGoals, AnnUnifGoals),
	(
	    foreach(UnifGoal, UnifGoals),
	    foreach(AnnUnifGoal, AnnUnifGoals),
	    fromto(CallNr0,CallNr1,CallNr2,CallNr),
	    fromto(Vs0,Vs1,Vs2,Vs),
	    fromto(Goals,Goals1,Goals2,Goals0),
	    param(Branch,Cut,LMP,CM)
	do
	    normalize_goal(UnifGoal, AnnUnifGoal, Branch, CallNr1, CallNr2, Cut, Vs1, Vs2, Goals1, Goals2, LMP, CM)
	).

normalize_body(G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM, _Last) :-
        normalize_goal(G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LMP, CM).


normalize_goal(G, AnnG, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, [Goal|Goals], Goals, LM-Vis, CM) :-
	callable(G),
	!,
	AnnG =: annotated_term{term:GAnn},
	ann_location(AnnG, File, Line, From, To),
	Goal = goal{
	    kind:Kind,
	    callpos:CallPos,
	    definition_module:DM,
	    lookup_module:LM1,
	    functor:N1/A1,
	    args:NormArgs,
            path:File,
            line:Line,
            from:From,
            to:To
	},
	functor(G, N, A),
	get_pred_info(Vis, LM, N/A, DM, ToolBody, CallType),
	( ToolBody = N1/A1 -> 			% replace tool with tool body
	    LM1=DM,
	    ann_update_term(CM, AnnG, AnnCM),
	    normalize_term(CM, AnnCM, NormCM, Vs0, Vs1, =),	% CM may be a variable
	    ModuleArg = [NormCM]
	;
	    N1=N, A1=A, LM1=LM,
	    Vs1=Vs0, ModuleArg = []
	),
	( CallType = external ->
	    same_call_pos(Branch, CallNr0, CallNr, CallPos),
	    Kind = simple
	;
	    new_call_pos(Branch, CallNr0, CallNr, CallPos),
	    Kind = regular
	),
	(					% normalize arguments
	    for(I,1,A),
	    fromto(NormArgs,[NormArg|MoreArgs],MoreArgs,ModuleArg),
	    fromto(Vs1,Vs2,Vs3,Vs),
	    param(G,GAnn)
	do
	    arg(I, G, Arg),
	    varg(I, GAnn, AnnArg),
	    normalize_term(Arg, AnnArg, NormArg, Vs2, Vs3, =)
	    % ( Vs2==Vs3 -> term is ground ; true )
	).
normalize_goal(G, AnnG, _, _, _, _, _, _, _, _, LM, _) :-
	compiler_event(#illegal_goal, term, AnnG, G, LM).
%	compiler_error(AnnG, term, "Illegal goal: %QVw", [G]).


    % Hack: check whether a preceding true/0 can be eliminated
    % Fail for simple goals, control constructs, and when in doubt.
    starts_regular((G,_), LM) ?- !, starts_regular(G, LM).
    starts_regular(LM:G, _LM) ?- !, ( var(LM) -> true ; starts_regular(G, LM) ).
    starts_regular(G@CM, LM) ?- !, ( var(CM) -> true ; starts_regular(G, LM) ).
    starts_regular(!, _) ?- !, fail.
    starts_regular(cut_to(_), _) ?- !, fail.
    starts_regular((_;_), _) ?- !, fail.
    starts_regular((_->_), _) ?- !, fail.
    starts_regular(not(_), _) ?- !, fail.
    starts_regular(\+(_), _) ?- !, fail.
    starts_regular(once(_), _) ?- !, fail.
    starts_regular(Goal, LM) :-
	callable(Goal),
	current_module(LM),
	functor(Goal, F, N),
	% also succeeds if F/N not defined yet
	\+ get_flag(F/N, call_type, external)@LM.


    % Look up relevant properties of the called predicate.
    % If it is not known yet, assume defaults (regular, non-tool, []-module).
    get_pred_info(ReqVis, LM, Pred0, DM, ToolBody, CallType) :-
	(
	    current_module(LM),
            virtual_pred(Pred0, Pred, Extra),
	    get_flag(Pred, visibility, Vis)@LM,
	    required_visibility(ReqVis, Vis)
	->

	    ( get_flag(Pred, tool, on)@LM ->
		tool_body(Pred, Body/N, DM)@LM,
                N1 is N+Extra,
                ToolBody = Body/N1
	    ;
		ToolBody = none,
		get_flag(Pred, definition_module, DM)@LM
	    ),
	    get_flag(Pred, call_type, CallType)@LM
	;
	    % Nothing known about Pred, assume defaults
	    DM = [], ToolBody = none, CallType = prolog
	).

    required_visibility(any, _) :- !.
    required_visibility(exported, exported) :- !.
    required_visibility(exported, reexported).

    % Pretend existence of call/Any and :/Any
    virtual_pred(Pred, Pred, 0).
    virtual_pred(call/N, call/1, Extra) :- N>1, Extra is N-1.
    virtual_pred((:)/N, (:)/2, Extra) :- N>2, Extra is N-2.


normalize_left_branch((G1->G2), Ann, DisjCallPos, BranchNr0, BranchNr, Cut, DisjCut, Vs0, Vs, Branches, Branches0, _LM, CM) ?- !,
	% we have an if-then-else (a branch of the disjunction that gets cut)
	Branches = [[SavecutGoal|Goals]|Branches0],
	new_branch(DisjCallPos, BranchNr0, BranchNr, BranchPos),
	same_call_pos(BranchPos, 1, CallNr1, CallPos1),
	savecut_goal(CallPos1, Vs0, Vs1, LocalCut, SavecutGoal),
        Ann =: annotated_term{term:(AG1->AG2)},
	normalize_body(G1, AG1, BranchPos, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals, Goals1, CM-any, CM, any),
	Goals1 = [CuttoGoal|Goals2],
	same_call_pos(BranchPos, CallNr2, CallNr3, CallPos2),
	cutto_goal(CallPos2, Vs2, Vs3, DisjCut, CuttoGoal),
	normalize_body(G2, AG2, BranchPos, CallNr3, _CallNr, Cut, Vs3, Vs, Goals2, [], CM-any, CM, last).
normalize_left_branch((G1;G2), Ann, DisjCallPos, BranchNr0, BranchNr, Cut, DisjCut, Vs0, Vs, Branches, Branches0, _LM, CM) ?-
	% An if-then-else in the left alternative needs its own choicepoint!
	nonvar(G1), G1 \= (_ -> _),
	!,
	% This disjunction can be merged with the parent one
        Ann =: annotated_term{term:(AG1;AG2)},
        normalize_left_branch(G1, AG1, DisjCallPos, BranchNr0, BranchNr1, Cut, DisjCut, Vs0, Vs1, Branches, Branches1, CM-any, CM),
	normalize_right_branch(G2, AG2, DisjCallPos, BranchNr1, BranchNr, Cut, DisjCut, Vs1, Vs, Branches1, Branches0, CM-any, CM).
normalize_left_branch(G1, Ann, DisjCallPos, BranchNr0, BranchNr, Cut, _DisjCut, Vs0, Vs, [Goals|Branches], Branches, LM, CM) :-
	% A normal (uncut) branch of the disjunction
	new_branch(DisjCallPos, BranchNr0, BranchNr, BranchPos),
	normalize_body(G1, Ann, BranchPos, 1, _CallNr, Cut, Vs0, Vs, Goals, [], LM, CM, last).


normalize_right_branch((G1;G2), Ann, DisjCallPos, BranchNr0, BranchNr, Cut, LocalCut, Vs0, Vs, Branches, Branches0, _LM, CM) ?- !,
	% This disjunction can be merged with the parent one
        Ann =: annotated_term{term:(AG1;AG2)},
        normalize_left_branch(G1, AG1, DisjCallPos, BranchNr0, BranchNr1, Cut, LocalCut, Vs0, Vs1, Branches, Branches1, CM-any, CM),
	normalize_right_branch(G2, AG2, DisjCallPos, BranchNr1, BranchNr, Cut, LocalCut, Vs1, Vs, Branches1, Branches0, CM-any, CM).
normalize_right_branch(G1, Ann, DisjCallPos, BranchNr0, BranchNr, Cut, _LocalCut, Vs0, Vs, [Goals|Goals0], Goals0, LM, CM) :-
	new_branch(DisjCallPos, BranchNr0, BranchNr, Branch),
	normalize_body(G1, Ann, Branch, 1, _CallNr, Cut, Vs0, Vs, Goals, [], LM, CM, last).


% Normalise a list of standard clauses, facts, etc

% normalize_clause_list(+Clauses, ?AnnClauses, -NormClause, -VarCount, +Options, +CM)
normalize_clause_list([Clause], [AnnClause], NormClause, VarCount, Options, CM) :- !,
	NormClause = [HeadMarker, SavecutGoal |NormClause1],
	same_call_pos([], 1, _CallNr, CallPos),
	head_marker(Clause, CM, CallPos, HeadMarker, Arity),
	savecut_goal(CallPos, Vs, Vs1, Cut, SavecutGoal),
	normalize_clause(Clause, AnnClause, [], NormClause1, CM, Cut, Vs1, []),
	assign_varids(Vs, Arity, VarCount, Options).
normalize_clause_list(Clauses, AnnClauses, NormClauses, VarCount, Options, CM) :-
	Clauses = [SomeClause|_],
	NormClauses = [
	    HeadMarker,
	    SavecutGoal, 
	    disjunction{callpos:CallPos,branches:NormBranches}
	],
	same_call_pos([], 1, CallNr1, CallPos0),
	head_marker(SomeClause, CM, CallPos0, HeadMarker, Arity),
	savecut_goal(CallPos0, Vs0, [], Cut, SavecutGoal),
	new_call_pos([], CallNr1, CallNr2, _CallPos),
	new_call_pos([], CallNr2, _CallNr, CallPos),
%	new_call_pos([], CallNr1, _CallNr, CallPos),
	(
	    foreach(Clause,Clauses),
            foreach(AnnClause,AnnClauses),
	    foreach(Goals,NormBranches),
	    fromto(1,BranchNr1,BranchNr2,_BranchNr),
	    fromto(Arity,VarId1,VarId2,VarCount),
	    param(CallPos,CM,Cut,Vs0,Options)
	do
	    new_branch(CallPos, BranchNr1, BranchNr2, ClauseBranch),
	    normalize_clause(Clause, AnnClause, ClauseBranch, Goals, CM, Cut, Vs, Vs0),
	    assign_varids(Vs, VarId1, VarId2, Options)
	).

    normalize_clause(Clause, AnnClause, Branch, Goals, CM, Cut, Vs, Vs0) :-
	clause_head_body(Clause, AnnClause, Head, Body, AnnHead, AnnBody, HeadType, CM),
	same_call_pos(Branch, 1, CallNr, CallPos),
	normalize_head(HeadType, Head, AnnHead, CallPos, Goals, Goals1, Vs, Vs1),
	normalize_body(Body, AnnBody, Branch, CallNr, _CallNr, Cut, Vs1, Vs0, Goals1, [], CM-any, CM, last).

    :- mode clause_head_body(+,?,-,-,-,-,-,+).
    clause_head_body((H0:- -?->B0), Ann, H, B, AH, AB, HeadType, CM) ?-
	get_flag((?-)/2, definition_module, sepia_kernel)@CM,
	!,
        Ann =: annotated_term{term:(AH:-AnnMatch)},
        AnnMatch =: annotated_term{term:(-?->AB)},
	H=H0, B=B0, HeadType = (?=).
    clause_head_body((H0:-B0), Ann, H, B, AH, AB, HeadType, _CM) ?- !,
        Ann =: annotated_term{term:(AH:-AB)},
	H=H0, B=B0, HeadType = (=).
    clause_head_body((H0?-B0), Ann, H, B, AH, AB, HeadType, CM) ?-
	get_flag((?-)/2, definition_module, sepia_kernel)@CM,
	!,
        Ann =: annotated_term{term:(AH?-AB)},
	H=H0, B=B0, HeadType = (?=).
    clause_head_body(H, AH, H, true, AH, AnnTrue, =, _CM) :-
        ann_update_term(true, AH, AnnTrue).


head_marker(Clause, CM, CallPos, Goal, Arity) :-
	Goal = goal{
	    kind:head, callpos:CallPos,
	    lookup_module:CM, definition_module:CM,
	    functor:N/Arity, args:HeadArgs},
    	clause_head_body(Clause, _, H, _, _, _, _, CM),
	functor(H, N, Arity),
	(
	    for(I,1,Arity),
	    foreach(HeadArg,HeadArgs)
	do
	    new_vardesc(I, HeadArg)
	).


% Create a goal to save the cut position in a new variable
% This will be optimized away later if the variable remains unused

savecut_goal(CallPos, Vs0, Vs1, CutVar, Goal) :-
	Goal = goal{
	    kind:simple,
	    callpos:CallPos,
	    definition_module:sepia_kernel,
	    lookup_module:sepia_kernel,
	    functor:get_cut/1,
	    args:[NormCutVar]
	},
	new_aux_variable(CutVar, NormCutVar, Vs0, Vs1).


cutto_goal(CallPos, Vs0, Vs1, CutVar, Goal) :-
	Goal = goal{
	    kind:simple,
	    callpos:CallPos,
	    definition_module:sepia_kernel,
	    lookup_module:sepia_kernel,
	    functor:cut_to/1,
	    args:[NormCutVar]
	},
	normalize_term(CutVar, _, NormCutVar, Vs0, Vs1, =).



% Normalised term representation:
%
%	variables	struct(variable)
%	attr.variables	struct(attrvar) - only if AttrFlag == (?=) !
%	atomic		as such
%	lists		as such
%	structs		structure(structure)
%	ground struct	ground_structure(ground)   TODO

:- mode normalize_term(?,?,-,-,+,+).
normalize_term(X, AnnX, Desc, Vs, Vs0, AttrFlag) :-
	var(X), !,
	normalize_var(X, AnnX, Desc, Vs, Vs0, AttrFlag, _VarId).
normalize_term([X|Xs], Ann, [Y|Ys], Vs, Vs0, AttrFlag) :- !,
	Ann =: annotated_term{term:[AnnX|AnnXs]},
	normalize_term(X, AnnX, Y, Vs, Vs1, AttrFlag),
	normalize_term(Xs, AnnXs, Ys, Vs1, Vs0, AttrFlag).
normalize_term(X, Ann, structure{name:N,arity:A,args:Args}, Vs, Vs0, AttrFlag) :-
	compound(X), !,
	Ann =: annotated_term{term:AnnX},
	functor(X, N, A),
	(
	    for(I,1,A),
	    foreach(NormArg,Args),
	    fromto(Vs,Vs2,Vs1,Vs0),
	    param(X,AnnX,AttrFlag)
	do
	    arg(I, X, Arg),
	    varg(I, AnnX, AnnArg),
	    normalize_term(Arg, AnnArg, NormArg, Vs2, Vs1, AttrFlag)
	).
normalize_term(X, _Ann, X, Vs, Vs, _AttrFlag) :- atom(X), !.
normalize_term(X, _Ann, X, Vs, Vs, _AttrFlag) :- number(X), !.
normalize_term(X, _Ann, X, Vs, Vs, _AttrFlag) :- string(X), !.
normalize_term(X, Ann, _, _, _, _) :-
	( nonvar(Ann) ->
	    Ann =: annotated_term{file:File,line:Line},
	    printf(error, "File %w, line %d: ", [File,Line])
	;
	    true
	),
	type_of(X, Type),
	printf(error, "Cannot compile term of type %w: %w%n", [Type,X]),
	exit_block(abort_compile_predicate).


normalize_var(X, AnnX, Desc, [X-VarDesc|Vs1], Vs0, AttrFlag, VarId) :-
	( nonvar(AnnX) ->
	    VarDesc = variable{source_info:AnnX,varid:VarId}
	;
	    VarDesc = variable{source_info:none,varid:VarId}
	),
	( meta(X), AttrFlag = (?=) ->
	    Desc = attrvar{variable:VarDesc,meta:NormMeta},
	    meta_attr_struct(X, Meta),
	    normalize_term(Meta, _Ann, NormMeta, Vs1, Vs0, AttrFlag)
	;
	    % treat as plain variable when not in matching-clause head
	    Desc = VarDesc, Vs1 = Vs0
	).


% Build a structure meta/N with all attributes of X
meta_attr_struct(X, Meta) :-
	meta_attributes(X, 1, Attrs),
	Meta =.. [meta|Attrs].

    meta_attributes(X, I, Attrs) :-
	( meta_index(_Name,I) ->
	    get_attribute(X, Attr, I),
	    Attrs = [Attr|Attrs1],
	    I1 is I+1,
	    meta_attributes(X, I1, Attrs1)
	;
	    Attrs = []
	).


% Introduce a new, auxiliary source variable that was not in the
% original source, or an additional occurrence of a source variable.
% Treat occurrences like normal source variables.
new_aux_variable(X, VarDesc, [X-VarDesc|Vs], Vs) :-
	VarDesc = variable{source_info:none}.



% Fill in the variable{varid:} fields with a unique integer >= 1
% for every distinct variable.  Variables that occur in argument positions
% will already have numbers assigned - simply copy that to every
% descriptor for that variable.  Note that we have not yet separated
% identical variables that occur in parallel branches of a disjunction,
% that is taken care of in the variable classification pass.

assign_varids(Vs, N0, N, Options) :-
	keysort(Vs, SortedVs),
	(
	    foreach(X-variable{varid:VarId,source_info:Source},SortedVs),
	    fromto(_OtherVar,X0,X,_),
	    fromto(0,VarId0,VarId,VarIdN),
	    fromto(none,Source0,Source,SourceN),
	    fromto(multi,Occ0,Occ,OccN),
	    fromto(N0,N1,N2,N3),
	    param(Options)
	do
	    ( X == X0 ->	% same variable, unify IDs
		N2 = N1,
		VarId = VarId0,
		Occ = multi
	    ; var(VarId0) ->
		N2 is N1+1,
		VarId0 = N2,	% assign next number
		Occ = single
	    ;
		head_singleton_check(Occ0, Source0, Options),
		N2 = N1,	% was already numbered
		Occ = single
	    )
	),
	( var(VarIdN) ->
	    N is N3+1,
	    VarIdN = N		% assign next number
	;
	    head_singleton_check(OccN, SourceN, Options),
	    N = N3		% was already numbered
	).


    % Report only singleton head arguments here (because this is the
    % last time we have the necessary information).
    % Other singletons are reported in the variable classification phase.
    head_singleton_check(single, Source, Options) ?-
	singleton_warning(Source, Options).
    head_singleton_check(multi, _, _).



% From a head, construct a normalised head, which is a pseudo
% goal of kind:head. The normalised head contains distinct variables
% these already get their VarIds (1..Arity) assigned here.
% Head unifications get flattened into a sequence of =/2 goals.
%
% p(a) :- ...	is normalised into p(T) :- T?=a, ...
% p(X,X) :- ...	is normalised into p(X,T) :- X=T, ...
% p(X,X) ?- ...	is normalised into p(X,T) :- X==T, ...
% p(X{A}) ?- ... is normalised into into p(X) :- X?=X{A}, ...
% p(X{A},X{A}) ?- ... is normalised into p(X,T) :- X?=X{A}, T==X, ...
%
% attrvar{} descriptors are only created on the rhs of ?=/2, in all other
% locations, we use simple variable{} descriptors for attributed variables.

normalize_head(HeadType, Head, AnnHead, CallPos, Goals1, Goals, Vs0, Vs) :-
	AnnHead =: annotated_term{term:HeadAnn},
	(
	    foreacharg(Arg,Head,I),
	    fromto(Vs0,Vs1,Vs3,Vs),
	    fromto([],Seen1,Seen2,_),
	    fromto(Goals1,Goals2,Goals3,Goals),
	    param(HeadType,HeadAnn,CallPos)
	do
	    Goal = goal{
		    kind:simple,
		    path:File,
		    line:Line,
		    from:From,
		    to:To,
		    callpos:CallPos,
		    definition_module:sepia_kernel,
		    lookup_module:sepia_kernel,
		    functor:Op/2,
		    args:[HeadArg,NormArg]
		},
	    varg(I, HeadAnn, AnnArg),
	    ann_location(AnnArg, File, Line, From, To),
	    ( nonvar(Arg) ->
		% p(nonvar) :-  becomes  p(T) :- T=nonvar
		% p(nonvar) ?-  becomes  p(T) :- T?=nonvar
		Goals2 = [Goal|Goals3],
		normalize_term(Arg, AnnArg, NormArg, Vs1, Vs3, HeadType),
		Seen2 = Seen1,
		Op = HeadType,
		new_vardesc(I, HeadArg)
	    ; varnonmember(Arg,Seen1) ->
		Seen2 = [Arg|Seen1],
		( meta(Arg), HeadType = (?=) ->
		    % p(X{A}) ?-  becomes  p(X) :- X?=X{A}
		    Goals2 = [Goal|Goals3],
		    Op = HeadType,
		    normalize_var(Arg, AnnArg, NormArg, Vs1, Vs3, ?=, I),
		    NormArg = attrvar{variable:HeadArg}
		;
		    % Don't create a new variable for the first occurrence.
		    % p(X) :- ...  remains  p(X) :- ...
		    normalize_var(Arg, AnnArg, HeadArg, Vs1, Vs3, =, I),
		    Goals2 = Goals3
		)
	    ;
		% repeat occurrence: T=X (or T==X for matching)
		Goals2 = [Goal|Goals3],
		normalize_var(Arg, AnnArg, NormArg, Vs1, Vs3, =, _VarId),
		Seen2 = Seen1,
		headtype_varop(HeadType, Op),
		new_vardesc(I, HeadArg)
	    )
	).

headtype_varop(=, =).
headtype_varop(?=, ==).

varnonmember(_X, []).
varnonmember(X, [Y|Ys]) :-
	X \== Y,
	varnonmember(X, Ys).


% Flatten unifications and normalise them such that there is always
% a variable on the left hand side. The sub-unifications inherit the
% source annotation from the original unification goal.
simplify_unification(X=Y, Ann, UnifGoals, AnnUnifGoals) ?-
	Ann =: annotated_term{term:(AnnX=AnnY)},
	simplify_unification(X, Y, AnnX, AnnY, Ann, UnifGoals, [], AnnUnifGoals, []).


%:- mode simplify_unification(?,?,?,?,?,-,?,-,?).
simplify_unification(X, Y, AnnX, AnnY, Ann, [X=Y|T], T, [AnnEq|AT], AT) :-
	var(X), !,
	ann_update_term(AnnX=AnnY, Ann, AnnEq).
simplify_unification(X, Y, AnnX, AnnY, Ann, Us, Us0, AnnUs, AnnUs0) :-
	var(Y), !,
	simplify_unification(Y, X, AnnY, AnnX, Ann, Us, Us0, AnnUs, AnnUs0).
simplify_unification(X, Y, _AnnX, _AnnY, _Ann, Us, Us, AnnUs, AnnUs) :-
	X == Y, !.
simplify_unification(X, Y, AnnX, AnnY, Ann, L, L0, AnnL, AnnL0) :-
	functor(X, F, N), functor(Y, F, N), !,
	AnnX =: annotated_term{term:XAnn},
	AnnY =: annotated_term{term:YAnn},
	(
	    for(I,1,N),
	    fromto(L,L1,L2,L0),
	    fromto(AnnL,AnnL1,AnnL2,AnnL0),
	    param(X,Y,XAnn,YAnn,Ann)
	do
	    arg(I, X, AX),
	    arg(I, Y, AY),
	    varg(I, XAnn, AnnAX),
	    varg(I, YAnn, AnnAY),
	    simplify_unification(AX, AY, AnnAX, AnnAY, Ann, L1, L2, AnnL1, AnnL2)
	).
simplify_unification(_X, _Y, _AnnX, _AnnY, Ann, [fail|T], T, [AnnF|AT], AT) :-
	ann_update_term(fail, Ann, AnnF).


/*
%----------------------------------------------------------------------
% Reorder a simple basic block prefix
% - bring indexable tests to the front
% - bring Var=Var unifications together
%----------------------------------------------------------------------

reorder_goals([], []).
reorder_goals(Goals, RGoals) :- Goals = [_|_],
	reorder_prefix(Goals, RGoals).
reorder_goals(disjunction{branches:Branches}, disjunction{branches:RBranches}) :-
	(
	    foreach(Branch,Branches),
	    foreach(RBranch,RBranches)
	do
	    reorder_goals(Branch, RBranch)
		
	).


reorder_prefix(Goals, ReorderedGoals) :-
	extract_and_prioritize_simple_prefix(Goals, KeyPrefix, Rest),
	keysort(KeyPrefix, SortedKeyPrefix),
	strip_key(SortedKeyPrefix, SortedPrefix),
	append(SortedPrefix, Rest, ReorderedGoals).

    strip_key([], []).
    strip_key([_K-X|KXs], [X|Xs]) :-
	strip_key(KXs, Xs).
	
extract_and_prioritize_simple_prefix([], [], []).
extract_and_prioritize_simple_prefix([Goal|Goals], Prefix, Rest) :-
	normalize_unif(Goal, Goal1),
	( prefix_goal(Goal1, Prio) ->
	    Prefix = [Prio-Goal1|Prefix0],
	    extract_and_prioritize_simple_prefix(Goals, Prefix0, Rest)
	;
	    Prefix = [],
	    Rest = [Goal1|Goals]
	).

:- mode prefix_goal(+,-).
%prefix_goal(goal{functor:atom/1}, 1) :- !.
%prefix_goal(goal{functor:atomic/1}, 1) :- !.
%prefix_goal(goal{functor:number/1}, 1) :- !.
%prefix_goal(goal{functor:var/1}, 1) :- !.
%prefix_goal(goal{functor:nonvar/1}, 1) :- !.
%prefix_goal(goal{functor:integer/1}, 1) :- !.
%prefix_goal(goal{functor:real/1}, 1) :- !.
%prefix_goal(goal{functor:rational/1}, 1) :- !.
%prefix_goal(goal{functor:breal/1}, 1) :- !.
%prefix_goal(goal{functor:free/1}, 1) :- !.
%prefix_goal(goal{functor:string/1}, 1) :- !.
%prefix_goal(goal{functor:meta/1}, 1) :- !.
%prefix_goal(goal{functor:is_handle/1}, 1) :- !.
%prefix_goal(goal{functor:is_suspension/1}, 1) :- !.
prefix_goal(goal{functor:(=)/2,args:[_,Y]}, 1) :- atomic(Y), !.
prefix_goal(goal{functor:(=)/2,args:[_,[_|_]]}, 2) :- !.
prefix_goal(goal{functor:(=)/2,args:[_,structure{}]}, 2) :- !.
prefix_goal(goal{functor:(=)/2,args:[variable{},variable{}]}, 3) :- !.
prefix_goal(goal{functor:(=)/2,args:[X,Y]}, 4) :- !,
	printf(warning_output, "Unclassified unification %w%n", [X=Y]).


normalize_unif(Goal, NormUnif) :-
	Goal = goal{functor:(=)/2,args:[X,Y]},
	Y = variable{},
	X \= variable{},
	!,
	update_struct(goal, args:[Y,X], Goal, NormUnif).
normalize_unif(Goal, Goal).
*/

%----------------------------------------------------------------------
% 
%----------------------------------------------------------------------

:- export print_normalized_clause/2.

print_normalized_clause(Stream, Clause) :-
	writeln(Stream, "------ Normalized Source ------"),
	print_normalized_goal(Stream, 0, Clause).


print_normalized_goal(_Stream, _Indent, []).
print_normalized_goal(Stream, Indent, [Goal|Goals]) :-
	print_normalized_goal(Stream, Indent, Goal),
	print_normalized_goal(Stream, Indent, Goals).
print_normalized_goal(Stream, Indent, disjunction{determinism:BranchDets,arity:A,
		args:Args,indexvars:IndexVars,callpos:P,branches:Bs,branchheadargs:BHA}) :-
	indent(Stream, Indent),
	( foreacharg(BrDet,BranchDets), fromto(semidet,Det1,Det2,Det) do
	    ( (BrDet==det;BrDet==failure) -> Det2=Det1 ; Det2=nondet )
	),
	printf(Stream, "DISJ/%w  (%w, callpos:", [A,Det]),
	print_call_pos(Stream, P),
	writeln(Stream, ")"),
	ArgIndent is Indent+1,
	( foreach(Arg,Args), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, Arg)
	),
	indent(Stream, Indent),
	writeln(Stream, "INDEXES:"),
	( foreach(Arg,IndexVars), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, Arg)
	),
	arity(BranchDets, NBranches),
	( foreach(Branch,Bs), count(BranchI,1,_), param(Stream,Indent,P,A,BHA,BranchDets,NBranches) do
	    Indent1 is Indent+1,
	    indent(Stream, Indent),
	    arg(BranchI, BranchDets, BranchDet),
	    printf(Stream, "BRANCH/%w (%d of %d, %w, callpos:", [A,BranchI,NBranches,BranchDet]),
	    append(P, [BranchI], PB),
	    print_call_pos(Stream, PB),
	    writeln(Stream, ")"),
	    ArgIndent is Indent1+1,
	    ( integer(A), A>0 ->
		arg(BranchI, BHA, Args),
		( foreach(Arg,Args), param(Stream,ArgIndent) do
		    indent(Stream, ArgIndent), writeln(Stream, Arg)
		)
	    ;
		true
	    ),
	    print_normalized_goal(Stream, Indent1, Branch)
	),
	indent(Stream, Indent),
	write(Stream, "JOIN  (callpos:"),
	print_call_pos(Stream, P),
	writeln(Stream, ")").
print_normalized_goal(Stream, Indent, goal{kind:K,callpos:P,state:State,
		lookup_module:LM, functor:F,args:Args,envmap:EAM,envsize:ESize,
                path:Path,line:Line}) :-
	indent(Stream, Indent),
	( K == head -> write(Stream, "HEAD") ; write(Stream, "GOAL") ),
        printf(Stream, "  %w  (lm:%w, kind:%w, path:%w, line:%w callpos:", [F,LM,K,Path,Line]),
	print_call_pos(Stream, P),
	decode_activity_map(EAM,Env),
	printf(Stream, ", env:%w@%w)%n", [ESize,Env]),
	ArgIndent is Indent+1,
	( foreach(Arg,Args), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, Arg)
	),
	print_goal_state(Stream, Indent, State).


%----------------------------------------------------------------------
% Denormalize (obtain a source suitable for inlining)
% TODO: with annotations
% TODO: Catch simple cases of recursion
% TODO: if local predicates from the compilation module are called,
%	they can only be called via :/2 if they are exported.
% TODO: purge redundant get_cuts?
% Note: context modules are handled correctly, since the tool
%	calls are already expanded at this point!
% Note: Unfortunately, we can only restore the annotations partially
%	(per goal, not per term), but enough for tracing
%----------------------------------------------------------------------

:- export denormalize_pred/3.
denormalize_pred([NormHead|NormBody], VarCount, (Head:-Body)) :-
	dim(Vars, [VarCount]),
	certainly_once denormalize_head(NormHead, Vars, Head),
	denormalize_conj(NormBody, Vars, Body).

%denormalize_head(goal{kind:head,functor:F/A,args:NormArgs}, Vars, Head) :-
%	functor(Head, F, A),
%	( foreach(NormArg,NormArgs), foreacharg(Arg,Head), param(Vars) do
%	    denormalize_term(NormArg, Vars, Arg)
%	).

denormalize_conj([], _Vars, true).
denormalize_conj([NormGoal|NormGoals], Vars, Conj) :-
	denormalize_goal(NormGoal, Vars, Goal1),
	(
	    foreach(NormGoal,NormGoals),
	    fromto(Goal1,PrevGoal,Goal,LastGoal),
	    fromto(Conj,(PrevGoal,Conj2),Conj2,LastGoal),
	    param(Vars)
	do
	    denormalize_goal(NormGoal, Vars, Goal)
	).

denormalize_goal(disjunction{branches:NormBranches}, Vars, Disj) ?-
	certainly_once NormBranches = [NormBranch1|NormBranches1],
	denormalize_conj(NormBranch1, Vars, Alt1),
	(
	    foreach(NormBranch,NormBranches1),
	    fromto(Alt1,PrevAlt,Alt,LastAlt),
	    fromto(Disj,(PrevAlt;Disj2),Disj2,LastAlt),
	    param(Vars)
	do
	    denormalize_conj(NormBranch, Vars, Alt)
	).
denormalize_goal(goal{kind:Kind,functor:F/A,args:NormArgs,lookup_module:LM,definition_module:DM}, Vars, QGoal) ?-
	verify Kind \== head,
	( DM==[] -> QGoal=LM:Goal ; QGoal=DM:Goal ),
	functor(Goal, F, A),
	( foreach(NormArg,NormArgs), foreacharg(Arg,Goal), param(Vars) do
	    denormalize_term(NormArg, Vars, Arg)
	).

%denormalize_term(variable{varid:VarId}, Vars, Var) :-
%	arg(VarId, Vars, Var).
%denormalize_term(structure{name:F,arity:A,args:NormArgs}, Vars, Term) :-
%	functor(Term, F, A),
%	( foreach(NormArg,NormArgs), foreacharg(Arg,Term), param(Vars) do
%	    denormalize_term(NormArg, Vars, Arg)
%	).
%denormalize_term([NormArg|NormArgs], Vars, [Arg|Args]) :-
%	denormalize_term(NormArg, Vars, Arg),
%	denormalize_term(NormArgs, Vars, Args).
%denormalize_term(NormTerm, _Vars, Term) :- atomic(NormTerm),
%	Term=NormTerm.


%----------------------------------------------------------------------
% This will leave goal arguments unannotated!
% Problem: goal without source location are intermixed!


:- export denormalize_pred/5.
denormalize_pred([NormHead|NormBody], VarCount, Head, Body, AnnBody) :-
	dim(Vars, [VarCount]),
	certainly_once denormalize_head(NormHead, Vars, Head),
	denormalize_conj(NormBody, Vars, Body, AnnBody).

denormalize_head(goal{kind:head,functor:F/A,args:NormArgs}, Vars, Head) :-
	functor(Head, F, A),
	( foreach(NormArg,NormArgs), foreacharg(Arg,Head), param(Vars) do
	    denormalize_term(NormArg, Vars, Arg)
	).

denormalize_conj([], _Vars, true, _AnnMissing).
denormalize_conj([NormGoal|NormGoals], Vars, Conj, AnnConj) :-
	denormalize_goal(NormGoal, Vars, Goal1, AnnGoal1),
	(
	    foreach(NormGoal,NormGoals),
	    fromto(Goal1,PrevGoal,Goal,LastGoal),
	    fromto(AnnGoal1,AnnPrevGoal,AnnGoal,AnnLastGoal),
	    fromto(Conj,(PrevGoal,Conj2),Conj2,LastGoal),
	    fromto(AnnConj,AnnConj1,AnnConj2,AnnLastGoal),
	    param(Vars)
	do
	    inherit_annotation((AnnPrevGoal,AnnConj2), AnnPrevGoal, AnnConj1),
	    denormalize_goal(NormGoal, Vars, Goal, AnnGoal)
	).

denormalize_goal(disjunction{branches:NormBranches}, Vars, Disj, AnnDisj) ?-
	certainly_once NormBranches = [NormBranch1|NormBranches1],
	denormalize_conj(NormBranch1, Vars, Alt1, AnnAlt1),
	(
	    foreach(NormBranch,NormBranches1),
	    fromto(Alt1,PrevAlt,Alt,LastAlt),
	    fromto(AnnAlt1,AnnPrevAlt,AnnAlt,AnnLastAlt),
	    fromto(Disj,(PrevAlt;Disj2),Disj2,LastAlt),
	    fromto(AnnDisj,AnnDisj1,AnnDisj2,AnnLastAlt),
	    param(Vars)
	do
	    inherit_annotation((AnnPrevAlt;AnnDisj2), AnnPrevAlt, AnnDisj1),
	    denormalize_conj(NormBranch, Vars, Alt, AnnAlt)
	).
denormalize_goal(NormGoal, Vars, QGoal, AnnQGoal) ?-
	NormGoal = goal{kind:Kind,functor:F/A,args:NormArgs,lookup_module:LM,definition_module:DM},
	verify Kind \== head,
	( DM==[] -> LDM=LM ; LDM=DM ),
	QGoal=LDM:Goal,
	functor(Goal, F, A),
	annotate_from_norm_goal(Goal, NormGoal, AnnGoal),
	inherit_annotation(LDM, AnnGoal, AnnLDM),
	inherit_annotation(AnnLDM:AnnGoal, AnnGoal, AnnQGoal),
	( foreach(NormArg,NormArgs), foreacharg(Arg,Goal), param(Vars) do
	    denormalize_term(NormArg, Vars, Arg)
	).

denormalize_term(variable{varid:VarId}, Vars, Var) :-
	arg(VarId, Vars, Var).
denormalize_term(structure{name:F,arity:A,args:NormArgs}, Vars, Term) :-
	functor(Term, F, A),
	( foreach(NormArg,NormArgs), foreacharg(Arg,Term), param(Vars) do
	    denormalize_term(NormArg, Vars, Arg)
	).
denormalize_term([NormArg|NormArgs], Vars, [Arg|Args]) :-
	denormalize_term(NormArg, Vars, Arg),
	denormalize_term(NormArgs, Vars, Args).
denormalize_term(NormTerm, _Vars, Term) :- atomic(NormTerm),
	Term=NormTerm.


annotate_from_norm_goal(Term, goal{path:P,line:L,from:F,to:T}, Ann) :-
	( var(P) ->
	    Ann=annotated_term{term:TermAnn,type:Type,file:'',line:0,from:0,to:0}
	;
	    Ann=annotated_term{term:TermAnn,type:Type,file:P,line:L,from:F,to:T}
	),
	functor(Term, N, A),
	functor(TermAnn, N, A),	% annotated arguments remain uninstantiated!
	type_of(Term, Type).

inherit_annotation(_Term, AnnTmpl, _AnnTerm) :- var(AnnTmpl), !.
inherit_annotation(Term, AnnTmpl,  AnnTerm) :-
	update_struct(annotated_term, [term:Term,type:Type], AnnTmpl, AnnTerm),
	type_of(Term, Type).