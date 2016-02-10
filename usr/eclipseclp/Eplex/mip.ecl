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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: mip.ecl,v 1.1 2012/07/31 02:17:06 jschimpf Exp $
%
% MIP branch and bound in ECLiPSe using lib(eplex)
%
% J.Schimpf, IC-Parc, 1/96
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
:- module(mip).
% ----------------------------------------------------------------------

:- comment(categories, ["Constraints"]).
:- comment(summary, "An example implementing MIP-style branch-and-bound").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(date, "$Date: 2012/07/31 02:17:06 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- use_module(library(eplex)).
:- lib(branch_and_bound).

:- export bb_inf/3.
:- export bb_inf/4.


% ----------------------------------------------------------------------
% MIP branch-and-bound
% ----------------------------------------------------------------------

bb_inf(IntVars, Expr, MinIpCost) :-
	bb_inf(IntVars, Expr, MinIpCost, eplex).

bb_inf(IntVars, Expr, MinIpCost, Pool) :-

        Pool: (Expr $= IpCost),
	% setup a simplex demon which wakes on bound changes
	Pool:eplex_solver_setup(min(Expr), IpCost, [], 9, [deviating_bounds
%		,pre(writeln(woken)),post(writeln(lpsol:IntVars))
	]),
	Pool:eplex_get(handle, ProbHandle),

	% print some statistics
	lp_get(ProbHandle, vars, VArr),
	functor(VArr, _, NVars),
	length(IntVars, NIntVars),
	writeln(log_output, variables:NVars/intvars:NIntVars),

	% call force_integers/1 within a branch-and-bound framework
	int_tolerance(Delta),
	bb_min((
		force_integers(IntVars, ProbHandle, 0),
		lp_get(ProbHandle, typed_solution, SolutionArr),
		lp_get(ProbHandle, cost, IpCost)
	    ),
	    IpCost, SolutionArr, OptSolutionArr, MinIpCost,
	    bb_options with [strategy:continue,delta:Delta]
	),

	% print some statistics
	lp_get(ProbHandle, statistics, [SS,SF|_]),
	writeln(log_output, (solver_succs=SS, solver_fails=SF)),
	lp_cleanup(ProbHandle),

	% instantiate solutions
	( VArr = OptSolutionArr, true -> true
	; writeln(error, "Instantiating solution lead to failure") ).


% Keep branching as long as there are integer-variables whose lp-solutions
% are not integral. The lp-demon will keep waking up during the process.

force_integers(IntVars, ProbHandle, D0) :-
%	( D0 > 20 -> abort ; true ),
	( integer_violation(IntVars, BranchingVar, SplitValue, ProbHandle) ->
%	    writeln(D0:pick(BranchingVar,IntVars)),
	    branch(BranchingVar, SplitValue, D0),
	    D1 is D0+1,
	    force_integers(IntVars, ProbHandle, D1)
	;
	    true    
	).


    branch(BranchingVar, SplitValue, Depth) :-
	S is round(SplitValue),
	( S > SplitValue ->
	    (
		% try the upper sub-range first
%		writeln(Depth:left->lwb(S,BranchingVar)),
		set_var_bounds(BranchingVar, S, 1.0Inf)
	    ;
		S1 is S-1.0,
%		writeln(Depth:right->upb(S1,BranchingVar)),
		set_var_bounds(BranchingVar, -1.0Inf, S1)
	    )
	;
	    (
		% try the lower sub-range first
%		writeln(Depth:left->upb(S,BranchingVar)),
		set_var_bounds(BranchingVar, -1.0Inf, S)
	    ;
		S1 is S+1.0,
%		writeln(Depth:right->lwb(S1,BranchingVar)),
		set_var_bounds(BranchingVar, S1, 1.0Inf)
	    )
	).


% ----------------------------------------------------------------------
% Variable selection
% ----------------------------------------------------------------------

integer_violation([X|Xs], FractVar, FractSol, ProbHandle) :-
	lp_var_solution(ProbHandle, X, Val),
	( abs(Val - round(Val)) >= int_tolerance ->
	    FractVar = X, FractSol = Val
	;
	    integer_violation(Xs, FractVar, FractSol, ProbHandle)
	).


/*
integer_violation(Xs, FractVar, FractSol) :-
	integer_violation(Xs, _, 1.0, FractVar),
%	integer_violation(Xs, _, 0.0, FractVar),
	lp_var_solution(FractVar, FractSol),
%	call(get_var_index(FractVar, Idx))@eplex,
%	Idx1 is Idx+1,
%	writeln(x(Idx1):FractSol),
	true.

integer_violation([], BestX, BestDiff, BestX) :-
	BestDiff >= int_tolerance,	% Did we actually find one?
	BestDiff < 1.0.
integer_violation([X|Xs], BestX, BestDiff, Res) :-
	lp_var_solution(X, Val),
	Diff is abs(Val - round(Val)),
%	( var(X), Val \== 0.0 ->
%	    call(get_var_index(X, Idx))@eplex,
%	    Idx1 is Idx+1,
%	    write('    '),
%	    writeln(x(Idx1)=Val)
%	;
%	    true
%	),
	(
	    Diff >= int_tolerance,
%	    better(BestX, BestDiff, X, Diff)
%	    Diff < BestDiff
	    Diff =< BestDiff	% use 1.0 initially
%	    Diff > BestDiff	% use 0.0 initially
	->
	    integer_violation(Xs, X, Diff, Res)
	;
	    integer_violation(Xs, BestX, BestDiff, Res)
	).
*/

% prefer general integers to binaries
better(OldX, _OldDiff, _NewX, _NewDiff) :-
	free(OldX), !.
better(OldX, OldDiff, NewX, NewDiff) :-
	( get_var_bounds(OldX, _, 1.0) ->
	    ( get_var_bounds(NewX, _, 1.0) ->
		NewDiff =< OldDiff
	    ;
		true
	    )
	;
	    ( get_var_bounds(NewX, _, 1.0) ->
		fail
	    ;
		NewDiff =< OldDiff
	    )
	).


% ----------------------------------------------------------------------
% end_module(mip).
% ----------------------------------------------------------------------

