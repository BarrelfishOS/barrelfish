/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 2000-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, IC-Parc
 * 
 * END LICENSE BLOCK */
% ----------------------------------------------------------------------
% 
% Solver for constraints over finite sets of integers (fd wrapper)
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: fd_sets.ecl,v 1.2 2009/07/16 09:11:27 jschimpf Exp $
%
% ----------------------------------------------------------------------

:- module(fd_sets).

:- lib(fd).

tr_generic_sets(impose_min(Var,Min), dvar_remove_smaller(Var,Min)).
tr_generic_sets(impose_max(Var,Max), dvar_remove_greater(Var,Max)).
tr_generic_sets(solver_module, fd).
tr_generic_sets(sbds_module, fd_sbds).

:- inline(impose_min/2, tr_generic_sets/2).
:- inline(impose_max/2, tr_generic_sets/2).
:- local macro(solver_module, tr_generic_sets/2, []).
:- local macro(sbds_module, tr_generic_sets/2, []).

impose_min(Var,Min) :- impose_min(Var,Min).
impose_max(Var,Max) :- impose_max(Var,Max).

:- include(generic_sets).

% ----------------------------------------------------------------------

:- comment(categories, ["Constraints"]).
:- comment(summary, "Solver over sets of integers (cooperates with lib(fd))").
:- comment(eg, "
% Example program: Steiner triplets
% Compute NB triplets of numbers from 1 to N such that
% any two triplets have at most one element in common.
% Try steiner(9,Sets).

:- lib(fd_sets).
:- lib(fd).

steiner(N, Sets) :-
	NB is N * (N-1) // 6,		% compute number of triplets
	intsets(Sets, NB, 1, N),	% initialise the set variables
	( foreach(S,Sets) do
	    #(S,3)			% constrain their cardinality
	),
	( fromto(Sets,[S1|Ss],Ss,[]) do
	    ( foreach(S2,Ss), param(S1) do
		#(S1 /\\ S2, C),		% constrain the cardinality
		C #=< 1			% of pairwise intersections
	    )
	),
        label_sets(Sets).		% search

label_sets([]).
label_sets([S|Ss]) :-
        insetdomain(S,_,_,_),
	label_sets(Ss).
").
