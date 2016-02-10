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
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: repairfd.pl,v 1.1 2006/09/23 01:53:53 snovello Exp $
%
%
% IDENTIFICATION:       repairfd.pl
%
% Contents:	CLP Repair example labeling
%
% Authors:	Hani El-Sakkout, Stefano Novello, Joachim Schimpf
%		IC-Parc
% ----------------------------------------------------------------------

% Example repair labeling strategy


% ----------------------------------------------------------------------
:- module(repairfd).
% ----------------------------------------------------------------------

:- lib(fd).
:- lib(repair).


:- export repair/1.

repair(ConflictSet) :-
	( conflict_vars([C|_onflict]) ->
		indomain(C),
		repair(ConflictSet)
	; conflict_constraints(ConflictSet, [C|_onflictConstraints]) ->
		term_variables(C, Vars),
		deleteffc(Var,Vars, _),
		Var tent_get Val,
		(Var = Val ; Var #\= Val), % choice
		repair(ConflictSet)
	;
		true
	).


:- export repair/0.	% backward compatibility

repair :-
	( conflict_vars([C|_onflict]) ->
		indomain(C),
		repair
	; conflict_constraints([C|_onflictConstraints]) ->
		term_variables(C, Vars),
		deleteffc(Var,Vars, _),
		Var tent_get Val,
		(Var = Val ; Var #\= Val), % choice
		repair
	;
		true
	).

