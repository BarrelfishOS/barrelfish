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
% Contributor(s): 
% 
% END LICENSE BLOCK
% The Deussen Problem -------------------------------------------------------

/*From mark@ecrc.de Tue Jul 14 11:05:16 1992

I thought a propositional satisfiability example would be good.
I therefore propose the Deussen problem Ulm027r1
(chosen pretty well at random).

Mark Wallace
*/

:- [bool]. % load in boolean ECH handler

% the ulm027r1 problem has 16 solutions

% no labeling
deussen0(Vars) :-		
	ulm027r1(L,Vars),
	solve_bools(L).

% built-in labeling
deussen1(Vars) :-		
	ulm027r1(L,Vars),
	solve_bools(L),
	labeling.

% user-defined labeling
deussen2(Vars) :-		
	ulm027r1(L,Vars),
	solve_bools(L),
	label_bool(Vars).

  solve_bools([]).
  solve_bools([X|L]) :-
	solve_bool(X,1),	% boolean expression X must be 1 (true)
	solve_bools(L).

% Deussen Problem Ulm027/1

ulm027r1(
[
U12 + U3 + U2,
U12 + ~~U3 + ~~U2,
~~U12 + ~~U3 + U2,
~~U12 + U3 + ~~U2,
U13 + U4 + U12,
U13 + ~~U4 + ~~U12,
~~U13 + ~~U4 + U12,
~~U13 + U4 + ~~U12,
U14 + U5 + U13,
U14 + ~~U5 + ~~U13,
~~U14 + ~~U5 + U13,
~~U14 + U5 + ~~U13,
~~U14,
U15 + U6 + U4,
U15 + ~~U6 + ~~U4,
~~U15 + ~~U6 + U4,
~~U15 + U6 + ~~U4,
U16 + U2 + U15,
U16 + ~~U2 + ~~U15,
~~U16 + ~~U2 + U15,
~~U16 + U2 + ~~U15,
U17 + U2 + U16,
U17 + ~~U2 + ~~U16,
~~U17 + ~~U2 + U16,
~~U17 + U2 + ~~U16,
U18 + U6 + U17,
U18 + ~~U6 + ~~U17,
~~U18 + ~~U6 + U17,
~~U18 + U6 + ~~U17,
~~U18,
U19 + U10 + U3,
U19 + ~~U10 + ~~U3,
~~U19 + ~~U10 + U3,
~~U19 + U10 + ~~U3,
U20 + U11 + U19,
U20 + ~~U11 + ~~U19,
~~U20 + ~~U11 + U19,
~~U20 + U11 + ~~U19,
U21 + U6 + U20,
U21 + ~~U6 + ~~U20,
~~U21 + ~~U6 + U20,
~~U21 + U6 + ~~U20,
U22 + U7 + U21,
U22 + ~~U7 + ~~U21,
~~U22 + ~~U7 + U21,
~~U22 + U7 + ~~U21,
~~U22,
U23 + U5 + U7,
U23 + ~~U5 + ~~U7,
~~U23 + ~~U5 + U7,
~~U23 + U5 + ~~U7,
U24 + U6 + U23,
U24 + ~~U6 + ~~U23,
~~U24 + ~~U6 + U23,
~~U24 + U6 + ~~U23,
U25 + U10 + U24,
U25 + ~~U10 + ~~U24,
~~U25 + ~~U10 + U24,
~~U25 + U10 + ~~U24,
U26 + U11 + U25,
U26 + ~~U11 + ~~U25,
~~U26 + ~~U11 + U25,
~~U26 + U11 + ~~U25,
~~U26
],
[
%U1,
U2,U3,U4,U5,U6,U7,  %U8,U9,
U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,
U20,U21,U22,U23,U24,U25,U26
]).





