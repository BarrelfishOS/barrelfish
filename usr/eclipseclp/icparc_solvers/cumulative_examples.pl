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
% Contributor(s): Vassilis Liatsos, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: cumulative_examples.pl,v 1.1 2006/09/23 01:53:25 snovello Exp $

% Author:      Vassilis Liatsos 
% Description
% Examples using the cumulative constraint
% ----------------------------------------------------------------------


%:- use_module(cumulative).
%:- use_module(edge_finder).
%:- use_module(edge_finder3).
%:- use_module('../lib_icparc/edge_finder').
% :- use_module('../lib_icparc/edge_finder3').
% :- use_module(cumulative).

:- lib(fd_global).
:- lib(edge_finder).
:- lib(branch_and_bound).

/*

This small resource scheduling problem is taken from
"Introducing Global Constraints in CHIP" by N.Beldiceanu and E.Contejean
Mathl. Comput. Modellinmg Vol. 20, No. 12, pp 97-123, 1994

Data for scheduling problem

--------------------------------------------
task     | t1 | t2 | t3 | t4 | t5 | t6 | t7 |
---------+----+----+----+----+----+----+----|
duration | 16 |  6 | 13 |  7 |  5 | 18 |  4 |
---------+----+----+----+----+----+----+----|
resource |  2 |  9 |  3 |  7 | 10 |  1 | 11 |
--------------------------------------------

*/

% Optimisation strategy: step or dichotomic
:- setval(strategy,step).

go(LO):-
	cputime(Start),
	LO = [O1,O2,O3,O4,O5,O6,O7],
	LD = [16,6,13,7,5,18,4],
	LR = [2,9,3,7,10,1,11],
	LE = [E1,E2,E3,E4,E5,E6,E7],
	LO :: 1..30,
	LE :: 1..30,
	O1 + 16 #= E1,
	O2 + 6 #= E2,
	O3 + 13 #= E3,
	O4 + 7 #= E4,
	O5 + 5 #= E5,
	O6 + 18 #= E6,
	O7 + 4 #= E7,
	cumulative(LO,LD,LR,13),
	maxlist(LE,Cost),
	getval(strategy,Strategy),
	Options = bb_options with strategy:Strategy,
	bb_min(labeling(LO),Cost,Options),
	cputime(End),
	Time is End - Start,
	printf("Proof of optimality in %.2f sec(s)\n",[Time]).

/*

Classical Rectangle Packing Problems
Taken from: "Extending CHIP in Order to Solve Complex 
Scheduling and Placement Problems"

*/

% Pfefferkorn
pfefferkorn(Solutions):-
	cputime(Start),
	LO = [O1,O2,O3,O4,O5,O6],
	LD = [6,4,2,2,2,2],
	LR = [2,2,3,3,3,1],
	LE = [E1,E2,E3,E4,E5,E6],
	LO :: 1..9,
	LE :: 1..9,
	O1 + 6 #= E1,
	O2 + 4 #= E2,
	O3 + 2 #= E3,
	O4 + 2 #= E4,
	O5 + 2 #= E5,
	O6 + 2 #= E6,
	% Symmetry constraints
%	O3 #<= O4, O4 #<= O5,
	cumulative(LO,LD,LR,5),

%	LO = [1, 5, 1, 3, 7, 5],
%	O1 = 1, later, O2 = 5,later, O3 = 1,later, O4 = 3,later, O5 = 7, later,O6 = 5,later,

	findall(LO,label_deleteff(LO),Solutions),
	once(member(LO,Solutions)),
	length(Solutions,No),
	printf('Number of solutions: %w\n',[No]),
        cputime(End),
	Time is End - Start,
	printf("Solutions found in %.2f sec(s)\n",[Time]).

later:- true.
/*
cucumulative_ef2mulative_ef2([1, 5, 1, O4{[3..7]}, O5{[3..7]}, O6{[2..7]}], [6, 4, 2, 2, 2, 2], [2, 2, 3, 3, 3, 1], [12, 8, 6, 6, 6, 2], 'HANDLE'(16'081087c0)) (dbg)?- skip
  (1078) 1  FAIL   cumulative_ef2([1, 5, 1, O4{[3..7]}, O5{[3..7]}, O6{[2..7]}], [6, 4, 2, 2, 2, 2], [2, 2, 3, 3, 3, 1], [12, 8, 6, 6, 6, 2], 'HANDLE'(16'081087c0)) (dbg)?- 
*/
test :-
 O4::3..7,
 O5::3..7,
 O6::2..7,
 cumulative([1,5,1,O4,O5,O6],[6, 4, 2, 2, 2, 2], [2, 2, 3, 3, 3, 1],5).
%	LO = [1, 5, 1, 3, 7, 5],
% Lauriere
lauriere(Solutions):-
	cputime(Start),
	LO = [O1,O2,O3,O4,O5,O6],
	[D1,R1] :: [6,2],
	[D2,R2] :: [4,2],
	[D3,R3,D4,R4,D5,R5] :: [2,3],
	[D6,R6] :: [2,1],
	LD = [D1,D2,D3,D4,D5,D6],
	LR = [R1,R2,R3,R4,R5,R6],
	LE = [E1,E2,E3,E4,E5,E6],
	LO :: 1..9,
	LE :: 1..9,
	O1 + D1 #= E1,
	O2 + D2 #= E2,
	O3 + D3 #= E3,
	O4 + D4 #= E4,
	O5 + D5 #= E5,
	O6 + D6 #= E6,
	D1 ## R1, D2 ## R2, D3 ## R3, D4 ## R4, D5 ## R5, D6 ## R6,
	cumulative(LO,LD,LR,5),
	Vars = [O1,D1,O2,D2,O3,D3,O4,D4,O5,D5,O6,D6],
	findall(Vars,labeling(Vars),Solutions),
	once(member(Vars,Solutions)),
	length(Solutions,No),
	printf('Number of solutions: %w\n',[No]),
        cputime(End),
	Time is End - Start,
	printf("Solutions found in %.2f sec(s)\n",[Time]).



% Tong
tong(Solutions):-
	cputime(Start),
	LO = [O1,O2,O3,O4],
	LD = [D1,D2,D3,D4],
	LR = [R1,R2,R3,R4],
	LE = [E1,E2,E3,E4],
	LD::4..9,
	LR::4..9,
	LO :: 1..10,
	LE :: 1..10,
	O1 + D1 #= E1,
	O2 + D2 #= E2,
	O3 + D3 #= E3,
	O4 + D4 #= E4,
	(foreach(D,LD),foreach(R,LR),fromto(0,In,Out,Sum) do
            Out =  D*R +In
 	),
	TotalArea is 9*9,
	TotalArea #= Sum,
	% O1 #<= O2, O2 #<= O3, O3 #<= O4,
	cumulative(LO,LD,LR,9),
	Vars = [O1,D1,R1,O2,D2,R2,O3,D3,R3,O4,D4,R4],
	findall(Vars,labeling(Vars),Solutions),
	once(member(Vars,Solutions)),
	length(Solutions,No),
	printf('Number of solutions: %w\n',[No]),
        cputime(End),
	Time is End - Start,
	printf("Solutions found in %.2f sec(s)\n",[Time]).

	

% Reingold
reingold(Solutions):-
	cputime(Start),
	LO = [O1,O2,O3,O4,O5,O6,O7],
	LD = [D1,D2,D3,D4,D5,D6,D7],
	LR = [R1,R2,R3,R4,R5,R6,R7],
	LE = [E1,E2,E3,E4,E5,E6,E7],
	LD::1..22,
	LR::1..13,
	LO :: 1..23,
	LE :: 1..23,
	O1 + D1 #= E1,
	O2 + D2 #= E2,
	O3 + D3 #= E3,
	O4 + D4 #= E4,
	O5 + D5 #= E5,
	O6 + D6 #= E6,
	O7 + D7 #= E7,
	(foreach(D,LD),foreach(R,LR),fromto(0,In,Out,Sum) do
            Out =  D*R +In
 	),
	TotalArea is 22*13,
	TotalArea #= Sum,
%	TotalArea #>= Sum, TotalArea #<=Sum,

	D1 #< D2, D2 #< D3, D3 #< D4, D4 #< D5, D5 #< D6, D6 #< D7, 
	R1 #> R2, R2 #> R3, R3 #> R4, R4 #> R5, R5 #> R6, R6 #> R7,
	
%       A solution for this problem - used to test the constraints
%       [D1,D2,D3,D4,D5,D6,D7] = [4,5,6,7,13,16,18],
%       [R1,R2,R3,R4,R5,R6,R7] = [11,10,9,7,3,2,1],

	cumulative(LO,LD,LR,13),
%	cumulative_ef(LO,LD,LR,13),

%	cannot_be_included(LD,LR),
	cannot_be_included(LD,LR,[],Bools),
	Vars = [O1,D1,R1,O2,D2,R2,O3,D3,R3,O4,D4,R4,O5,D5,R5,O6,D6,R6,O7,D7,R7],
%	Vars = [D1,R1,D2,R2,D3,R3,D4,R4,D5,R5,D6,R6,D7,R7,O1,O2,O3,O4,O5,O6,O7],
	findall(Vars,(label_deleteff(Bools),label_deleteff(Vars),writeln(sol(LD,LR))),Solutions),
	once(member(Vars,Solutions)),
	length(Solutions,No),
	printf('Number of solutions: %w\n',[No]),
        cputime(End),
	Time is End - Start,
	printf("Solutions found in %.2f sec(s)\n",[Time]).

	
label_deleteff([V|Vs]):-
	deleteff(Var,[V|Vs],Rest),
	indomain(Var),
	label_deleteff(Rest).
label_deleteff([]).

cannot_be_included([D|Ds],[R|Rs],Sofar,Bools):-
	cannot_be_included_aux(D,Ds,R,Rs,Sofar,NewSofar),
	cannot_be_included(Ds,Rs,NewSofar,Bools).
cannot_be_included([],[],Bools,Bools).


cannot_be_included_aux(D1,[D2|Ds],R1,[R2|Rs],Sofar,Bools):-
	B::0..1,
	rectangle_not_included(D1,R1,D2,R2,B),
	New = [B|Sofar],
	cannot_be_included_aux(D1,Ds,R1,Rs,New,Bools).
cannot_be_included_aux(_,[],_,[],Bools,Bools).


rectangle_not_included(D1,R1,D2,R2,B):-
	(nonvar(B)->
	    (B==0 -> 
		D1 #> R2, D2 #> R1
	    ; 
	        R1 #> D2, D1 #< R2
	    )
	;
	    dvar_range(D1,D1min,D1max),
	    dvar_range(D2,D2min,D2max),
	    dvar_range(R1,R1min,R1max),
	    dvar_range(R2,R2min,R2max),
	    (((D1min > R2max);(D2min > R1max)) ->
		B ## 1, D1 #> R2, D2 #> R1
	    ;
	        (((R1min > D2max);(R2min > D1max)) ->
		    B ## 0, R1 #> D2, R2 #> D1
		;   
		    Var = v(D1,R1,D2,R2),
		    suspend(rectangle_not_included(D1,R1,D2,R2,B),4,[Var->min,Var->max,B->inst])
		)
	    )
	).

/*


cannot_be_included([D|Ds],[R|Rs]):-
	cannot_be_included_aux(D,Ds,R,Rs),
	cannot_be_included(Ds,Rs).
cannot_be_included([],[]).

cannot_be_included_aux(D1,[D2|Ds],R1,[R2|Rs]):-
	rectangle_not_included(D1,R1,D2,R2),
	cannot_be_included_aux(D1,Ds,R1,Rs).
cannot_be_included_aux(_,[],_,[]).

rectangle_not_included(D1,R1,D2,R2):-
	% Assume D1 > D2 and R1 < R2
	dvar_range(D1,D1min,D1max),
	dvar_range(D2,D2min,D2max),
	dvar_range(R1,R1min,R1max),
	dvar_range(R2,R2min,R2max),
	( R2max < D1min -> R1 #< D2
        ; R1max < D2min -> R2 #<D2
        ; D2max < R1min -> D1 #< R2
        ; D1max < R2min -> D2 #< R1
        ; suspend(rectangle_not_included(D1,R1,D2,R2),4,[D1,D2,R1,R2]->constrained)
        ).

rectangle_not_included(D1,R1,D2,R2):-
	% Assume D1 > D2 and R1 < R2
	((D1 #> R2, R1 #< D2) ; (R1 #> D2, D1 #< R2)).


rectangle_not_included(D1,R1,D2,R2):-

	setval(prop,false),
	dvar_range(D1,D1min,D1max),
	dvar_range(D2,D2min,D2max),
	dvar_range(R1,R1min,R1max),
	dvar_range(R2,R2min,R2max),
	(D1max < D2min -> R1 #> R2, setval(prop,true) ; true),
	(R1max < R2min -> D1 #> D2, setval(prop,true) ; true),
	(D2max < D1min -> R2 #> R1, setval(prop,true) ; true),
	(R2max < R1min -> D2 #> D1, setval(prop,true) ; true),
	(getval(prop,true) ->
	    true
	;
	    suspend(rectangle_not_included(D1,R1,D2,R2),4,[D1,D2,R1,R2]->constrained)
	).

rectangle_not_included(D1,W1,D2,W2):-
	( (D1 #> D2 , W2 #> W1)    % case (i)
        ; (D2 #> D1 , W1 #> W2)    % case (ii)
        ; (D1 #> W2 , D2 #> W1)    % case (iii)
        ; (W2 #> D1 , W1 #> D2) ).  % case (iv)


*/
% Vassilis
test(LO):-
    LO = [O1,O2,O3,O4],
    LD = [2,2,2,2],
    LR = [2,2,2,2],
    LE = [E1,E2,E3,E4],
    LO :: 0..4,
    LE :: 0..4,
    O1 + 2 #= E1,
    O2 + 2 #= E2,
    O3 + 2 #= E3,
    O4 + 2 #= E4,
    % symmetry constraints 
    cumulative(LO,LD,LR,4),
    labeling(LO),writeln(LO).



