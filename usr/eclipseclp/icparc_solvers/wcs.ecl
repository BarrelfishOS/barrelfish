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
% Copyright (C) 1999 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: wcs.ecl,v 1.1 2006/09/23 01:54:01 snovello Exp $
%
% Sample code for weak commitment search
%
% Author:	Kish Shen, IC-Parc
%----------------------------------------------------------------------

:- lib(repair).
:- lib(fd).

:- local variable(varbindings), variable(nlabels).

:- set_event_handler(280, true/0). % make minimize/2 operate silently

wcs(Vars, Initial) :-
	setval(nlabels, 0),
	(foreach(Init, Initial), foreach(Var, Vars) do
            Var tent_set Init
	),
	do_search(Vars, 1).

do_search(Vars, _) :-
	try_one_step(Vars, Vars),
	% remember solution as a nogood so it would not be tried again
	remember_nogood(Vars).
do_search(Vars, N) :- 
% hit dead-end and failed, try again from start after recording nogoods
	add_nogood(Vars),  % put in most recent nogood
	getval(nlabels, NL),
	printf("Restart %w - labelled %w\n",[N,NL]),flush(output),
        N1 is N + 1,
	do_search(Vars, N1).


try_one_step([], _Vars) ?- !.
try_one_step(Left, Vars) :-
	conflict_constraints(Cons),
	label_next(Cons, Left, Vars).


label_next([], Left, _Vars) ?- !,
/* no conflicting repair constraints left. Solution is found, set remaining 
   vars to their tentative value
*/
        (foreach(V, Left) do V tent_get V).
label_next(Cons, Left0, Vars) :-
	pick_var(Cons, Left0, Var, Left1), 
	incval(nlabels),
	(label(Var) ->
	    !, try_one_step(Left1, Vars)
	;   remember_nogood(Vars),
	    fail
	).


label(Var) :-
	   indomain(Var).

%	minimize((
%             indomain(Var),
%	     conflict_constraints(Constraints),
%	     length(Constraints, L) ), L).




% if no suitable var can be found in repair constraints,
% then no solution exists and predicate fails.
pick_var(Cons, Left0, Var, Left) :-
	term_variables(Cons, Vars0),
	deleteffc(Var0, Vars0, Vars1),
	(is_validvar(Var0, Left0, Left) ->
	    Var = Var0 ; pick_var(Vars1, Left0, Var, Left)
	).


% checks that Var is a valid variable to label. Fails if it isn't.
is_validvar(Var, Left0, Left) :-
	Var tent_get V,
	nonvar(V), % Var is a tentative var..
	deletevar(Var, Left0, Left). % ..and Var is in Left0

% similar to delete/3, but treats var. as elements
deletevar(E, [E0|L0], L1) :-
	E == E0,
	L1 = L0.
deletevar(E, [E0|L0], [E0|L]) :-
	deletevar(E, L0, L).


% Implementation of nogoods

add_nogood(NewConfig) :-
	getval(varbindings, Partial),
	(foreach(P, Partial), foreach(V,NewConfig), 
	 fromto(NoGoods,NG0, NG1, []), fromto(NGVars,NGV0,NGV1,[])
         do 
            (nonvar(P) ->
		V tent_set P,
		NG0 = [P|NG1],
	        NGV0 = [V|NGV1]
	    ;   NG0 = NG1,   % keep old tentative value
	        NGV0 = NGV1
	    )
	),
%	NoGoods ~= NGVars r_prop.
	nogood(NGVars, NoGoods) r_prop.


remember_nogood(Vars) :-
	(foreach(Item, Vars), foreach(New, NVars) do
           (var(Item) -> true ; New = Item)
        ),
	setval(varbindings,NVars).


% forward checking nogood constraint
:- mode nogood(+, ++).
nogood([X|Xs], [V|Vs]) :-
	( X==V ->	nogood(Xs, Vs)
	; var(X) ->	nogood(Xs, Vs, X, V)
	; 		true
	).

    nogood([], [], X1, V1) :- X1 ## V1.
    nogood([X|Xs], [V|Vs], X1, V1) :-
	( X==V ->	nogood(Xs, Vs, X1, V1)
	; var(X) ->	suspend(nogood([X1,X|Xs], [V1,V|Vs]), 3, X-X1->inst)
	; 		true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% N-Queens
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


test_wcs_queens(N,L) :-
	length(L, N),
	L:: 1..N,
	create_initial(N, Init),
%	make_display_matrix(L,m),
	setup(L),
	wcs(L, Init),
	writeln(L).

create_initial(N, Init) :-
	(for(I,1,N), foreach(Pos, Init), param(N) do
            Pos is (I * 2) mod N + 1
	).

setup(L) :-
	repair_alldistinct(L),
	repair_constrain_queens(L).

repair_alldistinct([]).
repair_alldistinct([H|T]) :-
    repair_outof(H, T),
    repair_alldistinct(T).

repair_outof(_, []).
repair_outof(X, [H|T]) :-
    X ## H r_prop,
    repair_outof(X, T).

repair_constrain_queens([]).
repair_constrain_queens([X|Y]) :-
   repair_safe(X, Y, 1),
   repair_constrain_queens(Y).

repair_safe(_, [], _).
repair_safe(Q1, [Q2|T], N) :-
   % Q1 and Q2 are not on the same diagonal
   Q1 - Q2 #\= N r_prop,
   Q2 - Q1 #\= N r_prop,
   N1 is N + 1 ,
   repair_safe(Q1, T, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3SAT 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sat :-
	set_sat_constraints(Vars),
	initial_assignments(Vars, 0, Init),
	wcs(Vars, Init),
	writeln(Vars).

initial_assignments([], _, []).
initial_assignments([_V|Vs], N, [N|Init]) :-
% simple alternative 1s and 0s
	(N == 0 -> N1 = 1 ; N1 = 0),
	initial_assignments(Vs, N1, Init).

set_sat_constraints(Vars) :-
	findall(NG,conflict(NG),NGL),
	create_nogoods(NGL, [], VarIds),
	(foreach(VI,VarIds), foreach(V,Vars) do VI = _-V).

create_nogoods([], VarIds, VarIds).
create_nogoods([NG|NGs], VarIds0, VarIds) :-
	add_one_nogood(NG, [], [], VarIds0, VarIds1),
	create_nogoods(NGs, VarIds1, VarIds).

add_one_nogood([], Vars, Vals, VIds, VIds) :-
	Vars ~= Vals r_prop.
add_one_nogood([(VarId,Val)|VVs], Vars0, Vals0, VIds0, VIds) :-
	(member(VarId-V, VIds0) ->
	    VIds1 = VIds0
	;   V::0..1, VIds1 =[VarId-V|VIds0]  % V is a new var here
        ),
	Vars1 = [V|Vars0],
	Vals1 = [Val|Vals0],
	add_one_nogood(VVs, Vars1, Vals1, VIds1, VIds).


	    

