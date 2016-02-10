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
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: knapsack_ls.ecl,v 1.1 2006/09/23 01:53:49 snovello Exp $
%
% How to do local search with ECLiPSe and its repair-library.
% Applied to the knapsack problem.
%
% Author:	Joachim Schimpf, IC-Parc
%----------------------------------------------------------------------

:- lib(fd).
:- lib(repair).

knap(Method) :-
	open("knap.log", write, log),
	writeln(log, method:Method),
	cputime(T0),
	(
	    between(1, 100, 1, Instance),
		once knap(Method, Instance, Cost),
		Profit is -Cost,
		writeln(log, Instance:Profit),
	    fail
	;
	    T is cputime-T0,
	    writeln(log,time is T),
	    close(log)
	).

% knap(+Method, +ProblemInstance, -Cost)
knap(rw, Instance, Cost) :- knap_rw(Instance, 1000, Cost).
knap(hc, Instance, Cost) :- knap_hc(Instance, 10, 100, Cost).
knap(sa, Instance, Cost) :- knap_sa(Instance, 1000, Cost).
knap(ta, Instance, Cost) :- knap_ta(Instance, 1000, Cost).
knap(gd, Instance, Cost) :- knap_gd(Instance, 0.2, 1000, Cost).
knap(tb, Instance, Cost) :- knap_tb(Instance, 5, 1000, Cost).


%----------------------------------------------------------------------
% Search code template
%----------------------------------------------------------------------
%
% local_search(PARAMETERS, VarArr, Profit, Opt) :-
%	STARTING_SOLUTION(VarArr),		% starting solution
%	(
%	    GLOBAL_CONDITION
%	    fromto(0, Opt1, Opt4, Opt)
%	do
%	    (
%	        LOCAL_CONDITION
%		fromto(Opt1, Opt2, Opt3, Opt4)
%	    do
%		Profit tent_get PrevProfit,
%		(
%		    MOVE(VarArr),		% try a move
%		    IS_A_SOLUTION,
%		    Profit tent_get CurrentProfit,
%		    ACCEPTABLE(PrevProfit, CurrentProfit)
%		->
%		    ( CurrentProfit > Opt2 ->	% is it new optimum?
%			STORE_OPTIMUM
%			Opt3=CurrentProfit	% accept and remember
%		    ;
%			Opt3=Opt2		% accept
%		    )
%		;
%		    Opt3=Opt2			% reject
%		)
%	    ),
%	    RESTART_SOLUTION(VarArr)		% restart
%	).
%
%----------------------------------------------------------------------

%----------------------------------------------------------------------
% Problem setup
%----------------------------------------------------------------------

knapsack_setup(Id, VarArr, Profit, Profits, Weights) :-
	knapsack_data(Id, N, Profits, Weights, Capacity),

	length(Vars, N),				% variables
	Vars :: 0..1,

	Capacity #>= Weights*Vars   r_conflict cap,	% constraint

	Profit tent_is Profits*Vars,			% objective

	% Make a variable array with most promising ones first.
	% This order is only used in some of the search strategies.
	(
	    foreach(X, Vars), foreach(P, Profits), foreach(W, Weights),
	    foreach(ProfitPerWeight-X,PPWs)
	do
	    ProfitPerWeight is P/W
	),
	sort(1, >=, PPWs, SortedPPWs),
	( foreach(_-X,SortedPPWs), foreach(X,SortedVars) do true ),
	VarArr =.. [[]|SortedVars].


%----------------------------------------------------------------------
% Random walk (well, almost random)
%----------------------------------------------------------------------

knap_rw(Id, MaxIter, Opt) :-
	knapsack_setup(Id, VarArr, Profit, _, _),
	random_walk(MaxIter, VarArr, Profit, Opt).


random_walk(MaxIter, VarArr, Profit, Opt) :-
	init_tent_values(VarArr, random),
	(
	    for(_,1,MaxIter),
	    fromto(0, Opt2, Opt3, Opt),
	    param(Profit,VarArr)
	do
	    ( conflict_constraints(cap,[]) ->	% it's a solution
		Profit tent_get CurrentProfit,
		(
		    CurrentProfit > Opt2	% new optimum?
		->
		    printf("Found solution with profit %w%n", [CurrentProfit]),
		    Opt3=CurrentProfit
		;
		    Opt3=Opt2
		),
		change_random(VarArr, 0, 1)
	    ;
		Opt3=Opt2,
		change_random(VarArr, 1, 0)
	    )
	).

%----------------------------------------------------------------------
% Hill climbing
%----------------------------------------------------------------------

knap_hc(Id, MaxTries, MaxIter, Opt) :-
	knapsack_setup(Id, VarArr, Profit, _, _),
	hill_climb(MaxTries, MaxIter, VarArr, Profit, Opt).


hill_climb(MaxTries, MaxIter, VarArr, Profit, Opt) :-
	init_tent_values(VarArr, 0),		% starting solution
	(
	    for(I,1,MaxTries),
	    fromto(0, Opt1, Opt4, Opt),
	    param(MaxIter,Profit,VarArr)
	do
	    (
		for(_,1,MaxIter),
		fromto(Opt1, Opt2, Opt3, Opt4),
		param(I,VarArr,Profit)
	    do
		Profit tent_get PrevProfit,
		(
		    flip_random(VarArr),	% try a move
		    Profit tent_get CurrentProfit,
		    CurrentProfit > PrevProfit,	% is it uphill?
		    conflict_constraints(cap,[])	% is it a solution?
		->
		    ( CurrentProfit > Opt2 ->	% is it new optimum?
			printf("Found solution with profit %w%n",
				    [CurrentProfit]),
			Opt3=CurrentProfit	% accept and remember
		    ;
			Opt3=Opt2		% accept
		    )
		;
		    Opt3=Opt2			% reject
		)
	    ),
	    init_tent_values(VarArr, 0)		% restart
	).



%----------------------------------------------------------------------
% Simulated Annealing
%----------------------------------------------------------------------

knap_sa(Id, MaxIter, Opt) :-
	knapsack_setup(Id, VarArr, Profit, ProfitPerItem, _),
	list_min_max(ProfitPerItem, MinPPI, MaxPPI),
	sim_anneal(MaxPPI, MinPPI, MaxIter, VarArr, Profit, Opt).


sim_anneal(Tinit, Tend, MaxIter, VarArr, Profit, Opt) :-
	starting_solution(VarArr),		% starting solution
	(
	    fromto(Tinit, T, Tnext, Tend),
	    fromto(0, Opt1, Opt4, Opt),
	    param(MaxIter,Profit,VarArr,Tend)
	do
	    printf("Temperature is %d%n", [T]),
	    (
		fromto(MaxIter, J0, J1, 0),
		fromto(Opt1, Opt2, Opt3, Opt4),
		param(VarArr,Profit,T)
	    do
		Profit tent_get PrevProfit,
		(
		    flip_random(VarArr),	% try a move
		    Profit tent_get CurrentProfit,
		    exp((CurrentProfit-PrevProfit)/T) > frandom,
		    conflict_constraints(cap,[])	% is it a solution?
		->
		    ( CurrentProfit > Opt2 ->	% is it new optimum?
			printf("Found solution with profit %w%n", [CurrentProfit]),
			Opt3=CurrentProfit,	% accept and remember
			J1=J0
		    ; CurrentProfit > PrevProfit ->
			Opt3=Opt2, J1=J0	% accept
		    ;
			Opt3=Opt2, J1 is J0-1	% accept
		    )
		;
		    Opt3=Opt2, J1 is J0-1	% reject
		)
	    ),
	    Tnext is max(fix(0.8*T),Tend)
	).


%----------------------------------------------------------------------
% Threshold Accepting Algorithm
%----------------------------------------------------------------------

knap_ta(Id, MaxIter, Opt) :-
	knapsack_setup(Id, VarArr, Profit, ProfitPerItem, _),
	list_min_max(ProfitPerItem, MinPPI, MaxPPI),
	thresh_acc(MaxPPI, MinPPI, MaxIter, VarArr, Profit, Opt).


thresh_acc(Tinit, Tend, MaxIter, VarArr, Profit, Opt) :-
	starting_solution(VarArr),		% starting solution
	(
	    fromto(Tinit, T, Tnext, Tend),
	    fromto(0, Opt1, Opt4, Opt),
	    param(MaxIter,Profit,VarArr,Tend)
	do
	    printf("Temperature is %d%n", [T]),
	    (
		fromto(MaxIter, J0, J1, 0),
		fromto(Opt1, Opt2, Opt3, Opt4),
		param(VarArr,Profit,T)
	    do
		Profit tent_get PrevProfit,
		(
		    flip_random(VarArr),	% try a move
		    Profit tent_get CurrentProfit,
		    CurrentProfit > PrevProfit-T,
		    conflict_constraints(cap,[])	% is it a solution?
		->
		    ( CurrentProfit > Opt2 ->	% is it new optimum?
			printf("Found solution with profit %w%n", [CurrentProfit]),
			Opt3=CurrentProfit,	% accept and remember
			J1=J0
		    ; CurrentProfit > PrevProfit ->
			Opt3=Opt2, J1=J0	% accept
		    ;
			Opt3=Opt2, J1 is J0-1	% accept
		    )
		;
		    Opt3=Opt2, J1 is J0-1	% reject
		)
	    ),
	    Tnext is max(fix(0.8*T),Tend)
	).


%----------------------------------------------------------------------
% Great Deluge Algorithm
%----------------------------------------------------------------------

knap_gd(Id, Rain, MaxIter, Opt) :-
	knapsack_setup(Id, VarArr, Profit, _, _),
	great_deluge(Rain, MaxIter, VarArr, Profit, Opt).


great_deluge(Rain, MaxIter, VarArr, Profit, Opt) :-
	starting_solution(VarArr),		% starting solution
	(
	    fromto(0, I0, I1, MaxIter),
	    fromto(0, WaterLevel, NewWaterLevel, _),
	    fromto(0, Opt2, Opt3, Opt),
	    param(VarArr,Profit,Rain)
	do
%	    writeln(waterLevel is WaterLevel),
	    (
		flip_random(VarArr),	% try a move
		Profit tent_get CurrentProfit,
		CurrentProfit > WaterLevel,
		conflict_constraints(cap,[])	% is it a solution?
	    ->
		( CurrentProfit > Opt2 ->	% is it new optimum?
		    I1=0,
		    printf("Found solution with profit %w%n", [CurrentProfit]),
		    Opt3=CurrentProfit		% accept and remember
		;
		    I1 is I0+1,
		    Opt3=Opt2			% accept
		)
	    ;
		I1 is I0+1,
		Opt3=Opt2			% reject
	    ),
	    NewWaterLevel is WaterLevel+Rain
	).


%----------------------------------------------------------------------
% Tabu search
%----------------------------------------------------------------------

knap_tb(Id, TabuSize, MaxIter, Opt) :-
	knapsack_setup(Id, VarArr, Profit, _, _),
	tabu_search(TabuSize, MaxIter, VarArr, Profit, Opt).


tabu_search(TabuSize, MaxIter, VarArr, Profit, Opt) :-
	starting_solution(VarArr),		% starting solution
	tabu_init(TabuSize, none, Tabu0),
	(
	    fromto(MaxIter, I0, I1, 0),
	    fromto(Tabu0, Tabu1, Tabu2, _),
	    fromto(0, Opt2, Opt3, Opt),
	    param(VarArr,Profit)
	do
	    (
		try_set_best(VarArr, MoveId),	% try uphill move
		conflict_constraints(cap,[]),	% is it a solution?
		tabu_add(MoveId, Tabu1, Tabu2)	% is it allowed?
	    ->
%		writeln(pack(MoveId)),
		Profit tent_get CurrentProfit,
		( CurrentProfit > Opt2 ->	% is it new optimum?
		    printf("Found solution with profit %w%n", [CurrentProfit]),
		    Opt3=CurrentProfit		% accept and remember
		;
		    Opt3=Opt2			% accept
		),
		I1 is I0-1
	    ;
		(
		    try_clear_worst(VarArr, MoveId),	% try downhill move
		    tabu_add(MoveId, Tabu1, Tabu2)	% is it allowed?
		->
%		    writeln(unpack(MoveId)),
		    I1 is I0-1,
		    Opt3=Opt2			% reject
		;
		    writeln(stop),
		    I1=0,			% no moves possible, stop
		    Opt3=Opt2			% reject
		)
	    )
	).


%----------------------------------------------------------------------
% Tabu queue
%----------------------------------------------------------------------

tabu_init(Size, Init, Head-Tail) :-
	( for(_,1,Size), fromto(Head, [Init|T], T, Tail), param(Init) do
	    true
	).

tabu_add(X, Head-Tail, NewHead-NewTail) :-
	( fromto(Head, [Y|Ys0], Ys, []), param(X,Tail) do
	    X \== Y,			% fail if X already in queue
	    ( Ys0==Tail -> Ys=[] ; Ys=Ys0 )
	),
	Head = [_|NewHead],		% otherwise add it
	Tail = [X|NewTail].


%----------------------------------------------------------------------
% Move operators
%----------------------------------------------------------------------

change_random(VarArr, Old, New) :-
	functor(VarArr, _, N),
	Start is fix(frandom*N),
	flip_first(VarArr, Start, N, Old, New).

    flip_first(VarArr, Start, N, Old, New) :-
	Start < 2*N,
	X is VarArr[Start mod N + 1],
	( X tent_get Old ->
	    X tent_set New
	;
	    Next is Start+1,
	    flip_first(VarArr, Next, N, Old, New)
	).

flip_random(VarArr) :-
	functor(VarArr, _, N),
	X is VarArr[fix(frandom*N) + 1],
	X tent_get Old,
	New is xor(Old,1),
	X tent_set New.

try_set_best(VarArr, I) :-
	functor(VarArr, _, N),
	between(1, N, 1, I),
	arg(I, VarArr, X),
	X tent_get 0,
	X tent_set 1.

try_clear_worst(VarArr, I) :-
	functor(VarArr, _, N),
	between(N, 1, -1, I),
	arg(I, VarArr, X),
	X tent_get 1,
	X tent_set 0.



%----------------------------------------------------------------------
% Starting solutions
%----------------------------------------------------------------------

starting_solution(VarArr) :-
	init_tent_values(VarArr, mindomain).


starting_solution1(VarArr) :-
	init_tent_values(VarArr, random),
	repair_assignment(VarArr).

    repair_assignment(VarArr) :-
    	( conflict_constraints(cap,[]) ->
	    true
	;
	    change_random(VarArr, 0, 1),
	    repair_assignment(VarArr)
	).


list_min_max([X1|Xs], MinX, MaxX) :-
	( foreach(X,Xs), fromto(X1,Min0,Min1,MinX), fromto(X1,Max0,Max1,MaxX) do
	    ( X < Min0 -> Min1=X ; Min1=Min0 ),
	    ( X > Max0 -> Max1=X ; Max1=Max0 )
	).


init_tent_values(Vars, N) :- integer(N), !,
	( foreacharg(X,Vars),param(N) do X tent_set N ).
init_tent_values(Vars, mindomain) ?- !,
	( foreacharg(X,Vars) do mindomain(X,T), X tent_set T ).
init_tent_values(Vars, maxdomain) ?- !,
	( foreacharg(X,Vars) do maxdomain(X,T), X tent_set T ).
init_tent_values(Vars, random) ?- !,
	( foreacharg(X,Vars) do
	    dvar_range(X, Min, Max),
	    T is Min + fix(frandom * (Max-Min+1)),
	    X tent_set T
	).
init_tent_values(Vars, Method) ?-
	error(6, init_tent_values(Vars, Method)).


%----------------------------------------------------------------------
% Iteration limits
%----------------------------------------------------------------------

/*

The simplest iteration limit is a fixed number of iterations:

	( for(_, 1, MaxIter) do
		...
	)

If we want to count only certain iterations but ignore others:

	( fromto(0, I0, I1, MaxIter) do
		...
		( condition -> I1 is I0+1 ; I1=I0 )
	)

And this is how to write a loop that runs for a given time LoopTime:

	Tend is cputime + LoopTime,
	( fromto(cont, _, Cont, stop), param(Tend) do
		...
		( cputime > Tend -> Cont=stop ; Cont=cont )
	),

*/

%----------------------------------------------------------------------
% Generating knapsack data
%----------------------------------------------------------------------

/*

% This routine executes and reads the output of the knapsack problem
% generator by David Pisinger (http://www.diku.dk/~pisinger/codes.html)

knapsack_data(I, N, Profits, Weights, Cap) :-
	exec([gen2,20,100,2,I,100], [null,Out]),
%	exec([gen2,100,100,2,I,100], [null,Out]),
	 %    gen2  n   r type i  S
	 %
	 % where n: number of items, 
	 %       r: range of coefficients, 
	 %       type: 1=uncorrelated, 2=weakly corr, 3=strongly corr, 
	 %             4=inverse str.corr, 5=almost str.corr, 6=subset-sum, 
	 %             7=even-odd subset-sum, 8=even-odd knapsack, 
	 %             9=uncorrelated, similar weights,
	 %             11=Avis subset-sum, 12=Avis knapsack, 13=collapsing KP,
	 %             14=bounded strongly corr, 15=No small weights
	 %       i: instance no
	 %       S: number of tests in series (typically 1000)
	read_string(Out, end_of_file, _, Msg),
	nl, write(Msg), flush(output),
	close(Out),

	open("test.in", read, S),
	read_token(S, N, integer),
	(
	    for(I,1,N),
	    foreach(Profit,Profits),
	    foreach(Weight,Weights),
	    param(S)
	do
	    read_token(S, I, integer),
	    read_token(S, Profit, integer),
	    read_token(S, Weight, integer)
	),
	read_token(S, Cap, integer),
	close(S).
*/

% Alternatively, some instances obtained from the generator...

:- mode knapsack_data(+,-,-,-,-).
knapsack_data(1, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(2, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(3, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(4, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(5, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(6, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(7, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(8, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(9, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 100).
knapsack_data(10, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 102).
knapsack_data(11, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 113).
knapsack_data(12, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 123).
knapsack_data(13, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 133).
knapsack_data(14, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 144).
knapsack_data(15, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 154).
knapsack_data(16, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 164).
knapsack_data(17, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 175).
knapsack_data(18, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 185).
knapsack_data(19, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 195).
knapsack_data(20, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 205).
knapsack_data(21, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 216).
knapsack_data(22, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 226).
knapsack_data(23, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 236).
knapsack_data(24, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 247).
knapsack_data(25, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 257).
knapsack_data(26, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 267).
knapsack_data(27, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 278).
knapsack_data(28, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 288).
knapsack_data(29, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 298).
knapsack_data(30, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 308).
knapsack_data(31, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 319).
knapsack_data(32, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 329).
knapsack_data(33, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 339).
knapsack_data(34, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 350).
knapsack_data(35, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 360).
knapsack_data(36, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 370).
knapsack_data(37, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 380).
knapsack_data(38, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 391).
knapsack_data(39, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 401).
knapsack_data(40, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 411).
knapsack_data(41, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 422).
knapsack_data(42, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 432).
knapsack_data(43, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 442).
knapsack_data(44, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 453).
knapsack_data(45, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 463).
knapsack_data(46, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 473).
knapsack_data(47, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 483).
knapsack_data(48, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 494).
knapsack_data(49, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 504).
knapsack_data(50, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 514).
knapsack_data(51, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 525).
knapsack_data(52, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 535).
knapsack_data(53, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 545).
knapsack_data(54, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 556).
knapsack_data(55, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 566).
knapsack_data(56, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 576).
knapsack_data(57, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 586).
knapsack_data(58, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 597).
knapsack_data(59, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 607).
knapsack_data(60, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 617).
knapsack_data(61, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 628).
knapsack_data(62, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 638).
knapsack_data(63, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 648).
knapsack_data(64, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 659).
knapsack_data(65, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 669).
knapsack_data(66, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 679).
knapsack_data(67, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 689).
knapsack_data(68, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 700).
knapsack_data(69, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 710).
knapsack_data(70, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 720).
knapsack_data(71, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 731).
knapsack_data(72, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 741).
knapsack_data(73, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 751).
knapsack_data(74, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 761).
knapsack_data(75, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 772).
knapsack_data(76, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 782).
knapsack_data(77, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 792).
knapsack_data(78, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 803).
knapsack_data(79, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 813).
knapsack_data(80, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 823).
knapsack_data(81, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 834).
knapsack_data(82, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 844).
knapsack_data(83, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 854).
knapsack_data(84, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 864).
knapsack_data(85, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 875).
knapsack_data(86, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 885).
knapsack_data(87, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 895).
knapsack_data(88, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 906).
knapsack_data(89, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 916).
knapsack_data(90, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 926).
knapsack_data(91, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 937).
knapsack_data(92, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 947).
knapsack_data(93, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 957).
knapsack_data(94, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 967).
knapsack_data(95, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 978).
knapsack_data(96, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 988).
knapsack_data(97, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 998).
knapsack_data(98, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 1009).
knapsack_data(99, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 1019).
knapsack_data(100, 20, [2, 97, 62, 100, 16, 71, 26, 92, 15, 68, 90, 35, 110, 69, 18, 47, 12, 54, 47, 43], [1, 96, 67, 90, 13, 74, 22, 86, 23, 63, 89, 25, 100, 76, 23, 56, 5, 47, 45, 39], 1029).
