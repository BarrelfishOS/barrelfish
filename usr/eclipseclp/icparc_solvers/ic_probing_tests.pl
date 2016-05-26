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

:- use_module(ic_probing_for_scheduling).
:- lib(repair).
:- lib(ic).

ex1([X,Y,Z],Cost) :-
        [OldX,OldY,OldZ]=[1,5,6],
        Durations=[10,5,5],
        Resources=[1,2,1],
        MaxResource=2,
        NewStarts=[X,Y,Z],
        ic:(NewStarts::1..10),
        CostFun= abs(X-OldX) + abs(Y-OldY) + abs(Z-OldZ),
	probe_sched(NewStarts,Durations,Resources,MaxResource,CostFun),
	Cost is CostFun.
/* Expected result:
[eclipse]: ex1([X,Y,Z],Cost)
Found a solution with cost 9
probes(7)

[X,Y,Z] = [6, 1, 6]
Cost = 9
*/

ex2([X,Y,Z],[OldX,OldY,OldZ],Durations,Resources,MaxResource,Options,Cost) :-
        NewStarts=[X,Y,Z],
        ic:(NewStarts::1..10),
        Constraints=[
                     XDiff >= X-OldX, XDiff >= OldX-X,
                     YDiff >= Y-OldY, YDiff >= OldY-Y,
		     ZDiff >= Z-OldZ, ZDiff >= OldZ-Z,
		     Cost =:= XDiff+YDiff+ZDiff
                    ],
	probe_cstr_sched(NewStarts,Durations,Resources,MaxResource,
                         Constraints,Cost,Options).

/*
[eclipse]: ex2([X,Y,Z],[1,5,6],[10,5,5],[1,2,1],2,[granularity(1),priority(5)],Cost).
Found a solution with cost 9
probes(7)

X = 6
Y = 1
Z = 6
Cost = 9


[eclipse]: ex2([X,Y,Z],[1,5,6],[10,5,5],[1,2,1],2,[granularity(1),priority(2)],Cost).
Found a solution with cost 9
probes(14)

X = 6
Y = 1
Z = 6
Cost = 9

[eclipse]: ex2([X,Y,Z],[1,5,6],[10,5,5],[1,2,1],3,[granularity(1),priority(5)],Cost).
Found a solution with cost 4
probes(5)

X = 1
Y = 1
Z = 6
Cost = 4

[eclipse]: R1 :: 1..3, R2 :: 2..3, R3=1,
           ex2([X,Y,Z],[1,5,6],[10,5,5],[R1,R2,R3],3,[granularity(1),priority(5)],Cost).
Found a solution with cost 4
probes(5)

X = 1
Y = 1
Z = 6
R1 = 1
R2 = 2
R3 = 1
Cost = 4
*/

ex3(Starts,MaxResource,Cost) :-
	Starts=[S1,S2,S3,S4],
	ic:(S1::1..10),
	ic:(S2::3..10),
	ic:(S3::1..5),
	ic:(S4::3..10),
        [D1,D2,D3,D4]=[5,5,3,3],
        Durations=[D1,D2,D3,D4],
	Resources=[1,1,1,1],
	CostFun= max([S1+D1,S2+D2,S3+D3,S4+D4]),
	probe_sched(Starts,Durations,Resources,MaxResource,CostFun),
%	Cost is CostFun.
        ic:(Cost =:= eval(CostFun)).
 
/*
[eclipse]: ex3(Starts,1,Cost).

No 

[eclipse]: ex3(Starts,2,Cost).
Found a solution with cost 12
Found a solution with cost 11
Found a solution with cost 9
probes(8)

Starts = [1, 4, 1, 6]
Cost = 9
 
[eclipse]: ex3(Starts,3,Cost).
Found a solution with cost 8
probes(3)

Starts = [1, 3, 1, 4]
Cost = 8
   
*/

ex4(Reduction,Cost) :-
	run_bench(Reduction,Cost).

/*
[eclipse]: ex4(0,Cost).
Old resource: 8
New resource: 8
Found a solution with cost 0
probes(1)

Cost = 0

[eclipse]: ex4(1,Cost).
Old resource: 8
New resource: 7
Found a solution with cost 15
probes(11)

Cost = 15

[eclipse]: ex4(2,Cost).
Old resource: 8
New resource: 6
Found a solution with cost 105
probes(181)

Cost = 105

[eclipse]: ex4(3,Cost).
Old resource: 8
New resource: 5
Found a solution with cost 410
Found a solution with cost 390
Found a solution with cost 370
Found a solution with cost 230
Found a solution with cost 225
Found a solution with cost 220
probes(1065)

Cost = 220

*/


setup_options(Options) :-
	time_granularity(G),
        probe_priority(P),
	Options = [granularity(G),priority(P)].


%%% Time granularity
time_granularity( 5 ).

%%%  Priority of linear probe demon
probe_priority( 5 ).

%%%  Amount by which start and end times can be changed
max_time_change(300).

%%% Information specific to this data set

%%% Number of tasks
task_count( 20 ).
%%% The temporal horizon
time_horizon( 285, 1260).

/*
> t(1, 2, 1055, 1155, 10).
This declares an activity with a start time point of ID 1, 
and an end time point of ID 2, starting at minute 1055 
and terminating at minute 1155. The final parameter declares 
that the activity may be shrunk or grown by no more than 10 minutes.

> c(id(1) #>= id(3) + t(10)).
This declares the constraint that the time point with ID 1 must 
take place no sooner than the time point with ID 3 plus 10 minutes.

*/


t(1, 2, 1040, 1140, 10).
t(3, 4, 335, 435, 10).
t(5, 6, 315, 415, 10).
t(7, 8, 555, 655, 10).
t(9, 10, 595, 695, 10).
t(11, 12, 865, 965, 10).
t(13, 14, 860, 960, 10).
t(15, 16, 685, 785, 10).
t(17, 18, 820, 920, 10).
t(19, 20, 650, 750, 10).
t(21, 22, 510, 610, 10).
t(23, 24, 835, 935, 10).
t(25, 26, 880, 980, 10).
t(27, 28, 980, 1080, 10).
t(29, 30, 510, 610, 10).
t(31, 32, 515, 615, 10).
t(33, 34, 545, 645, 10).
t(35, 36, 540, 640, 10).
t(37, 38, 665, 765, 10).
t(39, 40, 510, 610, 10).

c(id(34) #<= id(36) + t(5)).
c(id(31) #<= id(36) - t(125)).
c(id(30) #<= id(32) - t(5)).
c(id(28) #>= id(29) + t(570)).
c(id(27) #>= id(39) + t(470)).
c(id(27) #>= id(30) + t(370)).
c(id(25) #= id(27) - t(100)).
c(id(22) #<= id(39) + t(100)).
c(id(21) #>= id(23) - t(325)).
c(id(19) #>= id(24) - t(285)).
c(id(19) #<= id(22) + t(40)).
c(id(18) #>= id(23) + t(85)).
c(id(17) #<= id(18) - t(100)).
c(id(14) #<= id(38) + t(195)).
c(id(13) #>= id(18) - t(60)).
c(id(12) #<= id(26) - t(15)).
c(id(12) #= id(25) + t(85)).
c(id(10) #= id(34) + t(50)).
c(id(9) #>= id(18) - t(325)).
c(id(5) #>= id(29) - t(195)).
c(id(3) #<= id(37) - t(330)).
c(id(3) #<= id(31) - t(180)).
c(id(3) #>= id(15) - t(350)).
c(id(2) #>= id(40) + t(530)).
c(id(1) #= id(6) + t(625)).


run_bench(Reduction,Cost) :-
	max_time_change(MaxChange),
	task_count(TaskCount),
        probe_bench(TaskCount,Reduction,MaxChange,Cost).

probe_bench(TaskCount,ResourceReduction,MaxChange,Cost) :-
        VarCount is 2*TaskCount,
        functor(VarArray,vars,VarCount),
        make_tasks(MaxChange,StartEndConstraints,VarArray,Tasks),
        make_constraints(VarArray,SimpleTemporalConstraints),
        append(StartEndConstraints,SimpleTemporalConstraints,Constraints),
        max_overlap(Tasks,OldResource),
        NewResource is OldResource-ResourceReduction,
        write('Old resource: '), writeln(OldResource),
        write('New resource: '), writeln(NewResource),
        make_cost_fun(Tasks,MaxChange,Cost,CostCons),
        append(Constraints,CostCons,AllCons),
        setup_options(Options),
        task_structure(Tasks,Starts,Durations,Resources),
        probe_cstr_sched(Starts,Durations,Resources,NewResource,AllCons,Cost,Options).

task_structure(Tasks,Starts,Durations,Resources) :-
	(  foreach(task(Start,Duration),Tasks),
           foreach(Start,Starts),
           foreach(Duration,Durations),
           foreach(Resource,Resources)
        do
	   Resource=1
        ).

max_overlap(Tasks,Resource) :-
       (   foreach(Task1,Tasks),
           foreach(Resource1,Resources),
	   param(Tasks) 
       do 
           compute_resource(Task1,Tasks,Resource1)
       ),
       Resource is max_res_list(Resources).

max_res_list(List,Max) :-
	(  foreach(Value,List),
           fromto(0,ThisMax,NextMax,Max)
        do
           (Value>ThisMax -> NextMax=Value ; NextMax=ThisMax)
        ).

compute_resource(task(S,_),Tasks,Resource) :-
	OldStart is tent_get(S),
	(   foreach(task(S2,D2), Tasks),
	    fromto(0,ThisOlap,NextOlap,Resource),
	    param(OldStart)
        do
	    ( OldStart2 is tent_get(S2),
	      OldEnd2 is tent_get(S2)+tent_get(D2),
	      ((OldStart2=<OldStart, OldEnd2>OldStart) -> 
		      NextOlap is ThisOlap+1
                ;     NextOlap is ThisOlap
              )
            )
        ).


% Include cost of moving the End !!! 
make_cost_fun(Tasks,MaxChange,Cost,Cons) :-
    (   foreach(task(S,Duration), Tasks),
        fromto([],Diffs,[SDiff,EDiff|Diffs],AllDiffs),
        fromto([],
	       Constraints,
               [ SDiff>=S-OldStart,
                 SDiff>=OldStart-S,
                 EDiff>=(S+Duration)-(OldStart+OldDuration),
                 EDiff>=(OldStart+OldDuration)-(S+Duration)
                | 
                 Constraints
               ],
               AllConstraints
              ),    
         param(MaxChange)
    do   ( ic:(SDiff::0..MaxChange), 
           ic:(EDiff::0..MaxChange), 
           S tent_get OldStart,
           Duration tent_get OldDuration
         )
     ),
     Cons = [Cost=:=sum(AllDiffs)|AllConstraints].
       
make_tasks(Shift,SEConstraints,VarArray,Tasks) :-
        Goal = t(_IdStart,_IdEnd,_OldStart,_OldEnd,_Shrink),
	findall(Goal,call(Goal),InTaskList),
        (  foreach(InTask,InTaskList),
           foreach(SECons,SEConstraints),
           foreach(Task,Tasks),
           param(Shift,VarArray)
        do make_task(InTask,Shift,SECons,VarArray,Task)
        ).

make_task(t(IdStart,IdEnd,OldStart,OldEnd,Shrink),Shift,SECons,VarArray,Task) :-
	Task = task(S,Duration),
        S tent_set OldStart,
        make_time_domain(S,OldStart,Shift),
        S is VarArray[IdStart],
        make_time_domain(E,OldEnd,Shift),
        E tent_set OldEnd,
        E is VarArray[IdEnd],
        OldDuration is OldEnd-OldStart,
        Min is OldDuration-Shrink,
        Max is OldDuration+Shrink, 
        Duration tent_set OldDuration,
        ic:(Duration::Min..Max),
        ic:(Duration#>=Min),
        ic:(E #= S+Duration),
        SECons= (E=:=S+Duration).

make_time_domain(S,OldStart,Shift) :-
	time_horizon( F, L),
        MinStart is max(OldStart-Shift,F),
        MaxStart is min(OldStart+Shift,L),
	ic:(S::MinStart..MaxStart).

make_constraints(VarArray,Constraints) :-
    findall(InCon,c(InCon),InCons),
    (  foreach(InCon,InCons), 
       foreach(OutCon,Constraints), 
       param(VarArray) 
    do extract_terms_from_expr(InCon,VarArray,OutCon)
    ).


extract_terms_from_expr(id(N),VarArray,R) :- !,
    R is VarArray[N].
extract_terms_from_expr(t(N),_,N) :- !.
extract_terms_from_expr(InCompound,VarArray,OutCompound) :-
    InCompound=..[Pred,Arg1,Arg2],
    conv_pred(Pred,NewPred), !,
    OutCompound=..[NewPred,NewArg1,NewArg2],
    extract_terms_from_expr(Arg1,VarArray,NewArg1),
    extract_terms_from_expr(Arg2,VarArray,NewArg2).

conv_pred( '+', '+' ).
conv_pred( '-', '-' ).
conv_pred( '#=', '=:=' ).
conv_pred( '#>=', '>=' ).
conv_pred( '#<=', '=<' ).
conv_pred(Other,_) :-
    write('Unexpected operator in expression: '), writeln(Other), abort.





