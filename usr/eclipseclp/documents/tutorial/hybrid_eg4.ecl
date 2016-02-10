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

:- lib(ic).
:- lib(eplex).
:- lib(branch_and_bound).

overlap(Start,Duration,Time,Bool) :-
        % Bool is 1 if the task with start time Start and duration
        % Duration overlaps time point Time and 0 otherwise
        ic:(Bool #= (Time$>=Start and Time$=<Start+Duration-1)).

ic_constraints(Time,S1,S2,B1,B2) :-
        % exactly one of task 1 with duration 3 and task 2 with
        % duration 5 overlaps time point Time
        ic: ([S1,S2]::1..20),
        overlap(S1,3,Time,B1),
        overlap(S2,5,Time,B2),
        ic: (B1+B2 #= 1).

before(Start,Duration,Time) :-
        % the task with start time Start and duration Duration is
        % completed before time point Time
        eplex:(Start+Duration $=< Time).

pos_overlap(Start,Duration,Time,Bool) :-
        % if Bool is 1 then the task with start time Start and
        % duration Duration overlaps time point Time
        Max1 is maxdiff(Start,Time),
        Max2 is maxdiff(Time,Start+Duration-1),
        eplex:(Time+(1-Bool)*Max1 $>= Start),
        eplex:(Time $=< Start+Duration-1+(1-Bool)*Max2).

neg_overlap(Start,Duration,Time,Bool1,Bool2) :-
        % if Bool1 is 1 then the task with start time Start and duration
        % Duration starts after time point Time
        Max1 is maxdiff(Time,Start-1),
        eplex:(Time $=< Start-1+(1-Bool1)*Max1),
        % if Bool2 is 1 then the task with start time Start and duration
        % Duration is completed before time point Time
        Max2 is maxdiff(Start+Duration,Time),
        eplex:(Time+(1-Bool2)*Max2 $>= Start+Duration).

maxdiff(Expr1,Expr2,MaxDiff) :-
        % the maximum diffrence between Expr1 and Expr2 is the max val
        % of (Expr1 - Expr2)
        MaxDiff is max_val(Expr1 - Expr2).

max_val(Expr, Max) :-
        % the maximum value of a variable is its upper bound
        var(Expr),!,
        get_var_bounds(Expr, _, Max).
max_val(Expr, Max) :-
        % the maximum value of a number is itself
        number(Expr),!,
        Max = Expr.
max_val(Expr1 + Expr2, Max) :-
        % the maximum value of (Exrp1 + Expr2) is the maximum value of
        % Expr1 plus the maximum value of Expr2
        Max is max_val(Expr1) + max_val(Expr2).
max_val(Expr1 - Expr2, Max) :-
        % the maximum value of (Exrp1 - Expr2) is the maximum value of
        % Expr1 minus the minimum value of Expr2
        Max is max_val(Expr1) - min_val(Expr2).

min_val(Expr, Min) :-
        % the minimum value of a variable is its lower bound
        var(Expr),!,
        get_var_bounds(Expr, Min, _).
min_val(Expr, Min) :-
        % the minimum value of a number is itself
        number(Expr),!,
        Min = Expr.
min_val(Expr1 + Expr2, Max) :-
        % the minimum value of (Exrp1 + Expr2) is the minimum value of
        % Expr1 plus the minimum value of Expr2
        Max is min_val(Expr1) + min_val(Expr2).
min_val(Expr1 - Expr2, Max) :-
        % the minimum value of (Exrp1 - Expr2) is the minimum value of
        % Expr1 minus the maximum value of Expr2
        Max is min_val(Expr1) - max_val(Expr2).

eplex_constraints_3(T,S1,S2,S3,B1,N1B1,N2B1,B2,N1B2,N2B2) :-
        % task 1 with duration 3 and task 2 with duration 5 are both
        % completed before the start time of task 3
        before(S1,3,S3),
        before(S2,5,S3),
        % task 1 with duration 3 either overlaps time point Time,
        % starts after it or is completed before it
        pos_overlap(S1,3,T,B1),
        neg_overlap(S1,3,T,N1B1,N2B1),
        eplex:(N1B1+N2B1 $= 1-B1),
        % task 2 with duration 5 either overlaps time point Time,
        % starts after it or is completed before it
        pos_overlap(S2,5,T,B2),
        neg_overlap(S2,5,T,N1B2,N2B2),
        eplex:(N1B2+N2B2 $= 1-B2),
        % exactly one of task 1 with duration 3 and task 2 with
        % duration 5 overlaps time point Time
        eplex:(B1+B2 $= 1).

hybrid4(Time, [S1,S2,S3], End) :-
        % give the eplex cost variable some default bounds
        ic:(End $:: -1.0Inf..1.0Inf),
        % we must give the start time of task 3 and the non-overlap
        % booleans ic bounds in order to suspend on changes to them
        ic:(S3::1..20),
        ic:([N1B1,N2B1,N1B2,N2B2]::0..1),
        % setup the problem constraints
        ic_constraints(Time,S1,S2,B1,B2),
        eplex_constraints_3(Time,S1,S2,S3,B1,N1B1,N2B1,B2,N1B2,N2B2),
        % perform the optimisation
        both_opt(labeling([B1,N1B1,N2B1,B2,N1B2,N2B2,S1,S2]),min(S3),End).

both_opt(Search,Obj,Cost) :-
        % setup the eplex solver
        eplex:eplex_solver_setup(Obj,Cost,[sync_bounds(yes)],[ic:min,ic:max]),
        % minimize Cost by branch-and-bound
        minimize((Search,eplex_get(cost,Cost)),Cost).
