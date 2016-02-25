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

eplex_constraints(S1,S2,S3) :-
        % task 1 with duration 3 and task 2 with duration 5 are both
        % completed before the start time of task 3
        before(S1,3,S3),
        before(S2,5,S3).

hybrid2(Time, [S1,S2,S3], End) :-
        % give the eplex cost variable some default bounds
        ic:(End $:: -1.0Inf..1.0Inf),
        % we must give the start time of task 3 ic bounds in order to
        % suspend on changes to them
        ic:(S3::1..20),
        % setup the problem constraints
        ic_constraints(Time,S1,S2,B1,B2),
        eplex_constraints(S1,S2,S3),
        % perform the optimisation
        both_opt(labeling([B1,B2,S1,S2]),min(S3),End).

both_opt(Search,Obj,Cost) :-
        % setup the eplex solver
        eplex:eplex_solver_setup(Obj,Cost,[sync_bounds(yes)],[ic:min,ic:max]),
        % minimize Cost by branch-and-bound
        minimize((Search,eplex_get(cost,Cost)),Cost).
