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

diff2ic(X,Y) :-
        % X and Y must differ by at least 2
        ic: ((X+2 $=< Y) or (Y+2 $=< X)).

list_diff2ic(List) :-
        % each pair must differ by at least 2
        (
            fromto(List, [X|Rest], Rest, [])
        do
            (
                foreach(Y, Rest),
                param(X)
            do
                diff2ic(X,Y)
            )
        ).

ic_list(List) :-
        length(List, Length),
        Max is 2*(Length-1),
        % each element must take a value between 1 and 2*(Length-1)
        ic: (List::1..Max),
        list_diff2ic(List),
        labeling(List).

diff2eplex(X,Y,Length,B) :-
        % if B is 1 then Y is at least 2 greater than X
        eplex: (X+2+B*Length $=< Y+Length),
        % if B is 0 then X is at least 2 greater than Y
        eplex: (X+Length $>= Y+2+(1-B)*Length).

list_diff2eplex(List, Length, Bools) :-
        % each pair must differ by at least 2
        (
            fromto(List, [X|Rest], Rest, []),
            fromto(Bools, Out, In, []),
            param(Length)
        do
            (
                foreach(Y, Rest),
                fromto(Out, [B|Bs], Bs, In),
                param(X, Length)
            do
                diff2eplex(X,Y,Length,B)
            )
        ).

eplex_list(List) :-
        length(List, Length),
        Max is 2*(Length-1),
        % each element must take a value between 1 and 2*(Length-1)
        eplex: (List::1..Max),
        list_diff2eplex(List, Length, Bools),
        % enforce Bools to be 0..1 and integer
        eplex: integers(Bools),
        eplex: (Bools::0..1),
        % setup the eplex solver with a dummy objective function
        eplex:eplex_solver_setup(min(0),Cost,[],[]),
        % solve by linear solver
        eplex:eplex_solve(Cost).

hybrid_list(List) :-
        % give the eplex cost variable some default bounds
        ic:(Cost $:: -1.0Inf..1.0Inf),
        length(List, Length),
        Max is 2*(Length-1),
        % each element must take a value between 1 and 2*(Length-1)
        ic: (List::1..Max),
        list_diff2ic(List),
        list_diff2eplex(List, Length, Bools),
        % enforce Bools to be 0..1 (but not integer in eplex)
        ic: (Bools::0..1),
        % setup the eplex solver with a dummy objective function
        eplex:eplex_solver_setup(min(0),Cost,[sync_bounds(yes)],[ic:min,ic:max]),
        % minimize Cost by branch-and-bound
        minimize((labeling(Bools),eplex_get(cost,Cost)),Cost).
