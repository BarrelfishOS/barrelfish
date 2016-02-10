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

:- lib(eplex).

:- local variable(best_cost).  % cost bound in branch-and-bound
:- local variable(best_solution). % storage for best solution

:- eplex_instance(mip).


main5(Cost, Vars) :-
        % b. create the problem variables and set their range
        Vars = [A1,A2,A3,B1,B2,B3,C1,C2,C3,D1,D2,D3], 
        mip: (Vars :: 0.0..1.0Inf),

        % c. post the constraints for the problem to the eplex instance
        mip: (A1 + A2 + A3 $= 21),
        mip: (B1 + B2 + B3 $= 40),
        mip: (C1 + C2 + C3 $= 34),
        mip: (D1 + D2 + D3 $= 10),

        mip: (A1 + B1 + C1 + D1 $=< 50),
        mip: (A2 + B2 + C2 + D2 $=< 30),
        mip: (A3 + B3 + C3 + D3 $=< 40),
        mip: (A1 $= A2),

        setval(best_cost, 1.0Inf),  % no best cost bound yet
        mip: eplex_solver_setup(min(
                   10*A1 + 7*A2 + 200*A3 + 
                   8*B1 + 5*B2 + 10*B3 +
                   5*C1 + 5*C2 +  8*C3 + 
                   9*D1 + 3*D2 +  7*D3)),
        mip: eplex_solve(RelaxedCost),   % get an initial solution

        % do-it-yourself branch-and-bound,
        % **SHOULD USE lib(branch_and_bound) INSTEAD**
        ( branching(Vars, Vars, RelaxedCost) 
        ; 
          getval(best_cost, Cost),
          Cost < 1.0Inf, % check that there is a solution
          getval(best_solution, Vars) % retrieve the best solution values
        ).

branching(IntVars, Vars, LastCost) :-
        % Find a variable X which does not have an integer solution value
        (integer_violation(IntVars, X, XVal) ->
            % for each integer variable X with solution value XVal
            % which violates the integer constraint
            Split is floor(XVal),
            % choice: branch on the two ranges for X
            (mip: (X $=< Split) ; mip: (X $>= Split + 1)),
            mip: eplex_solve(RelaxedCost),

            % check againt the current best cost bound 
            getval(best_cost, BestCost),
            RelaxedCost < BestCost, 
            branching(IntVars, Vars, RelaxedCost)
        ;
            % cannot find any integer violations; found a solution
            getval(best_cost, BestCost),
            LastCost < BestCost, 

            % found a better solution, remember it
            setval(best_cost, LastCost),
            (foreach(V, Vars) do mip: eplex_var_get(V,solution,V)),
            setval(best_solution, Vars), 
            fail    % backtrack to search for more solutions
        ).

% returns Var with solution value Val which violates the integer constraint
integer_violation([X|Xs], Var, Val) :-
        mip: eplex_var_get(X, solution, RelaxedSol),
        % m. we are dealing with floats here, so need some `margin' for a
        %    float value to be considered integer (1e-5 on either side)
        (abs( RelaxedSol - round(RelaxedSol) ) >= 1e-5 ->
            Var = X, Val = RelaxedSol
        ;
            integer_violation(Xs, Var, Val)
        ).

