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

:- eplex_instance(prob).  

main4(Cost, Vars) :-
        Vars = [A1,A2,A3,B1,B2,B3,C1,C2,C3,D1,D2,D3], 
        prob: (Vars $:: 0.0..1.0Inf),
        prob: integers(Vars),  % h. impose the integrality constraint
        prob: (A1 + A2 + A3 $= 21),
        prob: (B1 + B2 + B3 $= 40),
        prob: (C1 + C2 + C3 $= 34),
        prob: (D1 + D2 + D3 $= 10),

        prob: (A1 + B1 + C1 + D1 $=< 50),
        prob: (A2 + B2 + C2 + D2 $=< 30),
        prob: (A3 + B3 + C3 + D3 $=< 40),

        prob: eplex_solver_setup(min(
                10*A1 + 7*A2 + 200*A3 + 
                 8*B1 + 5*B2 + 10*B3 +
                 5*C1 + 5*C2 +  8*C3 + 
                 9*D1 + 3*D2 +  7*D3)),

        prob: (A1 $= A2), % g. the new constraint, added after setup

        %------------------------------- End of Modelling code

        prob: eplex_solve(Cost),  
        (foreach(V, Vars) do
            prob: eplex_var_get(V, typed_solution, V) 
        ).
