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

rc_prune_all(Vars,min,Opt) :-
        % get the current minimum value
        eplex_get(cost,Curr),
        (
            foreach(Var,Vars),
            param(Curr,Opt)
        do
            % apply reduced cost pruning to each variable
            rc_prune(Var,min,Curr,Opt)
        ).

rc_prune(Num,_,_,_) :- nonvar(Num), !.
rc_prune(Var,min,Curr,Opt) :-
        eplex_var_get(Var,reduced_cost,RC),
        ( RC =:= 0.0 ->
            true
        ;
            % if the variable is still uninstantiated and has a
            % non-zero reduced cost restrict its domain
            eplex_var_get(Var,solution,Val),
            ic: ((Var-Val)*RC+Curr $=< Opt)   % cons5
        ).

test(X,Y,Z,Opt) :-
        % set up variable bounds
        ic: ([X,Y,Z]::1..10),
        ic: (Opt:: -1.0Inf..1.0Inf),
        % setup constraints
        eplex: (5*X+2*Y+  Z $>= 10),
        eplex: (3*X+4*Y+5*Z $>= 12),
        % setup the linear solver with reduced cost recording enabled
        % and a post goal to perform reduced cost pruning
        eplex:eplex_solver_setup(
                                 min(X+Y+Z),
                                 Opt,
                                 [sync_bounds(yes),reduced_cost(yes)],
                                 [new_constraint,inst,
                                  post(rc_prune_all([X,Y,Z],min,Opt))]
                                ),
        % label the variables
        labeling([X,Y,Z]).
