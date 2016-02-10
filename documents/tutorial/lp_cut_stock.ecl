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

% eplex instance creation
:- eplex_instance(cut_stock).

lp_cut_stock(Lengths, Demands, StockLength, Vars, Cost) :-
        (
            foreach(Li, Lengths),
            foreach(Bi, Demands),
            foreach([], XijVars0),
            foreach(Maxi, Bounds),
            fromto(0, KIn, KOut, K0),
            param(StockLength)
        do
            KOut is KIn + fix(ceiling(Bi/floor(StockLength/Li))),
            Maxi is fix(floor(StockLength/Li))
        ),
        (
            for(J, 1, K0),
            foreach(Wj, Obj),
            foreach(Xj:Used, Vars),
            fromto(XijVars0, VIn, VOut, XijVars),
            param(Lengths, StockLength, Bounds)
        do
            cut_stock:integers([Xj,Wj]),
            % Xj variable bounds
            cut_stock:(Xj::0..1),
            % Wj variable bounds
            cut_stock:(Wj::0..StockLength),
            (
                foreach(Li, Lengths),
                foreach(Xij, Used),
                foreach(Li*Xij, Knapsack),
                foreach(XiVars, VIn),
                foreach([Xij|XiVars], VOut),
                foreach(Maxi, Bounds),
                param(Xj)
            do
                % Xij variable bounds
                cut_stock:integers(Xij),
                cut_stock:(Xij::0..Maxi)
            ),
            % cutting knapsack constraint
            cut_stock:(sum(Knapsack) + Wj =:= StockLength*Xj)
        ),
        (
            foreach(Bi, Demands),
            foreach(Xijs, XijVars)
        do
            % demand constraint
            cut_stock:(sum(Xijs) >= Bi)
        ),
        cut_stock:eplex_solver_setup(min(sum(Obj))),
        % optimization call
        cut_stock:eplex_solve(Cost).
