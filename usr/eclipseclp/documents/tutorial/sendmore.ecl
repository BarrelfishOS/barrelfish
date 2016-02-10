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
%

:- lib(ic).

sendmore(Digits) :-
        Digits = [S,E,N,D,M,O,R,Y],

        % Assign a finite domain with each letter - S, E, N, D, M, O, R, Y -
        % in the list Digits
        Digits :: [0..9],

        % Constraints
        alldifferent(Digits),
        S #\= 0,
        M #\= 0,
                     1000*S + 100*E + 10*N + D
                   + 1000*M + 100*O + 10*R + E
        #= 10000*M + 1000*O + 100*N + 10*E + Y,

        % Search
        labeling(Digits).

