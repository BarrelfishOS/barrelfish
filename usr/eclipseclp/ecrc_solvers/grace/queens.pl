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
% Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH.
% 
% END LICENSE BLOCK

%
% The N-queens puzzle. We have to place N queens on an NxN chess boards
% so that no-one can take another one. Our model is:
%
%        Variables: N variables with domain 1..N, each variable represents
%                        the position of a queen in one column
%        Constraints:
%                1) all columns are different - IMPLICIT
%                2) all rows are different
%                3) no diagonal has more than one queen
%


:- lib(fd).
:- lib(grace).                                 % Grace

queens(N, List) :-
    grace_start(queens),                       % Grace

    % Define variables and their domains
    length(List, N),
    List :: 1..N,
    grace_matrix(List, queens),                % Grace

    % Constraints
    %2
    alldistinct(List),
    %3
    constrain_queens(List),

    % Label the variables
    grace_label.                               % Grace

% A queen is safe if it cannot be taken by
% any of its right-hand neigbours
constrain_queens([]).
constrain_queens([X|Y]) :-
   safe(X, Y, 1),
   constrain_queens(Y).

safe(_, [], _).
safe(Q1, [Q2|T], N) :-
   % Q1 and Q2 are not on the same diagonal
   Q1 - Q2 #\= N,
   Q2 - Q1 #\= N,
   N1 is N + 1 ,
   safe(Q1, T, N1).

