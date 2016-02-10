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

% n-queens with finite domains
:- [domain]. % load in the finite domain ECH handler

:- setval(domain,number).

queen(N,L):- 
	length(L,N), 
	L::1..N,
	queen(L), chr_labeling.

queen([]).
queen([X|Xs]):- safe(X,Xs,1),queen(Xs).

safe(X,[],N).
safe(X,[H|T],N):- no_attack(X,H,N), M is N+1, safe(X,T,M).

no_attack(X,Y,N):- X ne Y, X ne Y-N, X ne Y+N, Y ne X-N, Y ne X+N.
