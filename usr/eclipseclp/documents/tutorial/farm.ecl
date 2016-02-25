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

farm(F, A, B, C) :-
	[A, B, C] :: 0.0 .. 1.0Inf,	% The 3 sides of the lake
	triangle_area(A, B, C, 7),	% The lake area is 7

	[F, FA, FB, FC] :: 1 .. 1.0Inf,	% The square areas are integral
	square_area(A, FA),
	square_area(B, FB),
	square_area(C, FC),

	F #= FA+FB+FC,FA $>= FB, FB $>= FC. % Avoid symmetric solutions

triangle_area(A, B, C, Area) :-
	S $>= 0,
	S $= (A+B+C)/2,
	Area $= sqrt(S*(S-A)*(S-B)*(S-C)).

square_area(A, Area) :-
	Area $= sqr(A).


solve(F) :-
	farm(F, A, B, C),		% the model
	indomain(F),			% ensure that solution is minimal
	locate([A, B, C], 0.01).
