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

go :-
	bagof(pa(P,A), O^current_op(P,A,O), PAs),
	sort(0, >, PAs, PAs1),
	writeln("Prec  Assoc  Operators"),
	write_ops(PAs1).

write_ops([]).
write_ops([pa(P,A)|PAs]) :-
	setof(O, current_op(P,A,O), Os),
	printf("%4d  %4s   %Dw\n", [P,A,Os]),
	write_ops(PAs).

