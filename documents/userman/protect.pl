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
% list the protected built-in predicates

:- coroutine.

protect :-
    setof(X,
	((current_predicate(X); current_built_in(X)),
	get_flag(X, protected, on)),
	    L),
    split_list(L, L1, L2),
    open('protect.tex', write, output),
    print_list(L1),
    printf("\\vspace*{\\fill}\n", []),
    printf("\\end{minipage}\n", []),
    printf("\\begin{minipage}[t]{6cm}\n", []),
    print_list(L2),
    close(output).

split_list(L, L1, L2) :-
    length(L, N),
    N2 is (N+1)//2,
    append(L1, L2, L),
    length(L1, N2),
    !.

print_list([]).
print_list([N/A|L]) :-
    once(latex_name(N, Name)),
    printf("\\bip{%a/%d}\\\\\n", [Name, A]),
    print_list(L).

latex_name(->, '$->$').
latex_name(\+, '$\\backslash+$').
latex_name(\=, '$\\backslash=$').
latex_name(\==, '$\\backslash==$').
latex_name(>, '$>$').
latex_name(>=, '$>=$').
latex_name(<, '$<$').
latex_name(=<, '$=<$').
latex_name(=\=, '$=\\backslash=$').
latex_name(~=, '$\\sim=$').
latex_name(X, X).

