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
:- lib(toplevel).

gen_errors :-

    toplevel_init(tty),	% to get the toplevel-related handlers installed

    open('err0.tex', write, errtex),
    p(1, 10),
    close(errtex),
    open('err1.tex', write, errtex),
    p(12, 39),
    close(errtex),
    open('err2.tex', write, errtex),
    p(40, 79),
    close(errtex),
    open('err3.tex', write, errtex),
    p(80, 109),
    close(errtex),
    open('err4.tex', write, errtex),
    p(110, 129),
    close(errtex),
    open('err5.tex', write, errtex),
    p(130, 149),
    close(errtex),
    open('err6.tex', write, errtex),
    p(150, 159),
    close(errtex),
    open('err7.tex', write, errtex),
    p(160, 169),
    close(errtex),
    open('err8.tex', write, errtex),
    p(170, 229),
    close(errtex),
    open('err9.tex', write, errtex),
    p(230, 269),
    close(errtex),
    open('err10.tex', write, errtex),
    p(270, 299),
    close(errtex).

p(From, To) :-
        writeln(errtex, "\
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the \"License\"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an \"AS IS\"
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
"),
    current_error(E),
    E >= From,
    E =< To,
    error_id(E,M),
    get_error_handler(E,H,_),
    H \= boot_error/2,
    printf(errtex, "%d & %s & %w \\\\\n",[E,M,H]),
    fail.
p(_, _).

