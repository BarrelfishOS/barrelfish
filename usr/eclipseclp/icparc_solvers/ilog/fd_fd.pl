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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: fd_fd.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $
% ----------------------------------------------------------------------

% $Id: fd_fd.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $

% Compatibility with the fd library

:- module_interface(fd_fd).

:- use_module(fd_ilog).

:- define_macro(constraints_number/2, tr_fd_fd/2, [goal]).
:- define_macro(fd_eval/1, tr_fd_fd/2, [goal]).
:- define_macro(is_integer_domain/1, tr_fd_fd/2, [goal]).
:- define_macro(deleteffc3, tr_fd_fd/2, [goal]).
:- define_macro(dvar_range/3, tr_fd_fd/2, [goal]).
:- define_macro(dvar_size/3, tr_fd_fd/2, [goal]).
:- define_macro(maxdomain/2, tr_fd_fd/2, [goal]).
:- define_macro(mindomain/2, tr_fd_fd/2, [goal]).
:- define_macro(dvar_domain_list/2, tr_fd_fd/2, [goal]).
:- define_macro(dvar_remove_element/2, tr_fd_fd/2, [goal]).
:- define_macro(dvar_remove_smaller/2, tr_fd_fd/2, [goal]).
:- define_macro(dvar_remove_greater/2, tr_fd_fd/2, [goal]).
:- define_macro(dvar_update/2, tr_fd_fd/2, [goal]).
:- define_macro(dom/2, tr_fd_fd/2, [goal]).

:- export tr_fd_fd/2.




:- begin_module(fd_fd).

tr_fd_fd(constraints_number(_Var, N), (N = 0)) :-
  printf("WARNING: constraints_number/2 not implemented, always returns 0\n", []).
tr_fd_fd(fd_eval(Goal), Goal) :-
  printf("WARNING: fd_eval/1 translated into call/1\n", []).
tr_fd_fd(is_integer_domain(X), is_domain(X)).
tr_fd_fd(deleteffc(A, B, C), deleteff(A, B, C)) :-
  printf("WARNING: deleteffc/3 not implemented, replaced by deleteff/3\n", []).
tr_fd_fd(dvar_range(X, Min, Max), (dvar_domain(X, D), dom_range(D, Min, Max))).
tr_fd_fd(dvar_size(X, Size), (dvar_domain(X, D), dom_size(D, Size))).
tr_fd_fd(maxdomain(Var, Max), (dvar_domain(Var, D), dom_range(D, _Min, Max))).
tr_fd_fd(mindomain(Var, Min), (dvar_domain(Var, D), dom_range(D, Min, _Max))).
tr_fd_fd(dvar_domain_list(V, L), dom(V, L)).
tr_fd_fd(dvar_remove_element(V, E), V ## E).
tr_fd_fd(dvar_remove_smaller(V, E), V #>= E).
tr_fd_fd(dvar_remove_greater(V, E), V #<= E).
tr_fd_fd(dvar_update(V, Dom), (var_fd(Dummy, Dom), V = Dummy)).
tr_fd_fd(dom(X, L), (dvar_domain(X, D), findall(V, dom_member(V, D), L))).

